module Lochs.Eval (Env, EvalResult(..), exec, mkEnv) where

import Control.Monad (ap, when)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Unique (newUnique)
import GHC.Clock (getMonotonicTime)

import Lochs.AST
import Lochs.Diagnostic
import Lochs.Runtime

data EvalResult a = Ok a
                  | Break
                  | Continue
                  | Return Value
                  | Err Diagnostic

instance Functor EvalResult where
    fmap f = \case
        Ok a     -> Ok (f a)
        Break    -> Break
        Continue -> Continue
        Return v -> Return v
        Err d    -> Err d

initialEnv :: [(String, Value)]
initialEnv = [("clock", VNativeFunction FClock)]

mkEnv :: IO Env
mkEnv = do
    m <- Map.fromList <$> traverse (traverse newIORef) initialEnv
    ref <- newIORef m
    pure $ Env ref Nothing

newtype Eval a = Eval { runEval :: Env -> IO (EvalResult a) }

instance Functor Eval where
    fmap f (Eval g) = Eval $ \env -> fmap (fmap f) (g env)

instance Applicative Eval where
    pure x = Eval $ \_ -> pure (Ok x)
    (<*>) = ap

instance Monad Eval where
    Eval g >>= f = Eval $ \env ->
        g env >>= \case
            Ok a     -> runEval (f a) env
            Break    -> pure Break
            Continue -> pure Continue
            Return v -> pure (Return v)
            Err d    -> pure (Err d)

liftIO' :: IO a -> Eval a
liftIO' io = Eval $ \_ -> Ok <$> io

throwErr :: Diagnostic -> Eval a
throwErr d = Eval $ \_ -> pure (Err d)

observe :: Eval a -> Eval (EvalResult a)
observe (Eval g) = Eval $ \env -> do
    result <- g env
    pure (Ok result)

reraise :: EvalResult a -> Eval a
reraise r = Eval $ \_ -> pure r

lochsReturn :: Value -> Eval a
lochsReturn v = Eval $ \_ -> pure (Return v)

getEnv :: Eval Env
getEnv = Eval $ \env -> pure (Ok env)

withEnv :: Env -> Eval a -> Eval a
withEnv env (Eval g) = Eval $ \_ -> g env

newScope :: Eval a -> Eval a
newScope action = do
    parentEnv <- getEnv
    ref <- liftIO' $ newIORef Map.empty
    withEnv (Env ref (Just parentEnv)) action

defineVar :: String -> Value -> Eval ()
defineVar name val = do
    env <- getEnv
    valCell <- liftIO' $ val `seq` newIORef val
    liftIO' $ modifyIORef (values env) (Map.insert name valCell)

varRef :: Int -> String -> Env -> Eval (IORef Value)
varRef line name env = do
    m <- liftIO' $ readIORef (values env)
    case Map.lookup name m of
      Just v -> pure v
      Nothing -> maybe nameError (varRef line name) (parent env)
  where nameError = throwErr $ mkDiagnostic line (" at " ++ name) "No such variable"

lookupVar :: Int -> String -> Eval Value
lookupVar line name = do
    env <- getEnv
    ref <- varRef line name env
    liftIO' $ readIORef ref

assignVar :: Int -> String -> Value -> Eval ()
assignVar line name val = do
    env <- getEnv
    ref <- varRef line name env
    liftIO' $ val `seq` writeIORef ref val

exec :: Env -> [Stmt] -> IO (EvalResult ())
exec env stmts = runEval (execProgram stmts) env

execProgram :: [Stmt] -> Eval ()
execProgram []     = pure ()
execProgram (x:xs) = execStmt x >> execProgram xs

execStmt :: Stmt -> Eval ()
execStmt (PrintStmt _line expr) = do
    val <- eval expr
    liftIO' $ putStrLn (stringify val)
execStmt (ExprStmt  _line expr) = eval expr >> pure ()
execStmt (VarDecl _line name expr) = do
    val <- traverse eval expr
    defineVar name $ fromMaybe VNil val
execStmt (FunDecl _line name params body) = do
    env <- getEnv
    u <- liftIO' newUnique
    defineVar name (VLochsFunction u env name params body)
execStmt (Block _line stmts) = newScope (execProgram stmts)
execStmt (IfStmt _line cond cons alt) = do
    val <- eval cond
    if isTruthy val
       then execStmt cons
       else maybe (pure ()) execStmt alt
execStmt w@(WhileStmt _line cond body) = do
    val <- eval cond
    when (isTruthy val) $ do
        observe (execStmt body) >>= \case
            Ok _     -> execStmt w
            Continue -> execStmt w
            Break    -> pure ()
            Return v -> lochsReturn v
            Err d    -> reraise (Err d)
execStmt (BreakStmt _)    = Eval $ \_ -> pure Break
execStmt (ContinueStmt _) = Eval $ \_ -> pure Continue
execStmt (ReturnStmt _ e) = do
    val <- fromMaybe (pure VNil) $ fmap eval e
    lochsReturn val

callNative :: NativeFunctionID -> [Value] -> Eval Value
callNative FClock _ = liftIO' getMonotonicTime >>= pure . VNumber

call' :: Callable -> [Value] -> Eval Value
call' (NativeFunction _ funId) args = callNative funId args
call' (LochsFunction _ env params body) args = do
    withEnv env . newScope $ do
        traverse_ (uncurry defineVar) (zip params args)
        observe (execProgram body) >>= \case
            Ok _     -> pure VNil
            Continue -> error "Unreachable: continue crossing function boundary"
            Break    -> error "Unreachable: break crossing function boundary"
            Return v -> pure v
            Err d    -> reraise (Err d)

call :: Int -> Callable -> [Value] -> Eval Value
call line callable args
    | length args == arity callable = call' callable args
    | otherwise                     = runtimeError line message
        where message = "Expected " ++ show (arity callable)
                ++ " arguments but got " ++ show (length args)

ensureCallable :: Int -> Value -> Eval Callable
ensureCallable _ (VNativeFunction funId) =
    pure $ NativeFunction (nativeFunctionArity funId) funId
ensureCallable _ (VLochsFunction _ env _ params body) =
    pure $ LochsFunction (length params) env params body
ensureCallable l v                      = typeError l v "function"

hydrate :: LitValue -> Value
hydrate (LitBool b) = VBool b
hydrate (LitNumber n) = VNumber n
hydrate (LitString s) = VString s
hydrate LitNil = VNil

eval :: Expr -> Eval Value
eval = \case
    Literal  _line v      -> pure $ hydrate v
    Grouping _line e      -> eval e
    Unary    line  op e   -> do
        operand <- eval e
        unary line op operand
    Binary line l op r -> do
        lhs <- eval l
        rhs <- eval r
        binary line op lhs rhs
    Logical _ l op r -> do
        lhs <- eval l
        logical op lhs r
    Variable line name -> lookupVar line name
    Assign line name expr -> do
        val <- eval expr
        assignVar line name val
        pure val
    Call line callee args -> do
        callee' <- eval callee
        args' <- traverse eval args
        fn <- ensureCallable line callee'
        call line fn args'

runtimeError :: Int -> String -> Eval a
runtimeError line message = throwErr $ mkDiagnostic line "" message

typeError :: Int -> Value -> String -> Eval a
typeError line val expected = throwErr $
    mkDiagnostic line "" ("Expected " ++ expected ++ " but got " ++ typeName val)

unary :: Int -> UnaryOp -> Value -> Eval Value
unary _line UnaryNeg (VNumber n) = pure $ VNumber (-n)
unary  line UnaryNeg v           = typeError line v "number"
unary _line UnaryNot v           = pure $ VBool (not (isTruthy v))

binary :: Int -> BinaryOp -> Value -> Value -> Eval Value
binary _line BinSub (VNumber l) (VNumber r) = pure $ VNumber (l - r)
binary  line BinSub (VNumber _) r           = typeError line r "number"
binary  line BinSub l           (VNumber _) = typeError line l "number"
binary  line BinSub l           _           = typeError line l "number"

binary  line BinDiv (VNumber _) (VNumber 0) = throwErr $ mkDiagnostic line "" "Division by zero"
binary _line BinDiv (VNumber l) (VNumber r) = pure $ VNumber (l / r)
binary  line BinDiv (VNumber _) r           = typeError line r "number"
binary  line BinDiv l           (VNumber _) = typeError line l "number"
binary  line BinDiv l           _           = typeError line l "number"

binary _line BinMul (VNumber l) (VNumber r) = pure $ VNumber (l * r)
binary  line BinMul (VNumber _) r           = typeError line r "number"
binary  line BinMul l           (VNumber _) = typeError line l "number"
binary  line BinMul l           _           = typeError line l "number"

binary _line BinAdd (VNumber l) (VNumber r) = pure $ VNumber (l + r)
binary  line BinAdd (VNumber _) r           = typeError line r "number"
binary  line BinAdd l           (VNumber _) = typeError line l "number"
binary _line BinAdd (VString l) (VString r) = pure $ VString (l ++ r)
binary  line BinAdd (VString _) r           = typeError line r "string"
binary  line BinAdd l           (VString _) = typeError line l "string"
binary  line BinAdd l           _           = typeError line l "number or string"

binary _line BinGt  (VNumber l) (VNumber r) = pure $ VBool   (l > r)
binary  line BinGt  (VNumber _) r           = typeError line r "number"
binary  line BinGt  l           (VNumber _) = typeError line l "number"
binary  line BinGt  l           _           = typeError line l "number"
binary _line BinGte (VNumber l) (VNumber r) = pure $ VBool   (l >= r)
binary  line BinGte (VNumber _) r           = typeError line r "number"
binary  line BinGte l           (VNumber _) = typeError line l "number"
binary  line BinGte l           _           = typeError line l "number"
binary _line BinLt  (VNumber l) (VNumber r) = pure $ VBool   (l < r)
binary  line BinLt  (VNumber _) r           = typeError line r "number"
binary  line BinLt  l           (VNumber _) = typeError line l "number"
binary  line BinLt  l           _           = typeError line l "number"
binary _line BinLte (VNumber l) (VNumber r) = pure $ VBool   (l <= r)
binary  line BinLte (VNumber _) r           = typeError line r "number"
binary  line BinLte l           (VNumber _) = typeError line l "number"
binary  line BinLte l           _           = typeError line l "number"

binary _line BinEq  l           r           = pure $ VBool   (isEqual l r)
binary _line BinNe  l           r           = pure $ VBool   (not (isEqual l r))

logical :: LogicalOp -> Value -> Expr -> Eval Value
logical LogicalAnd val r = if isTruthy val then eval r else pure val
logical LogicalOr  val r = if isTruthy val then pure val else eval r
