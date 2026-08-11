module Lochs.Eval (EvalResult(..), GlobalEnv, exec, mkEnv) where

import Control.Monad (ap, when)
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
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

mkEnv :: IO GlobalEnv
mkEnv = do
    m <- Map.fromList <$> traverse (traverse newIORef) initialEnv
    ref <- newIORef m
    pure $ GlobalEnv ref

data EvalContext = EvalContext
    { ctxGlobalEnv :: GlobalEnv
    , ctxLocalEnv :: Maybe LocalEnv
    }

newtype Eval a = Eval { runEval :: EvalContext -> IO (EvalResult a) }

instance Functor Eval where
    fmap f (Eval g) = Eval $ \ctx -> fmap (fmap f) (g ctx)

instance Applicative Eval where
    pure x = Eval $ \_ -> pure (Ok x)
    (<*>) = ap

instance Monad Eval where
    Eval g >>= f = Eval $ \ctx ->
        g ctx >>= \case
            Ok a     -> runEval (f a) ctx
            Break    -> pure Break
            Continue -> pure Continue
            Return v -> pure (Return v)
            Err d    -> pure (Err d)

liftIO' :: IO a -> Eval a
liftIO' io = Eval $ \_ -> Ok <$> io

throwErr :: Diagnostic -> Eval a
throwErr d = Eval $ \_ -> pure (Err d)

observe :: Eval a -> Eval (EvalResult a)
observe (Eval g) = Eval $ \ctx -> do
    result <- g ctx
    pure (Ok result)

reraise :: EvalResult a -> Eval a
reraise r = Eval $ \_ -> pure r

lochsReturn :: Value -> Eval a
lochsReturn v = Eval $ \_ -> pure (Return v)

getEnv :: Eval (Maybe LocalEnv)
getEnv = Eval $ \ctx -> pure (Ok $ ctxLocalEnv ctx)

getEnvAt :: Int -> Eval (Maybe LocalEnv)
getEnvAt 0 = getEnv
getEnvAt n = do
    p <- (>>= parent) <$> getEnv
    case p of
      Nothing -> error "Name resolution error"
      Just p' -> withEnv p' $ getEnvAt (n - 1)

getGlobalEnv :: Eval GlobalEnv
getGlobalEnv = Eval $ \ctx -> pure (Ok $ ctxGlobalEnv ctx)

withEnv :: LocalEnv -> Eval a -> Eval a
withEnv env (Eval g) = Eval $ \ctx -> g (ctx { ctxLocalEnv = Just env })

newScope :: Int -> Eval a -> Eval a
newScope nvars action = do
    parentEnv <- getEnv
    arr <- liftIO' $ newArray_ (0, nvars)
    withEnv (LocalEnv arr parentEnv) action

defineVar :: ResolvedName -> Value -> Eval ()
defineVar (Global name) val = do
    GlobalEnv env <- getGlobalEnv
    valCell <- liftIO' $ val `seq` newIORef val
    liftIO' $ modifyIORef env (Map.insert name valCell)
defineVar (Local depth index _) val = do
    env <- getEnvAt depth
    case env of
      Nothing -> error "Name resolution error"
      Just env' -> liftIO' $ writeArray (values env') index val

data VarRef = VarRef { readRef :: Eval Value, writeRef :: Value -> Eval () }

varRef :: Int -> ResolvedName -> Eval VarRef
varRef line (Global name) = do
    GlobalEnv env <- getGlobalEnv
    m <- liftIO' $ readIORef env
    case Map.lookup name m of
      Just v -> pure $ VarRef (liftIO' (readIORef v)) (liftIO' . writeIORef v)
      Nothing -> throwErr $ mkDiagnostic line (" at " ++ name) "No such variable"
varRef _ (Local depth index _) = do
    env <- getEnvAt depth
    case env of
      Nothing -> error "Name resolution error"
      Just env' ->
          let arr = values env'
           in pure $ VarRef (liftIO' (readArray arr index)) (liftIO' . writeArray arr index)

lookupVar :: Int -> ResolvedName -> Eval Value
lookupVar line resolved = do
    ref <- varRef line resolved
    readRef ref

assignVar :: Int -> ResolvedName -> Value -> Eval ()
assignVar line resolved val = do
    ref <- varRef line resolved
    val `seq` writeRef ref val

exec :: GlobalEnv -> [Stmt ResolvedName] -> IO (EvalResult ())
exec env stmts = runEval (execProgram stmts) $ EvalContext env Nothing

execProgram :: [Stmt ResolvedName] -> Eval ()
execProgram []     = pure ()
execProgram (x:xs) = execStmt x >> execProgram xs

execStmt :: Stmt ResolvedName -> Eval ()
execStmt (PrintStmt _line expr) = do
    val <- eval expr
    liftIO' $ putStrLn (stringify val)
execStmt (ExprStmt  _line expr) = eval expr >> pure ()
execStmt (VarDecl _line name expr) = do
    val <- traverse eval expr
    defineVar name $ fromMaybe VNil val
execStmt (FunDecl _line name params body nvars) = do
    env <- getEnv
    u <- liftIO' newUnique
    defineVar name (VLochsFunction u env (Just name) params body nvars)
execStmt (Block _line stmts nvars) = newScope nvars (execProgram stmts)
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
call' (LochsFunction _ env params body nvars) args = do
    (maybe id withEnv env) . (newScope nvars) $ do
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
ensureCallable _ (VLochsFunction _ env _ params body nvars) =
    pure $ LochsFunction (length params) env params body nvars
ensureCallable l v                      = typeError l v "function"

hydrate :: LitValue -> Value
hydrate (LitBool b) = VBool b
hydrate (LitNumber n) = VNumber n
hydrate (LitString s) = VString s
hydrate LitNil = VNil

eval :: Expr ResolvedName -> Eval Value
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
    Fun _ params body nvars -> do
        env <- getEnv
        u <- liftIO' newUnique
        pure $ VLochsFunction u env Nothing params body nvars

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

logical :: LogicalOp -> Value -> Expr ResolvedName -> Eval Value
logical LogicalAnd val r = if isTruthy val then eval r else pure val
logical LogicalOr  val r = if isTruthy val then pure val else eval r
