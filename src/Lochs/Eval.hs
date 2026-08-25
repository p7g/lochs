{-# LANGUAGE Strict #-}

module Lochs.Eval (Abort(..), EvalResult(..), GlobalEnv, exec, mkEnv) where

import Control.Monad (when)
import Data.Array.Dynamic.L qualified as DA
import Data.Array.Base (unsafeNewArray_, unsafeRead, unsafeWrite)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Unique (newUnique)
import GHC.Clock (getMonotonicTime)

import Lochs.AST
import Lochs.Diagnostic
import Lochs.Runtime

data Abort = Break
           | Continue
           | Return Value
           | Err Diagnostic

data EvalResult a = Ok a
                  | Abort Abort

instance Functor EvalResult where
    {-# INLINE fmap #-}
    fmap f = \case
        Ok a    -> Ok (f a)
        Abort a -> Abort a

initialEnv :: [(String, Value)]
initialEnv = [("clock", nativeFunction FClock)]

mkEnv :: IO GlobalEnv
mkEnv = do
    m <- Map.fromList <$> traverse (traverse newIORef) initialEnv
    ref <- newIORef m
    pure $ GlobalEnv ref

data EvalContext = EvalContext
    { ctxGlobalEnv :: GlobalEnv
    , ctxLocalEnv  :: LocalEnv
    }

newtype Eval a = Eval { runEval :: EvalContext -> IO (EvalResult a) }

instance Functor Eval where
    {-# INLINE fmap #-}
    fmap f (Eval g) = Eval $ \ctx -> fmap (fmap f) (g ctx)

instance Applicative Eval where
    {-# INLINE pure #-}
    pure x = Eval $ \_ -> pure (Ok x)
    {-# INLINE (<*>) #-}
    Eval mf <*> Eval ma = Eval $ \ctx -> do
        rf <- mf ctx
        case rf of
          Ok f -> do
              ra <- ma ctx
              case ra of
                Ok a    -> pure $ Ok (f a)
                Abort a -> pure $ Abort a
          Abort a -> pure $ Abort a

instance Monad Eval where
    {-# INLINE (>>=) #-}
    Eval g >>= f = Eval $ \ctx ->
        g ctx >>= \case
            Ok a    -> runEval (f a) ctx
            Abort a -> pure $ Abort a

liftIO' :: IO a -> Eval a
liftIO' io = Eval $ \_ -> Ok <$> io

throwErr :: Diagnostic -> Eval a
throwErr d = Eval $ \_ -> pure $ Abort (Err d)

observe :: Eval a -> Eval (EvalResult a)
observe (Eval g) = Eval $ \ctx -> do
    result <- g ctx
    pure (Ok result)

reraise :: Abort -> Eval a
reraise r = Eval $ \_ -> pure $ Abort r

lochsReturn :: Value -> Eval a
lochsReturn v = Eval $ \_ -> pure $ Abort (Return v)

getEnv :: Eval LocalEnv
getEnv = Eval $ \ctx -> pure (Ok $ ctxLocalEnv ctx)

getScopeAt :: Int -> Eval Scope
getScopeAt n = do
    LocalEnv envs <- getEnv
    sz <- liftIO' $ DA.size envs
    liftIO' $ DA.unsafeRead envs (sz - n - 1)

getGlobalEnv :: Eval GlobalEnv
getGlobalEnv = Eval $ \ctx -> pure (Ok $ ctxGlobalEnv ctx)

withEnv :: LocalEnv -> Eval b -> Eval b
withEnv env (Eval g) = Eval $ \ctx -> g (ctx { ctxLocalEnv = env })

copyEnv :: LocalEnv -> Eval LocalEnv
copyEnv (LocalEnv scopes) = liftIO' $ do
    scopes' <- DA.empty
    DA.for scopes (DA.push scopes')
    pure $ LocalEnv scopes'

newScope :: Int -> Eval a -> Eval a
newScope 0 action = action
newScope nvars action = do
    LocalEnv env <- getEnv
    arr <- liftIO' $ unsafeNewArray_ (0, nvars - 1)
    let scope = Scope arr
    liftIO' $ DA.push env scope
    result <- action
    _ <- liftIO' $ DA.pop env
    pure result

{-# INLINE scopeWrite #-}
scopeWrite :: Scope -> Int -> Value -> IO ()
scopeWrite (Scope arr) ix v = unsafeWrite arr ix v

{-# INLINE scopeRead #-}
scopeRead :: Scope -> Int -> IO Value
scopeRead (Scope arr) ix = unsafeRead arr ix

defineVar :: ResolvedName -> Value -> Eval ()
defineVar (Global name) val = do
    GlobalEnv env <- getGlobalEnv
    valCell <- liftIO' $ val `seq` newIORef val
    liftIO' $ modifyIORef env (Map.insert name valCell)
defineVar (Local depth index _) val = do
    scope <- getScopeAt depth
    liftIO' $ scopeWrite scope index val

data VarRef = VarRef { readRef :: Eval Value, writeRef :: Value -> Eval () }

{-# INLINE varRef #-}
varRef :: Int -> ResolvedName -> Eval VarRef
varRef line (Global name) = do
    GlobalEnv env <- getGlobalEnv
    m <- liftIO' $ readIORef env
    case Map.lookup name m of
      Just v -> pure $ VarRef (liftIO' (readIORef v)) (liftIO' . writeIORef v)
      Nothing -> throwErr $ mkDiagnostic line (" at " ++ name) "No such variable"
varRef _ (Local depth index _) = do
    scope <- getScopeAt depth
    pure $ VarRef (liftIO' (scopeRead scope index)) (\val -> liftIO' $ scopeWrite scope index val)

lookupVar :: Int -> ResolvedName -> Eval Value
lookupVar line resolved = do
    ref <- varRef line resolved
    readRef ref

assignVar :: Int -> ResolvedName -> Value -> Eval ()
assignVar line resolved val = do
    ref <- varRef line resolved
    val `seq` writeRef ref val

exec :: GlobalEnv -> [Stmt ResolvedName] -> IO (EvalResult ())
exec global stmts = do
    localEnvs <- LocalEnv <$> DA.empty
    runEval (execProgram stmts) $ EvalContext global localEnvs

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
    env <- getEnv >>= copyEnv
    u <- liftIO' newUnique
    defineVar name (VLochsFunction u env (Just name) params body nvars (length params))
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
            Ok _             -> execStmt w
            Abort Continue   -> execStmt w
            Abort Break      -> pure ()
            Abort (Return v) -> lochsReturn v
            Abort (Err d)    -> reraise (Err d)
execStmt (BreakStmt _)    = Eval $ \_ -> pure $ Abort Break
execStmt (ContinueStmt _) = Eval $ \_ -> pure $ Abort Continue
execStmt (ReturnStmt _ e) = do
    val <- fromMaybe (pure VNil) $ fmap eval e
    lochsReturn val

callNative :: NativeFunctionID -> [Value] -> Eval Value
callNative FClock _ = VNumber <$> liftIO' getMonotonicTime

arityError :: Int -> Int -> Int -> Eval a
arityError line expected actual =
    let message = "Expected " ++ show expected
            ++ " arguments but got " ++ show actual
     in runtimeError line message

declareArgs :: Int -> Int -> [ResolvedName] -> [Value] -> Eval ()
declareArgs line arity params args = go params args 0
    where go [] [] _ = pure ()
          go [] r  p = arityError line arity (p + length r)
          go _  [] p = arityError line arity p
          go (p:ps) (a:as) np = do
              defineVar p a
              go ps as (np + 1)

call :: Int -> Value -> [Value] -> Eval Value
call line (VNativeFunction funId arity) args
  | length args == arity = callNative funId args
  | otherwise            = arityError line arity (length args)
call line (VLochsFunction _ locals _ params body nVars arity) args =
    (withEnv locals) . (newScope nVars) $ do
        declareArgs line arity params args
        result <- observe (execProgram body)
        case result of
          Ok _             -> pure VNil
          Abort Continue   -> error "Unreachable: continue crossing function boundary"
          Abort Break      -> error "Unreachable: break crossing function boundary"
          Abort (Return v) -> pure v
          Abort (Err d)    -> reraise (Err d)
call line val _ = typeError line val "function"

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
        call line callee' args'
    Fun _ params body nvars -> do
        env <- getEnv >>= copyEnv
        u <- liftIO' newUnique
        pure $ VLochsFunction u env Nothing params body nvars (length params)

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
