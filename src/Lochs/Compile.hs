{-# LANGUAGE Strict, MagicHash, UnboxedTuples #-}

module Lochs.Compile (GlobalEnv, Program, compile, exec, mkEnv) where

import Control.Exception (Exception, throwIO, try)
import Data.Array.Dynamic.L qualified as DA
import GHC.Exts (Int (I#), newArray#, readArray#, writeArray#)
import GHC.IO (IO (IO))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Unique (newUnique)
import GHC.Clock (getMonotonicTime)

import Lochs.AST
import Lochs.Diagnostic
import Lochs.Runtime

newtype Program = Program (EvalContext -> IO Flow)

data LochsError = LochsError Diagnostic

instance Exception LochsError

instance Show LochsError where
    show (LochsError d) = show d

compile :: GlobalEnv -> [Stmt ResolvedName] -> IO Program
compile globals stmts = Program <$> compileStmts globals stmts

exec :: Program -> IO (Maybe Diagnostic)
exec (Program f) = do
    ctx <- newContext
    r <- try @LochsError (f ctx)
    pure $ case r of
             Left (LochsError d) -> Just d
             Right _           -> Nothing

newtype GlobalEnv = GlobalEnv (IORef (Map.Map String (IORef Value)))

initialEnv :: [(String, Value)]
initialEnv = [("clock", nativeFunction FClock)]

mkEnv :: IO GlobalEnv
mkEnv = do
    m <- Map.fromList <$> traverse (traverse newIORef) initialEnv
    GlobalEnv <$> newIORef m

newContext :: IO EvalContext
newContext = do
    local <- LocalEnv <$> DA.empty
    pure $ EvalContext local

getScopeAt :: EvalContext -> Int -> IO Scope
getScopeAt ctx n = do
    let LocalEnv envs = ctxLocal ctx
    sz <- DA.size envs
    DA.unsafeRead envs (sz - n - 1)

globalRef :: GlobalEnv -> String -> IO (IORef Value)
globalRef (GlobalEnv envRef) name = do
    m <- readIORef envRef
    case Map.lookup name m of
      Just ref -> pure ref
      Nothing -> do
          ref <- newIORef VUninit
          modifyIORef' envRef $ Map.insert name ref
          pure ref

{-# NOINLINE undefinedGlobal #-}
undefinedGlobal :: Int -> String -> IO a
undefinedGlobal line name = throwIO $ LochsError diag
    where diag = mkDiagnostic line (" at " ++ name) "No such variable"

verifyGlobal :: Int -> String -> Value -> IO ()
verifyGlobal line name = \case
    VUninit -> undefinedGlobal line name
    _ -> pure ()

lookupGlobal :: Int -> String -> IORef Value -> IO Value
lookupGlobal line name ref = do
    val <- readIORef ref
    verifyGlobal line name val
    pure val

assignGlobal :: Int -> String -> IORef Value -> Value -> IO ()
assignGlobal line name ref val = do
    existing <- readIORef ref
    verifyGlobal line name existing
    writeIORef ref val

defineGlobal :: IORef Value -> Value -> IO ()
defineGlobal = writeIORef

defineLocal :: EvalContext -> Int -> Int -> Value -> IO ()
defineLocal ctx depth index val = do
    scope <- getScopeAt ctx depth
    val `seq` writeSlot scope index val

lookupLocal :: Int -> Int -> EvalContext -> IO Value
lookupLocal depth ix ctx = do
    scope <- getScopeAt ctx depth
    readSlot scope ix

assignLocal :: EvalContext -> Int -> Int -> Value -> IO ()
assignLocal ctx depth ix val = do
    scope <- getScopeAt ctx depth
    val `seq` writeSlot scope ix val

copyEnv :: LocalEnv -> IO LocalEnv
copyEnv (LocalEnv scopes) = do
    scopes' <- DA.empty
    DA.for scopes (DA.push scopes')
    pure $ LocalEnv scopes'

newScope :: Int -> IO Scope
newScope (I# nvars) = IO $ \s ->
    case newArray# nvars VUninit s of
      (# s', arr #) -> (# s', Scope arr #)

readSlot :: Scope -> Int -> IO Value
readSlot (Scope arr) (I# ix) = IO (readArray# arr ix)

writeSlot :: Scope -> Int -> Value -> IO ()
writeSlot (Scope arr) (I# ix) val = IO $ \s ->
    case writeArray# arr ix val s of s' -> (# s', () #)

pushScope :: EvalContext -> Scope -> IO ()
pushScope ctx scope = do
    let LocalEnv arr = ctxLocal ctx
    DA.push arr scope

popScope :: EvalContext -> IO ()
popScope ctx = do
    let LocalEnv arr = ctxLocal ctx
    _ <- DA.pop arr
    pure ()

withScope :: Scope -> StmtC -> StmtC
withScope scope action ctx = do
    pushScope ctx scope
    result <- action ctx
    popScope ctx
    pure result

withNewScope :: Int -> StmtC -> StmtC
withNewScope nvars action ctx = do
    scope <- newScope nvars
    withScope scope action ctx

seqStmts :: [StmtC] -> StmtC
seqStmts []  = \_ -> pure Normal
seqStmts [s] = s
seqStmts (a : rest) =
    let b = seqStmts rest
     in \ctx -> do
         f <- a ctx
         case f of
           Normal -> b ctx
           other  -> pure other

typeError :: Int -> Value -> String -> IO a
typeError line val expected = throwIO $ LochsError diag
    where diag = mkDiagnostic line "" ("Expected " ++ expected ++ " but got " ++ typeName val)

runtimeError :: Int -> String -> IO a
runtimeError line message = throwIO $ LochsError diag
    where diag = mkDiagnostic line "" message

compileStmts :: GlobalEnv -> [Stmt ResolvedName] -> IO StmtC
compileStmts globals stmts = seqStmts <$> traverse (compileStmt globals) stmts

compileStmt :: GlobalEnv -> Stmt ResolvedName -> IO StmtC
compileStmt globals = \case
    PrintStmt _ expr -> do
        exprC <- compileExpr globals expr
        pure $ \ctx -> exprC ctx >>= putStrLn . stringify >> pure Normal
    ExprStmt _ expr -> do
        exprC <- compileExpr globals expr
        pure $ \ctx -> exprC ctx >> pure Normal
    VarDecl _ (Local depth ix _) expr -> do
        exprC <- maybe (pure (const (pure VNil))) (compileExpr globals) expr
        pure $ \ctx -> exprC ctx >>= defineLocal ctx depth ix >> pure Normal
    VarDecl _ (Global name) expr -> do
        exprC <- maybe (pure (const (pure VNil))) (compileExpr globals) expr
        global <- globalRef globals name
        pure $ \ctx -> exprC ctx >>= defineGlobal global >> pure Normal
    FunDecl _ resolved@(Local depth ix _) params body nvars -> do
        bodyC <- compileStmts globals body
        let arity = length params
        pure $ \ctx -> do
            u <- newUnique
            env <- copyEnv (ctxLocal ctx)
            let val = VLochsFunction u env (Just resolved) params bodyC nvars arity
            defineLocal ctx depth ix val
            pure Normal
    FunDecl _ resolved@(Global name) params body nvars -> do
        bodyC <- compileStmts globals body
        let arity = length params
        global <- globalRef globals name
        pure $ \_ -> do
            u <- newUnique
            env <- LocalEnv <$> DA.empty
            let val = VLochsFunction u env (Just resolved) params bodyC nvars arity
            defineGlobal global val
            pure Normal
    Block _ stmts nvars -> compileStmts globals stmts >>= pure . withNewScope nvars
    IfStmt _ cond cons alt -> do
        condC <- compileExpr globals cond
        consC <- compileStmt globals cons
        altC  <- maybe (pure (const (pure Normal))) (compileStmt globals) alt
        pure $ \ctx -> condC ctx >>= \v -> if isTruthy v then consC ctx else altC ctx
    WhileStmt _ cond body -> do
        condC <- compileExpr globals cond
        bodyC <- compileStmt globals body
        let loop ctx = do
                val <- condC ctx
                if (isTruthy val)
                then do
                    r <- bodyC ctx
                    case r of
                        Normal -> loop ctx
                        Continue -> loop ctx
                        Break -> pure Normal
                        Return v -> pure (Return v)
                else pure Normal
        pure loop
    BreakStmt _       -> pure $ \_ -> pure Break
    ContinueStmt _    -> pure $ \_ -> pure Continue
    ReturnStmt _ expr ->
        case expr of
        Just expr' -> do
            exprC <- compileExpr globals expr'
            pure $ \ctx -> exprC ctx >>= \v -> pure $! Return v
        Nothing -> pure $ \_ -> pure (Return VNil)

vTrue, vFalse :: Value
vTrue = VBool True
vFalse = VBool False

mkBool :: Bool -> Value
mkBool b = if b then vTrue else vFalse

hydrate :: LitValue -> Value
hydrate (LitBool b) = VBool b
hydrate (LitNumber n) = VNumber n
hydrate (LitString s) = VString s
hydrate LitNil = VNil

compileExpr :: GlobalEnv -> Expr ResolvedName -> IO ExprC
compileExpr globals = \case
    Literal _ lit ->
        let v = hydrate lit
        in pure $ \_ -> pure v
    Grouping _ expr -> compileExpr globals expr
    Unary line op expr -> do
        exprC <- compileExpr globals expr
        unary line op exprC
    Binary line l op r -> do
        lC <- compileExpr globals l
        rC <- compileExpr globals r
        binary line op lC rC
    Logical _ l op r -> do
        lC <- compileExpr globals l
        rC <- compileExpr globals r
        let opFun v = case op of
                        LogicalAnd -> isTruthy v
                        LogicalOr  -> not (isTruthy v)
        pure $ \ctx -> lC ctx >>= \v -> if opFun v then rC ctx else pure v
    Variable _ (Local depth ix _) -> pure $ lookupLocal depth ix
    Variable line (Global name) -> do
        global <- globalRef globals name
        pure $ \_ -> lookupGlobal line name global
    Assign _ (Local depth ix _) expr -> do
        exprC <- compileExpr globals expr
        pure $ \ctx -> do
            val <- exprC ctx
            assignLocal ctx depth ix val
            pure val
    Assign line (Global name) expr -> do
        exprC <- compileExpr globals expr
        global <- globalRef globals name
        pure $ \ctx -> do
            val <- exprC ctx
            assignGlobal line name global val
            pure val
    Call line callee args -> do
        calleeC <- compileExpr globals callee
        argsC <- traverse (compileExpr globals) args
        pure $ call line calleeC argsC
    Fun _ params body nvars -> do
        bodyC <- compileStmts globals body
        let arity = length params
        pure $ \ctx -> do
            u <- newUnique
            env <- copyEnv (ctxLocal ctx)
            pure $ VLochsFunction u env Nothing params bodyC nvars arity

unary :: Int -> UnaryOp -> ExprC -> IO ExprC
unary line UnaryNeg exprC = pure $ \ctx ->
    exprC ctx >>= \case
        VNumber n -> pure $! VNumber (-n)
        other     -> typeError line other "number"
unary _ UnaryNot exprC = pure $ \ctx -> do
    v <- exprC ctx
    pure $! mkBool (not (isTruthy v))

{-# INLINE numBinOp #-}
numBinOp :: Int -> (Double -> Double -> IO Value) -> ExprC -> ExprC -> ExprC
numBinOp line op lC rC ctx = do
    l <- lC ctx
    r <- rC ctx
    case (l, r) of
      (VNumber l', VNumber r') -> op l' r'
      (VNumber _, other) -> typeError line other "number"
      (other, _) -> typeError line other "number"

binary :: Int -> BinaryOp -> ExprC -> ExprC -> IO ExprC
binary line BinSub lC rC = pure $ \ctx -> numBinOp line (\l r -> pure $! VNumber (l - r)) lC rC ctx
binary line BinDiv lC rC =
    let safeDiv _ 0 = runtimeError line "Division by zero"
        safeDiv l r = pure $! VNumber (l / r)
     in pure $ \ctx -> numBinOp line safeDiv lC rC ctx
binary line BinMul lC rC = pure $ \ctx -> numBinOp line (\l r -> pure $! VNumber (l * r)) lC rC ctx
binary line BinAdd lC rC = pure $ \ctx -> do
    l <- lC ctx
    r <- rC ctx
    case l of
      VNumber l' -> case r of
          VNumber r' -> pure $! VNumber (l' + r')
          other      -> typeError line other "number"
      VString l' -> case r of
          VString r' -> pure $! VString (l' ++ r')
          other      -> typeError line other "string"
      other -> typeError line other "number or string"
binary line BinGt  lC rC = pure $ \ctx -> numBinOp line (\l r -> pure $! mkBool (l > r)) lC rC ctx
binary line BinGte lC rC = pure $ \ctx -> numBinOp line (\l r -> pure $! mkBool (l >= r)) lC rC ctx
binary line BinLt  lC rC = pure $ \ctx -> numBinOp line (\l r -> pure $! mkBool (l < r)) lC rC ctx
binary line BinLte lC rC = pure $ \ctx -> numBinOp line (\l r -> pure $! mkBool (l <= r)) lC rC ctx
binary _ BinEq  lC rC = pure $ \ctx -> do
    l <- lC ctx
    r <- rC ctx
    pure $! mkBool (isEqual l r)
binary _ BinNe  lC rC = pure $ \ctx -> do
    l <- lC ctx
    r <- rC ctx
    pure $! mkBool (not (isEqual l r))

arityError :: Int -> Int -> Int -> IO a
arityError line expected actual =
    let message = "Expected " ++ show expected
            ++ " arguments but got " ++ show actual
     in runtimeError line message

callNative :: NativeFunctionID -> [Value] -> IO Value
callNative FClock _ = VNumber <$> getMonotonicTime

call :: Int -> ExprC -> [ExprC] -> ExprC
call line calleeC argsC ctx = do
    callee <- calleeC ctx
    case callee of
      VLochsFunction _ env _ params body nvars arity -> do
          scope <- newScope nvars
          -- Evaluate each argument in the caller's context and write it
          -- straight into the callee's scope, so no argument list is built.
          let go (Local _ ix _ : ps) (a : as) = do
                  v <- a ctx
                  v `seq` writeSlot scope ix v
                  go ps as
              go [] [] = pure ()
              go [] r  = arityError line arity (arity + length r)
              go p  [] = arityError line arity (arity - length p)
              go _  _  = error "parameter resolved as global"
          go params argsC
          let ctx' = ctx { ctxLocal = env }
          pushScope ctx' scope
          result <- body ctx'
          popScope ctx'
          case result of
            Normal -> pure VNil
            Return v -> pure v
            _ -> runtimeError line "break/continue escaped from call"
      VNativeFunction funId arity -> do
          args <- traverse ($ ctx) argsC
          if length args == arity
            then callNative funId args
            else arityError line arity (length args)
      val -> typeError line val "function"
