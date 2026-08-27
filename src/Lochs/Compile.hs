{-# LANGUAGE Strict, MagicHash, UnboxedTuples #-}

module Lochs.Compile (CompileContext, Program, compile, exec, mkContext) where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (forM, forM_, void, when)
import Data.Array.Dynamic.L qualified as DA
import GHC.Exts (Int (I#), newArray#, readArray#, writeArray#)
import GHC.IO (IO (IO))
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.Map qualified as Map
import Data.Text qualified as T
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

compile :: CompileContext -> [Stmt ResolvedName] -> IO Program
compile cctx stmts = Program <$> compileStmts cctx stmts

exec :: Program -> IO (Maybe Diagnostic)
exec (Program f) = do
    cctx <- newContext
    r <- try @LochsError (f cctx)
    pure $ case r of
             Left (LochsError d) -> Just d
             Right _           -> Nothing

newtype GlobalEnv = GlobalEnv (IORef (Map.Map String (IORef Value)))

data CompileContext = CompileContext
    { globalEnv :: GlobalEnv
    , attrTable :: IORef (Map.Map String Int)
    , nextAttr :: IORef Int
    }

initialEnv :: [(String, Value)]
initialEnv = [("clock", nativeFunction FClock)]

mkEnv :: IO GlobalEnv
mkEnv = do
    m <- Map.fromList <$> traverse (traverse newIORef) initialEnv
    GlobalEnv <$> newIORef m

mkContext :: IO CompileContext
mkContext = do
    env <- mkEnv
    attrs <- newIORef Map.empty
    attrCount <- newIORef 0
    pure $ CompileContext env attrs attrCount

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

pushScope :: LocalEnv -> Scope -> IO ()
pushScope (LocalEnv arr) scope = DA.push arr scope

popScope :: LocalEnv -> IO ()
popScope (LocalEnv arr) = void $! DA.pop arr

withScope :: Scope -> StmtC -> StmtC
withScope scope action ctx = do
    pushScope (ctxLocal ctx) scope
    result <- action ctx
    popScope (ctxLocal ctx)
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

compileStmts :: CompileContext -> [Stmt ResolvedName] -> IO StmtC
compileStmts cctx stmts = seqStmts <$> traverse (compileStmt cctx) stmts

compileStmt :: CompileContext -> Stmt ResolvedName -> IO StmtC
compileStmt cctx = \case
    PrintStmt _ expr -> do
        exprC <- compileExpr cctx expr
        pure $ \ctx -> exprC ctx >>= putStrLn . stringify >> pure Normal
    ExprStmt _ expr -> do
        exprC <- compileExpr cctx expr
        pure $ \ctx -> exprC ctx >> pure Normal
    VarDecl _ (Local depth ix _) expr -> do
        exprC <- maybe (pure (const (pure VNil))) (compileExpr cctx) expr
        pure $ \ctx -> exprC ctx >>= defineLocal ctx depth ix >> pure Normal
    VarDecl _ (Global name) expr -> do
        exprC <- maybe (pure (const (pure VNil))) (compileExpr cctx) expr
        global <- globalRef (globalEnv cctx) name
        pure $ \ctx -> exprC ctx >>= defineGlobal global >> pure Normal
    FunDecl _ resolved@(Local depth ix _) params body nvars -> do
        bodyC <- compileStmts cctx body
        let arity = length params
        pure $ \ctx -> do
            u <- newUnique
            env <- copyEnv (ctxLocal ctx)
            let val = VLochsFunction u env (Just resolved) params bodyC nvars arity False
            defineLocal ctx depth ix val
            pure Normal
    FunDecl _ resolved@(Global name) params body nvars -> do
        bodyC <- compileStmts cctx body
        let arity = length params
        global <- globalRef (globalEnv cctx) name
        pure $ \_ -> do
            u <- newUnique
            env <- LocalEnv <$> DA.empty
            let val = VLochsFunction u env (Just resolved) params bodyC nvars arity False
            defineGlobal global val
            pure Normal
    ClassDecl _ resolved@(Local depth ix _) methodDecls -> do
        arityRef <- newIORef 0
        compiledMethods <- forM methodDecls $ \(MethodDecl _ n p b nvars) -> do
            bC <- compileStmts cctx b
            let arity = length p
                isInit = n == "init"
            when isInit $ writeIORef arityRef arity
            attrId <- internAttr cctx n
            pure $! (attrId, Just (Global n), p, bC, nvars, arity, isInit)

        clsArity <- readIORef arityRef
        pure $ \ctx -> do
            methodsRef <- newIORef IntMap.empty

            forM_ compiledMethods $ \(attrId, n, p, bC, nvars, arity, isInit) -> do
                env <- copyEnv (ctxLocal ctx)
                u <- newUnique
                let val = VLochsFunction u env n p bC nvars arity isInit
                modifyIORef' methodsRef (IntMap.insert attrId val)

            methods <- readIORef methodsRef
            u <- newUnique
            let val = VClass (Class u resolved clsArity methods)
            defineLocal ctx depth ix val
            pure Normal
    ClassDecl _ resolved@(Global name) methodDecls -> do
        global <- globalRef (globalEnv cctx) name
        methodsRef <- newIORef IntMap.empty
        arityRef <- newIORef 0

        forM_ methodDecls $ \(MethodDecl _ n p b nvars) -> do
            -- methods on global classes have no closure
            env <- LocalEnv <$> DA.empty
            u <- newUnique
            bC <- compileStmts cctx b
            attrId <- internAttr cctx n
            let arity = length p
                isInit = n == "init"
                f = VLochsFunction u env (Just (Global n)) p bC nvars arity isInit
            when isInit $ writeIORef arityRef arity
            modifyIORef' methodsRef (IntMap.insert attrId f)

        arity <- readIORef arityRef
        methods <- readIORef methodsRef
        pure $ \_ -> do
            u <- newUnique
            let val = VClass (Class u resolved arity methods)
            defineGlobal global val
            pure Normal
    Block _ stmts nvars -> compileStmts cctx stmts >>= pure . withNewScope nvars
    IfStmt _ cond cons alt -> do
        condC <- compileExpr cctx cond
        consC <- compileStmt cctx cons
        altC  <- maybe (pure (const (pure Normal))) (compileStmt cctx) alt
        pure $ \ctx -> condC ctx >>= \v -> if isTruthy v then consC ctx else altC ctx
    WhileStmt _ cond body -> do
        condC <- compileExpr cctx cond
        bodyC <- compileStmt cctx body
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
            exprC <- compileExpr cctx expr'
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
hydrate (LitString s) = VString (T.pack s)
hydrate LitNil = VNil

internAttr :: CompileContext -> String -> IO Int
internAttr cctx s = do
    tbl <- readIORef (attrTable cctx)
    case Map.lookup s tbl of
      Just attrId -> pure attrId
      Nothing -> do
        attrId <- atomicModifyIORef' (nextAttr cctx) $ \n -> (n + 1, n)
        writeIORef (attrTable cctx) (Map.insert s attrId tbl)
        pure attrId

boundMethod :: Int -> Value -> Class -> Int -> IO (Maybe Value)
boundMethod line obj (Class _ _ _ methods) attrId =
    case IntMap.lookup attrId methods of
      Just (VLochsFunction _ env n p b nvars arity isInit) -> do
          u <- newUnique
          env' <- copyEnv env
          scope <- newScope 1
          writeSlot scope 0 obj -- this
          pushScope env' scope
          let f = VLochsFunction u env' n p b nvars arity isInit
          pure (Just f)
      Just v  -> typeError line v "function" -- internal error
      Nothing -> pure Nothing

compileExpr :: CompileContext -> Expr ResolvedName -> IO ExprC
compileExpr cctx = \case
    Literal _ lit ->
        let v = hydrate lit
        in pure $ \_ -> pure v
    Grouping _ expr -> compileExpr cctx expr
    Unary line op expr -> do
        exprC <- compileExpr cctx expr
        unary line op exprC
    Binary line l op r -> do
        lC <- compileExpr cctx l
        rC <- compileExpr cctx r
        binary line op lC rC
    Logical _ l op r -> do
        lC <- compileExpr cctx l
        rC <- compileExpr cctx r
        let opFun v = case op of
                        LogicalAnd -> isTruthy v
                        LogicalOr  -> not (isTruthy v)
        pure $ \ctx -> lC ctx >>= \v -> if opFun v then rC ctx else pure v
    Variable _ (Local depth ix _) -> pure $ lookupLocal depth ix
    Variable line (Global name) -> do
        global <- globalRef (globalEnv cctx) name
        pure $ \_ -> lookupGlobal line name global
    Assign _ (Local depth ix _) expr -> do
        exprC <- compileExpr cctx expr
        pure $ \ctx -> do
            val <- exprC ctx
            assignLocal ctx depth ix val
            pure val
    Assign line (Global name) expr -> do
        exprC <- compileExpr cctx expr
        global <- globalRef (globalEnv cctx) name
        pure $ \ctx -> do
            val <- exprC ctx
            assignGlobal line name global val
            pure val
    Call line callee args -> do
        calleeC <- compileExpr cctx callee
        argsC <- traverse (compileExpr cctx) args
        call cctx line calleeC argsC
    Fun _ params body nvars -> do
        bodyC <- compileStmts cctx body
        let arity = length params
        pure $ \ctx -> do
            u <- newUnique
            env <- copyEnv (ctxLocal ctx)
            pure $! VLochsFunction u env Nothing params bodyC nvars arity False
    GetProp line expr attr -> do
        exprC <- compileExpr cctx expr
        attrId <- internAttr cctx attr
        pure $ \ctx -> do
            obj <- exprC ctx
            case obj of
              VInstance _ cls fieldsRef -> do
                  fields <- readIORef fieldsRef
                  case IntMap.lookup attrId fields of
                    Just val -> pure val
                    Nothing -> do
                        method <- boundMethod line obj cls attrId
                        case method of
                          Just method' -> pure method'
                          Nothing -> runtimeError line ("Undefined property " ++ attr)
              _ -> typeError line obj "object"
    SetProp line expr attr value -> do
        exprC <- compileExpr cctx expr
        valueC <- compileExpr cctx value
        attrId <- internAttr cctx attr
        pure $ \ctx -> do
            obj <- exprC ctx
            case obj of
              VInstance _ _ fieldsRef -> do
                  val <- valueC ctx
                  modifyIORef' fieldsRef (IntMap.insert attrId val)
                  pure val
              _ -> typeError line obj "object"
    This _ (Local depth ix _) -> pure $ lookupLocal depth ix
    This _ (Global _) -> error "global this"

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
          VString r' -> pure $! VString (T.append l' r')
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

lochsCall :: EvalContext -> Int -> LocalEnv -> [ResolvedName] -> StmtC -> Int -> Int -> [ExprC] -> Bool
          -> IO Value
lochsCall ctx line env params body nvars arity argsC isInit = do
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
    pushScope env scope
    let ctx' = ctx { ctxLocal = env }
    result <- body ctx'
    popScope env
    if isInit
       then do
           this <- lookupLocal 0 0 ctx'
           pure this
       else case result of
              Normal -> pure VNil
              Return v -> pure v
              _ -> runtimeError line "break/continue escaped from call"

call :: CompileContext -> Int -> ExprC -> [ExprC] -> IO ExprC
call cctx line calleeC argsC = do
    initId <- internAttr cctx "init"
    pure $ \ctx -> do
        callee <- calleeC ctx
        case callee of
          VLochsFunction _ env _ params body nvars arity isInit ->
              lochsCall ctx line env params body nvars arity argsC isInit
          VNativeFunction funId arity -> do
            args <- traverse ($ ctx) argsC
            if length args == arity
               then callNative funId args
               else arityError line arity (length args)
          VClass c@(Class _ _ arity _) -> do
            args <- traverse ($ ctx) argsC
            u <- newUnique
            d <- newIORef IntMap.empty
            if length args == arity
               then do
                   let obj = VInstance u c d
                   initMethod <- boundMethod line obj c initId
                   case initMethod of
                     Just (VLochsFunction _ env _ params body nvars _ _) ->
                         void $! lochsCall ctx line env params body nvars arity argsC True
                     Just v -> typeError line v "function"
                     Nothing -> pure ()
                   pure obj
               else arityError line arity (length args)
          val -> typeError line val "function"
