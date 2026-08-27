module Lochs.Resolver (resolve) where

import Control.Monad (forM, when)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)

import Lochs.AST
import Lochs.Diagnostic

resolve :: [Stmt UnresolvedName] -> ([Stmt ResolvedName], [Diagnostic])
resolve ss =
    let context = Context NoFunction False NoClass
        (ss', _, d) = runResolve (traverse resolveStmt ss) [] context
     in (ss', d)

data EnvEntry = EnvEntry
    { defined :: Bool
    , index :: Int
    , uses :: Int
    , declLine :: Int
    }

type EnvMap = Map.Map String EnvEntry
type Env = [EnvMap]

data FunctionType = Function | Method | Initializer | NoFunction
    deriving (Eq)

data ClassType = Class | NoClass
    deriving (Eq)

data Context = Context
    { funType :: FunctionType
    , inLoop :: Bool
    , classType :: ClassType
    }

newtype Resolve a = Resolve { runResolve :: Env -> Context -> (a, Env, [Diagnostic]) }

instance Functor Resolve where
    fmap f (Resolve g) = Resolve $ \env ctx ->
        let (a, env', d) = g env ctx
         in (f a, env', d)

instance Applicative Resolve where
    pure a = Resolve $ \env _ -> (a, env, [])

    Resolve f <*> Resolve x = Resolve $ \env ctx ->
        let (g, env', d1) = f env ctx
            (a, env'', d2) = x env' ctx
         in (g a, env'', d1 ++ d2)

instance Monad Resolve where
    Resolve m >>= k = Resolve $ \env ctx ->
        let (a, env', d1) = m env ctx
            (b, env'', d2) = runResolve (k a) env' ctx
         in (b, env'', d1 ++ d2)

modifyContext :: (Context -> Context) -> Resolve a -> Resolve a
modifyContext f (Resolve m) = Resolve $ \env ctx -> m env (f ctx)

withinLoop :: Resolve a -> Resolve a
withinLoop = modifyContext $ \ctx -> ctx { inLoop = True }

readContext :: (Context -> a) -> Resolve a
readContext f = Resolve $ \env ctx -> (f ctx, env, [])

getEnv :: Resolve Env
getEnv = Resolve $ \env _ -> (env, env, [])

modifyEnv :: (Env -> Env) -> Resolve ()
modifyEnv f = Resolve $ \env _ -> ((), f env, [])

diag :: Diagnostic -> Resolve ()
diag d = Resolve $ \env _ -> ((), env, [d])

enterScope :: Resolve ()
enterScope = modifyEnv (Map.empty:)

exitScope :: Resolve ()
exitScope = modifyEnv (drop 1)

scopeSize :: Resolve Int
scopeSize = do
    env <- getEnv
    case env of
      [] -> pure 0
      m:_ -> pure $ Map.size m

modifyLocal :: (EnvMap -> EnvMap) -> Env -> Env
modifyLocal _ [] = []
modifyLocal f (m:ms) = f m : ms

checkExisting :: Int -> String -> Env -> Resolve ()
checkExisting line name (m:_) | Map.member name m = 
    diag (mkDiagnostic line "" "Already a variable with this name in scope")
checkExisting _ _ _ = pure ()

declare :: Int -> UnresolvedName -> Resolve ()
declare line (UnresolvedName name) = do
    env <- getEnv
    checkExisting line name env
    modifyEnv $ modifyLocal (\m -> Map.insert name (EnvEntry False (Map.size m) 0 line) m)

define :: UnresolvedName -> Resolve ()
define (UnresolvedName name) = modifyEnv $ modifyLocal (Map.adjust (\e -> e { defined = True }) name)

countDepth :: String -> Env -> Maybe (Int, Int)
countDepth name = loop 0
    where loop depth = \case
            [] -> Nothing
            m:ms ->
                case Map.lookup name m of
                  Just e -> Just (depth, index e)
                  Nothing -> loop (depth + 1) ms

lookupName :: Int -> UnresolvedName -> Resolve ResolvedName
lookupName line (UnresolvedName name) = do
    env <- getEnv
    case env of
      [] -> pure $ Global name
      m:_ -> do
          let undef = fromMaybe False (not . defined <$> Map.lookup name m)
          when undef $
            diag (mkDiagnostic line "" "Can't read local variable in its own initializer")
          pure $ case countDepth name env of
            Just (depth, index) -> Local depth index name
            Nothing -> Global name

trackUse :: ResolvedName -> Resolve ()
trackUse (Global _) = pure ()
trackUse (Local depth _ name) = modifyEnv $ \env ->
    let (pre, ms) = splitAt depth env
     in case ms of
          [] -> error "name resolution error"
          m:post ->
              let m' = Map.adjust (\e -> e { uses = uses e + 1 }) name m
                  env' = pre ++ (m':post)
               in env'

reportUnused :: Resolve ()
reportUnused = do
    env <- getEnv
    case env of
      [] -> error "internal compiler error"
      m:_ ->
          let unused = sortOn (declLine . snd) [(k, v) | (k, v) <- Map.toList m, uses v == 0]
              report name entry =
                  diag $ mkDiagnostic (declLine entry) "" ("Unused variable " ++ name)
           in traverse_ (uncurry report) unused

resolveFun :: Int -> FunctionType -> [UnresolvedName] -> [Stmt UnresolvedName]
           -> Resolve ([Stmt ResolvedName], Int, [ResolvedName])
resolveFun line ty params body = do
    enterScope
    traverse_ (declare line) params
    traverse_ define params
    body' <- modifyContext (\ctx -> ctx { funType = ty, inLoop = False }) $
        traverse resolveStmt body
    nvars <- scopeSize
    params' <- traverse (lookupName line) params
    traverse_ trackUse params' -- don't error for unused params
    reportUnused
    exitScope
    pure (body', nvars, params')

resolveStmt :: Stmt UnresolvedName -> Resolve (Stmt ResolvedName)
resolveStmt = \case
    ExprStmt l e    -> ExprStmt l <$> resolveExpr e
    PrintStmt l e   -> PrintStmt l <$> resolveExpr e
    Block l s _     -> do
        enterScope
        s' <- traverse resolveStmt s
        nvars <- scopeSize
        reportUnused
        exitScope
        pure $ Block l s' nvars
    IfStmt l c t e  -> IfStmt l <$> resolveExpr c <*> resolveStmt t <*> traverse resolveStmt e
    WhileStmt l c b -> WhileStmt l <$> resolveExpr c <*> withinLoop (resolveStmt b)
    BreakStmt l     -> do
        loop <- readContext inLoop
        when (not loop) $
            diag (mkDiagnostic l "" "'break' outside loop")
        pure $ BreakStmt l
    ContinueStmt l -> do
        loop <- readContext inLoop
        when (not loop) $
            diag (mkDiagnostic l "" "'continue' outside loop")
        pure $ ContinueStmt l
    ReturnStmt l e -> do
        f <- readContext funType
        when (f == NoFunction) $
            diag (mkDiagnostic l "" "'return' outside function")
        when (f == Initializer && isJust e) $
            diag (mkDiagnostic l "" "Can't return a value from an initializer")
        ReturnStmt l <$> traverse resolveExpr e
    VarDecl l n e -> do
        declare l n
        e' <- traverse resolveExpr e
        define n
        n' <- lookupName l n
        pure $ VarDecl l n' e'
    FunDecl l n p b _ -> do
        declare l n
        define n
        n' <- lookupName l n
        (b', nvars, p') <- resolveFun l Function p b
        pure $ FunDecl l n' p' b' nvars
    ClassDecl l n ms -> modifyContext (\_ -> Context NoFunction False Class) $ do
        declare l n
        define n
        n' <- lookupName l n
        enterScope
        declare l (UnresolvedName "this")
        define (UnresolvedName "this")
        ms' <- forM ms $ \(MethodDecl l' mn p b _) -> do
            let ft = if mn == "init" then Initializer else Method
            (b', nvars, p') <- resolveFun l' ft p b
            pure $ MethodDecl l' mn p' b' nvars
        exitScope
        pure $ ClassDecl l n' ms'

resolveExpr :: Expr UnresolvedName -> Resolve (Expr ResolvedName)
resolveExpr = \case
    Binary l a o b  -> Binary l <$> resolveExpr a <*> pure o <*> resolveExpr b
    Logical l a o b -> Logical l <$> resolveExpr a <*> pure o <*> resolveExpr b
    Grouping l e    -> Grouping l <$> resolveExpr e
    Literal l v     -> pure $ Literal l v
    Unary l o e     -> Unary l o <$> resolveExpr e
    Variable l n    -> do
        n' <- lookupName l n
        trackUse n'
        pure $ Variable l n'
    Assign l n e    -> Assign l <$> lookupName l n <*> resolveExpr e
    Call l c a      -> Call l <$> resolveExpr c <*> traverse resolveExpr a
    Fun l p b _     -> do
        (b', nvars, p') <- resolveFun l Function p b
        pure $ Fun l p' b' nvars
    GetProp l e n   -> GetProp l <$> resolveExpr e <*> pure n
    SetProp l e n v -> SetProp l <$> resolveExpr e <*> pure n <*> resolveExpr v
    This l n        -> do
        cls <- readContext classType
        when (cls == NoClass) $
            diag (mkDiagnostic l "" "'this' outside class")
        n' <- lookupName l n
        trackUse n'
        pure $ This l n'
