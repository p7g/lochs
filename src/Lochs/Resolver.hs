module Lochs.Resolver (resolve) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Map qualified as Map

import Lochs.AST
import Lochs.Diagnostic

resolve :: [Stmt UnresolvedName] -> ([Stmt ResolvedName], [Diagnostic])
resolve ss =
    let (ss', _, d) = runResolve (traverse resolveStmt ss) [] (Context NoFunction False)
     in (ss', d)

type Env = [Map.Map String Bool]

data FunctionType = Function | NoFunction
    deriving (Eq)

data Context = Context
    { funType :: FunctionType
    , inLoop :: Bool
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

setLocal :: String -> Bool -> Env -> Env
setLocal _ _ [] = []
setLocal n v (m:ms) = Map.insert n v m : ms

checkExisting :: Int -> String -> Env -> Resolve ()
checkExisting line name (m:_) | Map.member name m = 
    diag (mkDiagnostic line "" "Already a variable with this name in scope")
checkExisting _ _ _ = pure ()

declare :: Int -> String -> Resolve ()
declare line name = do
    env <- getEnv
    checkExisting line name env
    modifyEnv $ setLocal name False

define :: String -> Resolve ()
define name = modifyEnv $ setLocal name True

countDepth :: String -> Env -> Maybe Int
countDepth name = loop 0
    where loop depth = \case
            [] -> Nothing
            m:ms
              | Map.member name m -> Just depth
              | otherwise         -> loop (depth + 1) ms

lookupName :: Int -> UnresolvedName -> Resolve ResolvedName
lookupName line (UnresolvedName name) = do
    env <- getEnv
    case env of
      [] -> pure $ Global name
      m:_ -> do
          when (Map.lookup name m == Just False) $
            diag (mkDiagnostic line "" "Can't read local variable in its own initializer")
          pure $ maybe (Global name) (flip Local name) (countDepth name env)

resolveFun :: FunctionType -> Int -> (Maybe String) -> [String]
           -> [Stmt UnresolvedName] -> Resolve [Stmt ResolvedName]
resolveFun ty line name params body = do
    traverse_ (declare line) name
    traverse_ define name
    enterScope
    traverse_ define params
    body' <- modifyContext (\ctx -> ctx { funType = ty, inLoop = False }) $
        traverse resolveStmt body
    exitScope
    pure $ body'

resolveStmt :: Stmt UnresolvedName -> Resolve (Stmt ResolvedName)
resolveStmt = \case
    ExprStmt l e    -> ExprStmt l <$> resolveExpr e
    PrintStmt l e   -> PrintStmt l <$> resolveExpr e
    Block l s       -> do
        enterScope
        s' <- traverse resolveStmt s
        exitScope
        pure $ Block l s'
    IfStmt l c t e  -> IfStmt l <$> resolveExpr c <*> resolveStmt t <*> traverse resolveStmt e
    WhileStmt l c b -> WhileStmt l <$> resolveExpr c <*> withinLoop (resolveStmt b)
    BreakStmt l     -> do
        loop <- readContext inLoop
        when (not loop) $
            diag (mkDiagnostic l "" "break outside loop")
        pure $ BreakStmt l
    ContinueStmt l  -> do
        loop <- readContext inLoop
        when (not loop) $
            diag (mkDiagnostic l "" "continue outside loop")
        pure $ ContinueStmt l
    ReturnStmt l e  -> do
        f <- readContext funType
        when (f == NoFunction) $
            diag (mkDiagnostic l "" "Return outside function")
        ReturnStmt l <$> traverse resolveExpr e
    VarDecl l n e   -> do
        declare l n
        e' <- traverse resolveExpr e
        define n
        pure $ VarDecl l n e'
    FunDecl l n p b -> FunDecl l n p <$> resolveFun Function l (Just n) p b

resolveExpr :: Expr UnresolvedName -> Resolve (Expr ResolvedName)
resolveExpr = \case
    Binary l a o b  -> Binary l <$> resolveExpr a <*> pure o <*> resolveExpr b
    Logical l a o b -> Logical l <$> resolveExpr a <*> pure o <*> resolveExpr b
    Grouping l e    -> Grouping l <$> resolveExpr e
    Literal l v     -> pure $ Literal l v
    Unary l o e     -> Unary l o <$> resolveExpr e
    Variable l n    -> Variable l <$> lookupName l n
    Assign l n e    -> Assign l <$> lookupName l n <*> resolveExpr e
    Call l c a      -> Call l <$> resolveExpr c <*> traverse resolveExpr a
    Fun l p b       -> Fun l p <$> resolveFun Function l Nothing p b
