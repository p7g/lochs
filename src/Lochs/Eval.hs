module Lochs.Eval (Env, mkEnv, exec) where

import Control.Monad (ap)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

import Lochs.AST
import Lochs.Diagnostic
import Lochs.Runtime

exec :: Env -> [Stmt] -> IO (Either Diagnostic ())
exec env stmts = runEval (execProgram stmts) env

data Env = Env
    { values :: IORef (Map.Map String (IORef Value))
    , parent :: Maybe Env
    }

mkEnv :: IO Env
mkEnv = do
    ref <- newIORef Map.empty
    pure $ Env ref Nothing

newtype Eval a = Eval { runEval :: Env -> IO (Either Diagnostic a) }

instance Functor Eval where
    fmap f (Eval g) = Eval $ \env -> fmap (fmap f) (g env)

instance Applicative Eval where
    pure x = Eval $ \_ -> pure (Right x)
    (<*>) = ap

instance Monad Eval where
    Eval g >>= f = Eval $ \env ->
        g env >>= either (pure . Left) (\a -> runEval (f a) env)

liftIO' :: IO a -> Eval a
liftIO' io = Eval $ \_ -> Right <$> io

throwErr :: Diagnostic -> Eval a
throwErr d = Eval $ \_ -> pure (Left d)

getEnv :: Eval Env
getEnv = Eval $ \env -> pure (Right env)

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
execStmt (Block _line stmts) = newScope (execProgram stmts)

eval :: Expr -> Eval Value
eval = \case
    Literal  _line v      -> pure v
    Grouping _line e      -> eval e
    Unary    line  op e   -> do
        operand <- eval e
        unary line op operand
    Binary line l op r -> do
        lhs <- eval l
        rhs <- eval r
        binary line op lhs rhs
    Variable line name -> lookupVar line name
    Assign line name expr -> do
        val <- eval expr
        assignVar line name val
        pure val

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
