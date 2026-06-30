module Lochs.Eval (exec) where

import Control.Monad (ap)

import Lochs.AST
import Lochs.Diagnostic
import Lochs.Runtime

exec :: [Stmt] -> IO (Either Diagnostic ())
exec stmts = runEval (execProgram stmts)

newtype Eval a = Eval { runEval :: IO (Either Diagnostic a) }

instance Functor Eval where
    fmap f (Eval io) = Eval $ fmap (fmap f) io

instance Applicative Eval where
    pure = Eval . pure . Right
    (<*>) = ap

instance Monad Eval where
    Eval io >>= f = Eval $ io >>= either (pure . Left) (runEval . f)

liftIO' :: IO a -> Eval a
liftIO' io = Eval (Right <$> io)

throwErr :: Diagnostic -> Eval a
throwErr = Eval . pure . Left

execProgram :: [Stmt] -> Eval ()
execProgram []     = pure ()
execProgram (x:xs) = execOne x >> execProgram xs

execOne :: Stmt -> Eval ()
execOne (PrintStmt _line expr) = do
    val <- eval expr
    liftIO' $ putStrLn (stringify val)
execOne (ExprStmt  _line expr) = eval expr >> pure ()

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
