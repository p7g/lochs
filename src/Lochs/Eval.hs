module Lochs.Eval (eval) where

import Lochs.AST
import Lochs.Diagnostic
import Lochs.Runtime

type EvalResult a = Either Diagnostic a

eval :: Expr -> EvalResult Value
eval = \case
    Literal  _line v      -> Right v
    Grouping _line e      -> eval e
    Unary    line  op e   -> do
        operand <- eval e
        unary  line op operand
    Binary line l op r -> do
        lhs <- eval l
        rhs <- eval r
        binary line op lhs rhs

typeError :: Int -> Value -> String -> EvalResult a
typeError line val expected = Left $
    mkDiagnostic line "" ("Expected " ++ expected ++ " but got " ++ typeName val)

unary :: Int -> UnaryOp -> Value -> EvalResult Value
unary _line UnaryNeg (VNumber n) = Right $ VNumber (-n)
unary  line UnaryNeg v           = typeError line v "number"
unary _line UnaryNot v           = Right $ VBool (not (isTruthy v))

binary :: Int -> BinaryOp -> Value -> Value -> EvalResult Value
binary _line BinSub (VNumber l) (VNumber r) = Right $ VNumber (l - r)
binary  line BinSub (VNumber _) r           = typeError line r "number"
binary  line BinSub l           (VNumber _) = typeError line l "number"
binary  line BinSub l           _           = typeError line l "number"

binary _line BinDiv (VNumber l) (VNumber r) = Right $ VNumber (l / r)
binary  line BinDiv (VNumber _) r           = typeError line r "number"
binary  line BinDiv l           (VNumber _) = typeError line l "number"
binary  line BinDiv l           _           = typeError line l "number"

binary _line BinMul (VNumber l) (VNumber r) = Right $ VNumber (l * r)
binary  line BinMul (VNumber _) r           = typeError line r "number"
binary  line BinMul l           (VNumber _) = typeError line l "number"
binary  line BinMul l           _           = typeError line l "number"

binary _line BinAdd (VNumber l) (VNumber r) = Right $ VNumber (l + r)
binary  line BinAdd (VNumber _) r           = typeError line r "number"
binary  line BinAdd l           (VNumber _) = typeError line l "number"
binary _line BinAdd (VString l) (VString r) = Right $ VString (l ++ r)
binary  line BinAdd (VString _) r           = typeError line r "string"
binary  line BinAdd l           (VString _) = typeError line l "string"
binary  line BinAdd l           _           = typeError line l "number or string"

binary _line BinGt  (VNumber l) (VNumber r) = Right $ VBool   (l > r)
binary  line BinGt  (VNumber _) r           = typeError line r "number"
binary  line BinGt  l           (VNumber _) = typeError line l "number"
binary  line BinGt  l           _           = typeError line l "number"
binary _line BinGte (VNumber l) (VNumber r) = Right $ VBool   (l >= r)
binary  line BinGte (VNumber _) r           = typeError line r "number"
binary  line BinGte l           (VNumber _) = typeError line l "number"
binary  line BinGte l           _           = typeError line l "number"
binary _line BinLt  (VNumber l) (VNumber r) = Right $ VBool   (l < r)
binary  line BinLt  (VNumber _) r           = typeError line r "number"
binary  line BinLt  l           (VNumber _) = typeError line l "number"
binary  line BinLt  l           _           = typeError line l "number"
binary _line BinLte (VNumber l) (VNumber r) = Right $ VBool   (l <= r)
binary  line BinLte (VNumber _) r           = typeError line r "number"
binary  line BinLte l           (VNumber _) = typeError line l "number"
binary  line BinLte l           _           = typeError line l "number"

binary _line BinEq  l           r           = Right $ VBool   (isEqual l r)
binary _line BinNe  l           r           = Right $ VBool   (not (isEqual l r))
