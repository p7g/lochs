module Lochs.Eval (eval) where

import Lochs.AST
import Lochs.Runtime

eval :: Expr -> Value
eval = \case
    Literal  line v      -> v
    Grouping line e      -> eval e
    Unary    line op e   -> unary  line op $ eval e
    Binary   line l op r -> binary line op (eval l) (eval r)

unary :: Int -> UnaryOp -> Value -> Value
unary line UnaryNeg (VNumber n) = VNumber (-n)
unary line UnaryNot v           = VBool (not (isTruthy v))
unary _    _        _           = undefined  -- FIXME

binary :: Int -> BinaryOp -> Value -> Value -> Value
binary line BinSub (VNumber l) (VNumber r) = VNumber $ l - r
binary line BinDiv (VNumber l) (VNumber r) = VNumber $ l / r
binary line BinMul (VNumber l) (VNumber r) = VNumber $ l * r
binary line BinAdd (VNumber l) (VNumber r) = VNumber $ l + r
binary line BinAdd (VString l) (VString r) = VString $ l ++ r
binary line BinGt  (VNumber l) (VNumber r) = VBool   $ l > r
binary line BinGte (VNumber l) (VNumber r) = VBool   $ l >= r
binary line BinLt  (VNumber l) (VNumber r) = VBool   $ l < r
binary line BinLte (VNumber l) (VNumber r) = VBool   $ l <= r
binary line BinEq  l           r           = VBool   $ isEqual l r
binary line BinNe  l           r           = VBool   $ not (isEqual l r)
binary _    _      _           _           = undefined  -- FIXME
