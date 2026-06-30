module Lochs.Pretty (pretty) where

import Lochs.AST (BinaryOp(..), Expr(..), UnaryOp(..))
import Lochs.Runtime (stringify)

pretty :: Expr -> String
pretty = \case
    Binary l op r -> parenthesize (binop_pretty op) [l, r]
    Grouping expr -> parenthesize "group" [expr]
    Literal value -> stringify value
    Unary op expr -> parenthesize (unop_pretty op) [expr]

binop_pretty :: BinaryOp -> String
binop_pretty = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"

unop_pretty :: UnaryOp -> String
unop_pretty = \case
    Neg -> "-"
    Not -> "!"

parenthesize :: String -> [Expr] -> String
parenthesize name exprs = "(" ++ name ++ exprs_part ++ ")"
    where exprs_part = foldMap ((" "++) . pretty) exprs
