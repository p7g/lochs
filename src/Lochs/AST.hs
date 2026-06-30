module Lochs.AST (BinaryOp(..), Expr(..), Stmt(..), UnaryOp(..)) where

import Lochs.Runtime qualified as R

data BinaryOp = Add | Sub | Mul | Div
    deriving (Show)

data UnaryOp = Neg | Not
    deriving (Show)

data Expr = Binary Expr BinaryOp Expr
          | Grouping Expr
          | Literal R.Value
          | Unary UnaryOp Expr

data Stmt
