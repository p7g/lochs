module Lochs.AST (BinaryOp(..), Expr(..), Stmt(..), UnaryOp(..)) where

import Lochs.Runtime qualified as R

data BinaryOp = BinAdd
              | BinSub
              | BinMul
              | BinDiv
              | BinEq
              | BinNe
    deriving (Show)

data UnaryOp = UnaryNeg | UnaryNot
    deriving (Show)

data Expr = Binary Expr BinaryOp Expr
          | Grouping Expr
          | Literal R.Value
          | Unary UnaryOp Expr
          deriving (Show)

data Stmt
