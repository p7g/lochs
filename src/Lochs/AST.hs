module Lochs.AST (BinaryOp(..), Expr(..), Stmt(..), UnaryOp(..)) where

import Lochs.Runtime qualified as R

data BinaryOp = BinAdd
              | BinSub
              | BinMul
              | BinDiv
              | BinEq
              | BinNe
              | BinGt
              | BinGte
              | BinLt
              | BinLte
    deriving (Show)

data UnaryOp = UnaryNeg | UnaryNot
    deriving (Show)

data Expr = Binary   Int Expr BinaryOp Expr
          | Grouping Int Expr
          | Literal  Int R.Value
          | Unary    Int UnaryOp Expr
          deriving (Show)

data Stmt
