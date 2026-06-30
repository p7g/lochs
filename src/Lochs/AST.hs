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

data Expr = Binary   { exprLine :: Int, lhs :: Expr, binOp :: BinaryOp, rhs :: Expr }
          | Grouping { exprLine :: Int, expr :: Expr }
          | Literal  { exprLine :: Int, value :: R.Value }
          | Unary    { exprLine :: Int, unaryOp :: UnaryOp, expr :: Expr }
          deriving (Show)

data Stmt = ExprStmt  Int Expr
          | PrintStmt Int Expr
          deriving (Show)
