module Lochs.AST
    ( AssignTarget(..)
    , BinaryOp(..)
    , Expr(..)
    , Stmt(..)
    , UnaryOp(..)
    ) where

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

data AssignTarget = ATVariable String
    deriving (Show)

data Expr = Binary   { exprLine :: Int, lhs :: !Expr, binOp :: !BinaryOp, rhs :: !Expr }
          | Grouping { exprLine :: Int, expr :: !Expr }
          | Literal  { exprLine :: Int, value :: !R.Value }
          | Unary    { exprLine :: Int, unaryOp :: UnaryOp, expr :: !Expr }
          | Variable { exprLine :: Int, name :: !String }
          | Assign   { exprLine :: Int, target :: !String, expr :: !Expr }
          deriving (Show)

data Stmt = ExprStmt  { stmtLine :: Int, stmtExpr :: !Expr }
          | PrintStmt { stmtLine :: Int, stmtExpr :: !Expr }
          | VarDecl   { stmtLine :: Int, varName :: !String, init :: !(Maybe Expr) }
          | Block     { stmtLine :: Int, stmts :: ![Stmt] }
          deriving (Show)
