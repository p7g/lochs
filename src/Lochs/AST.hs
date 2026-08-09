module Lochs.AST
    ( AssignTarget(..)
    , BinaryOp(..)
    , LitValue(..)
    , LogicalOp(..)
    , Expr(..)
    , Stmt(..)
    , UnaryOp(..)
    ) where

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

data LogicalOp = LogicalAnd | LogicalOr
    deriving (Show)

data UnaryOp = UnaryNeg | UnaryNot
    deriving (Show)

data AssignTarget = ATVariable String
    deriving (Show)

data LitValue = LitBool Bool
              | LitNumber Double
              | LitString String
              | LitNil
              deriving (Show)

data Expr = Binary   { exprLine :: Int, lhs :: !Expr, binOp :: !BinaryOp, rhs :: !Expr }
          | Logical  { exprLine :: Int, lhs :: !Expr, logicalOp :: !LogicalOp, rhs :: !Expr }
          | Grouping { exprLine :: Int, expr :: !Expr }
          | Literal  { exprLine :: Int, value :: !LitValue }
          | Unary    { exprLine :: Int, unaryOp :: UnaryOp, expr :: !Expr }
          | Variable { exprLine :: Int, name :: !String }
          | Assign   { exprLine :: Int, target :: !String, expr :: !Expr }
          | Call     { exprLine :: Int, callee :: !Expr, arguments :: ![Expr] }
          deriving (Show)

data Stmt = ExprStmt     { stmtLine :: Int, stmtExpr :: !Expr }
          | PrintStmt    { stmtLine :: Int, stmtExpr :: !Expr }
          | VarDecl      { stmtLine :: Int, varName :: !String, init :: !(Maybe Expr) }
          | FunDecl      { stmtLine :: Int, funName :: !String, params :: ![String], funBody :: ![Stmt] }
          | Block        { stmtLine :: Int, stmts :: ![Stmt] }
          | IfStmt       { stmtLine :: Int, cond :: !Expr, cons :: !Stmt, alt :: !(Maybe Stmt) }
          | WhileStmt    { stmtLine :: Int, cond :: !Expr, body :: !Stmt }
          | BreakStmt    { stmtLine :: Int }
          | ContinueStmt { stmtLine :: Int }
          | ReturnStmt   { stmtLine :: Int, returnExpr :: !(Maybe Expr) }
          deriving (Show)
