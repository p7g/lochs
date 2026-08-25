{-# LANGUAGE Strict #-}

module Lochs.AST
    ( AssignTarget(..)
    , BinaryOp(..)
    , Expr(..)
    , LitValue(..)
    , LogicalOp(..)
    , ResolvedName(..)
    , Stmt(..)
    , UnaryOp(..)
    , UnresolvedName(..)
    , resolvedNameText
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

newtype UnresolvedName = UnresolvedName String deriving (Show)
data ResolvedName = Local Int Int String
                  | Global String
                  deriving (Show)

resolvedNameText :: ResolvedName -> String
resolvedNameText (Local _ _ n) = n
resolvedNameText (Global n)    = n

data Expr a
    = Binary   { exprLine :: Int, lhs :: Expr a, binOp :: BinaryOp, rhs :: Expr a }
    | Logical  { exprLine :: Int, lhs :: Expr a, logicalOp :: LogicalOp, rhs :: Expr a }
    | Grouping { exprLine :: Int, expr :: (Expr a) }
    | Literal  { exprLine :: Int, value :: LitValue }
    | Unary    { exprLine :: Int, unaryOp :: UnaryOp, expr :: Expr a }
    | Variable { exprLine :: Int, name :: a }
    | Assign   { exprLine :: Int, target :: a, expr :: Expr a }
    | Call     { exprLine :: Int, callee :: Expr a, arguments :: [Expr a] }
    | Fun      { exprLine :: Int, funExprParams :: [a], funExprBody :: [Stmt a], funExprVars :: Int }
    deriving (Show)

data Stmt a
    = ExprStmt     { stmtLine :: Int, stmtExpr :: Expr a }
    | PrintStmt    { stmtLine :: Int, stmtExpr :: Expr a }
    | VarDecl      { stmtLine :: Int, varName :: a, init :: Maybe (Expr a) }
    | FunDecl      { stmtLine :: Int, funName :: a, params :: [a], funBody :: [Stmt a], nVars :: Int }
    | Block        { stmtLine :: Int, stmts :: [Stmt a], nVars :: Int }
    | IfStmt       { stmtLine :: Int, cond :: Expr a, cons :: Stmt a, alt :: Maybe (Stmt a) }
    | WhileStmt    { stmtLine :: Int, cond :: Expr a, body :: Stmt a }
    | BreakStmt    { stmtLine :: Int }
    | ContinueStmt { stmtLine :: Int }
    | ReturnStmt   { stmtLine :: Int, returnExpr :: Maybe (Expr a) }
    deriving (Show)
