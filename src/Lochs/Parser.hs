module Lochs.Parser (ParseResult(..), parse) where

import Control.Monad (ap)

import Lochs.AST
import Lochs.Diagnostic hiding (line)
import Lochs.Runtime (Value(..))
import Lochs.Scanner

data ParseResult a = Failure   [Diagnostic] [Token]
                   | Success a [Diagnostic] [Token]

parse :: [Token] -> ([Stmt], [Diagnostic])
parse ts = case runParser program ts [] of
    Success stmts ds _ -> (stmts, ds)
    Failure ds _       -> ([], ds)

newtype Parser a = Parser
    { runParser :: [Token] -> [Diagnostic] -> ParseResult a }

instance Functor Parser where
    fmap f p = Parser $ \cs ds ->
        case runParser p cs ds of
          Failure   ds' cs' -> Failure ds' cs'
          Success x ds' cs' -> Success (f x) ds' cs'

instance Applicative Parser where
    pure x = Parser $ \cs ds -> Success x ds cs
    (<*>) = ap

instance Monad Parser where
    p >>= f = Parser $ \cs ds ->
        case runParser p cs ds of
          Failure   ds' cs' -> Failure ds' cs'
          Success a ds' cs' -> runParser (f a) cs' ds'

instance MonadFail Parser where
    fail s = parseError 0 "" ("Internal parser error: " ++ s)

catchError :: Parser a -> Parser a -> Parser a
p `catchError` recovery = Parser $ \cs ds ->
    case runParser p cs ds of
      Failure ds' cs' -> runParser recovery cs' ds'
      success         -> success

parseError :: Int -> String -> String -> Parser a
parseError line loc message = Parser $ \cs ds ->
    Failure (ds ++ [mkDiagnostic line loc message]) cs

unexpectedToken :: Token -> Either TokenType String -> Parser a
unexpectedToken got expected = parseError (line got) loc message
    where message = "Unexpected token, got " ++ show (ty got)
                    ++ " but expected " ++ expected'
          loc = " at " ++ show (lexeme got)
          expected' = case expected of
            Left tt -> show tt
            Right s -> s

item :: Parser Token
item = Parser $ \cs ds -> case cs of
    []   -> Failure (ds ++ [mkDiagnostic 0 " at end" "Expected token"]) []
    c:cs' -> Success c ds cs'

peek :: Parser (Maybe Token)
peek = Parser $ \cs ds -> case cs of
    []  -> Success Nothing  ds []
    t:_ -> Success (Just t) ds cs

token :: TokenType -> Parser Token
token tt = do
    tok <- peek
    case tok of
      Nothing -> parseError 0 " at end" ("Expected " ++ show tt)
      Just tok'
        | ty tok' == tt -> item
        | otherwise     -> unexpectedToken tok' (Left tt)

filterMap :: String -> (Token -> Maybe a) -> Parser a
filterMap diagMsg predicate = do
    tok <- peek
    case tok of
      Nothing -> parseError 0 " at end" diagMsg
      Just tok' ->
          case predicate tok' of
            Nothing -> unexpectedToken tok' (Right diagMsg)
            Just x  -> item >> pure x

match :: [TokenType] -> Parser (Maybe Token)
match tts = do
    tok <- peek
    case tok of
      Nothing -> pure Nothing
      Just tok'
        | ty tok' `elem` tts -> Just <$> item
        | otherwise          -> pure Nothing

synchronize :: Parser ()
synchronize = peek >>= \case
    Nothing -> pure ()
    Just tok
      | ty tok `elem` [TClass, TFor, TFun, TIf, TPrint, TReturn, TVar, TWhile] ->
          pure ()
      | otherwise -> item >> synchronize

program :: Parser [Stmt]
program = loop []
    where loop stmts = do
            stmt <- declaration
            eofTok <- match [TEOF]
            let stmts' = case stmt of
                           Just stmt' -> stmt':stmts
                           Nothing    -> stmts
            case eofTok of
              Just _ -> pure $ reverse stmts'
              Nothing -> loop stmts'

declaration :: Parser (Maybe Stmt)
declaration = tryDecl `catchError` (synchronize >> pure Nothing)
    where tryDecl = do
            Just tok <- peek
            case ty tok of
                TVar -> Just <$> varDecl
                _    -> Just <$> statement

varDecl :: Parser Stmt
varDecl = do
    tok <- token TVar
    name <- filterMap "identifier" $ \tok' ->
        case ty tok' of
          TIdentifier ident -> Just ident
          _                 -> Nothing
    eq <- match [TEqual]
    expr <- case eq of
      Nothing -> pure Nothing
      Just _  -> fmap Just expression
    _ <- token TSemicolon
    pure $ VarDecl (line tok) name expr

statement :: Parser Stmt
statement = do
    Just tok <- peek
    case ty tok of
      TPrint     -> printStatement
      TLeftBrace -> block
      _          -> exprStatement

printStatement :: Parser Stmt
printStatement = do
    tok <- token TPrint
    expr <- expression
    _ <- token TSemicolon
    pure $ PrintStmt (line tok) expr

block :: Parser Stmt
block = token TLeftBrace >> loop []
    where loop stmts = do
            Just tok' <- peek
            case ty tok' of
              TRightBrace -> item >> pure (Block (line tok') (reverse stmts))
              _           -> do
                  stmt <- declaration
                  case stmt of
                    Just stmt' -> loop (stmt':stmts)
                    Nothing    -> loop stmts

exprStatement :: Parser Stmt
exprStatement = do
    expr <- expression
    _ <- token TSemicolon
    pure $ ExprStmt (exprLine expr) expr

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment = do
    lhs <- equality
    equals <- match [TEqual]
    case equals of
      Nothing -> pure lhs
      Just _  -> do
          value <- assignment
          case lhs of
            Variable l n -> pure $ Assign l n value
            _            ->
                parseError (exprLine lhs) "" ("Can't assign to " ++ show lhs)

binop :: Token -> BinaryOp
binop tok = case ty tok of
    TBangEqual  -> BinNe
    TEqualEqual -> BinEq
    TMinus      -> BinSub
    TPlus       -> BinAdd
    TSlash      -> BinDiv
    TStar       -> BinMul
    _           -> undefined

binary :: [TokenType] -> Parser Expr -> Parser Expr
binary tts next = next >>= loop
    where loop lhs = match tts >>= \case
              Nothing -> pure lhs
              Just op' -> do
                rhs <- next
                let lhs' = Binary (line op') lhs (binop op') rhs
                loop lhs'

equality :: Parser Expr
equality = binary [TEqualEqual, TBangEqual] comparison

comparison :: Parser Expr
comparison = binary [TGreater, TGreaterEqual, TLess, TLessEqual] term

term :: Parser Expr
term = binary [TMinus, TPlus] factor

factor :: Parser Expr
factor = binary [TSlash, TStar] unary

unop :: Token -> UnaryOp
unop tok = case ty tok of
    TBang  -> UnaryNot
    TMinus -> UnaryNeg
    _      -> undefined

unary :: Parser Expr
unary = do
    op <- match [TBang, TMinus]
    case op of
        Nothing -> primary
        Just op' -> do
            rhs <- unary
            pure $ Unary (line op') (unop op') rhs

primary :: Parser Expr
primary = peek >>= \case
    Nothing -> parseError 0 " at end" "Expected token"
    Just tok -> case ty tok of
        TTrue         -> item >> pure (Literal l (VBool True))
        TFalse        -> item >> pure (Literal l (VBool False))
        TNil          -> item >> pure (Literal l VNil)
        TNumber n     -> item >> pure (Literal l (VNumber n))
        TString s     -> item >> pure (Literal l (VString s))
        TIdentifier i -> item >> pure (Variable l i)
        TLeftParen    -> do
            _ <- token TLeftParen
            expr <- expression
            _ <- token TRightParen
            pure $ Grouping l expr
        _          -> unexpectedToken tok (Right "expression")
      where l = line tok
