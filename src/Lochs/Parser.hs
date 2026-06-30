module Lochs.Parser (parse) where

import Data.Bifunctor (bimap)
import Data.List (singleton)

import Lochs.AST (BinaryOp(..), Expr(..), UnaryOp(..))
import Lochs.Diagnostic (Diagnostic, mkDiagnostic)
import Lochs.Runtime (Value(..))
import Lochs.Scanner (Token(..), TokenType(..))

parse :: [Token] -> Either [Diagnostic] Expr
parse = bimap singleton fst . runParser (requireEOF expression)

newtype Parser a = Parser { runParser :: [Token] -> Either Diagnostic (a, [Token]) }

instance Functor Parser where
    fmap f p = Parser $ \cs ->
        case runParser p cs of
          Left d         -> Left d
          Right (x, cs') -> Right (f x, cs')

instance Applicative Parser where
    pure x = Parser $ \cs -> Right (x, cs)
    pf <*> px = Parser $ \cs ->
        case runParser pf cs of
          Left d         -> Left d
          Right (f, cs') ->
              case runParser px cs' of
                Left d          -> Left d
                Right (x, cs'') -> Right (f x, cs'')

instance Monad Parser where
    p >>= f = Parser $ \cs ->
        case runParser p cs of
          Left d         -> Left d
          Right (a, cs') -> runParser (f a) cs'

parseError :: Int -> String -> Parser a
parseError line message = Parser $ \_ -> Left $ mkDiagnostic line "" message

unexpectedToken :: Token -> Either TokenType String -> Parser a
unexpectedToken got expected = parseError (line got) message
    where message = "Unexpected token, got " ++ show (ty got)
                    ++ " but expected " ++ expected'
          expected' = case expected of
            Left tt -> show tt
            Right s -> s

item :: Parser Token
item = Parser $ \case
    []   -> Left $ mkDiagnostic 0 "" "Unexpected end of file"
    c:cs -> Right (c, cs)

peek :: Parser (Maybe Token)
peek = Parser $ \case
    []   -> Right (Nothing, [])
    t:ts -> Right (Just t, t:ts)

token :: TokenType -> Parser Token
token tt = do
    tok <- peek
    case tok of
      Nothing -> parseError 0 ("Unexpected end of file, expected " ++ show tt)
      Just tok'
        | ty tok' == tt -> item
        | otherwise     -> unexpectedToken tok' (Left tt)

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
      | ty tok `elem` [Class, For, Fun, If, Print, Return, Var, While] ->
          pure ()
      | otherwise -> item >> synchronize

requireEOF :: Parser a -> Parser a
requireEOF p = do
    val <- p
    tok <- peek
    case tok of
      Just tok' -> unexpectedToken tok' (Right "end of file")
      Nothing -> pure val

expression :: Parser Expr
expression = equality

binop :: Token -> BinaryOp
binop tok = case ty tok of
    BangEqual  -> BinNe
    EqualEqual -> BinEq
    Minus      -> BinSub
    Plus       -> BinAdd
    Slash      -> BinDiv
    Star       -> BinMul
    _          -> undefined

binary :: [TokenType] -> Parser Expr -> Parser Expr
binary tts next = next >>= loop
    where loop lhs = match tts >>= \case
              Nothing -> pure lhs
              Just op' -> do
                rhs <- next
                let lhs' = Binary lhs (binop op') rhs
                loop lhs'

equality :: Parser Expr
equality = binary [EqualEqual, BangEqual] comparison

comparison :: Parser Expr
comparison = binary [Greater, GreaterEqual, Less, LessEqual] term

term :: Parser Expr
term = binary [Minus, Plus] factor

factor :: Parser Expr
factor = binary [Slash, Star] unary

unop :: Token -> UnaryOp
unop tok = case ty tok of
    Bang  -> UnaryNot
    Minus -> UnaryNeg
    _     -> undefined

unary :: Parser Expr
unary = do
    op <- match [Bang, Minus]
    case op of
        Nothing -> primary
        Just op' -> do
            rhs <- unary
            pure $ Unary (unop op') rhs

primary :: Parser Expr
primary = peek >>= \case
    Nothing -> parseError 0 "Unexpected end of file"
    Just tok -> case ty tok of
        True_     -> item >> pure (Literal (VBool True))
        False_    -> item >> pure (Literal (VBool False))
        Nil       -> item >> pure (Literal VNil)
        Number n  -> item >> pure (Literal (VNumber n))
        String_ s -> item >> pure (Literal (VString s))
        LeftParen -> do
            _ <- token LeftParen
            expr <- expression
            _ <- token RightParen
            pure $ Grouping expr
        _         -> unexpectedToken tok (Right "expression")
