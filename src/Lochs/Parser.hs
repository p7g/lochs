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

parseError :: Int -> String -> String -> Parser a
parseError line loc message = Parser $ \_ -> Left $ mkDiagnostic line loc message

unexpectedToken :: Token -> Either TokenType String -> Parser a
unexpectedToken got expected = parseError (line got) loc message
    where message = "Unexpected token, got " ++ show (ty got)
                    ++ " but expected " ++ expected'
          loc = " at " ++ show (lexeme got)
          expected' = case expected of
            Left tt -> show tt
            Right s -> s

item :: Parser Token
item = Parser $ \case
    []   -> Left $ mkDiagnostic 0 " at end" "Expected token"
    c:cs -> Right (c, cs)

peek :: Parser (Maybe Token)
peek = Parser $ \case
    []   -> Right (Nothing, [])
    t:ts -> Right (Just t, t:ts)

token :: TokenType -> Parser Token
token tt = do
    tok <- peek
    case tok of
      Nothing -> parseError 0 " at end" ("Expected " ++ show tt)
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
      | ty tok `elem` [TClass, TFor, TFun, TIf, TPrint, TReturn, TVar, TWhile] ->
          pure ()
      | otherwise -> item >> synchronize

requireEOF :: Parser a -> Parser a
requireEOF p = do
    val <- p
    _ <- token TEOF
    pure val

expression :: Parser Expr
expression = equality

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
        TTrue      -> item >> pure (Literal l (VBool True))
        TFalse     -> item >> pure (Literal l (VBool False))
        TNil       -> item >> pure (Literal l VNil)
        TNumber n  -> item >> pure (Literal l (VNumber n))
        TString s  -> item >> pure (Literal l (VString s))
        TLeftParen -> do
            _ <- token TLeftParen
            expr <- expression
            _ <- token TRightParen
            pure $ Grouping l expr
        _          -> unexpectedToken tok (Right "expression")
      where l = line tok
