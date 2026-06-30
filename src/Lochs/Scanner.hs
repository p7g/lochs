module Lochs.Scanner (Token(..), TokenType(..), scan) where

import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit)
import Data.Maybe (fromMaybe)

import Lochs.Diagnostic (Diagnostic, mkDiagnostic)

data TokenType
    -- Single-character tokens
    = LeftParen | RightParen | LeftBrace | RightBrace
    | Comma | Dot | Minus | Plus | Semicolon | Slash | Star

    -- One- or two-character tokens
    | Bang | BangEqual | Equal | EqualEqual
    | Greater | GreaterEqual | Less | LessEqual

    -- Literals
    | Identifier String | String_ String | Number Double

    -- Keywords
    | And | Class | Else | False_ | Fun | For | If | Nil | Or
    | Print | Return | Super | This | True_ | Var | While

    | Eof
    deriving (Eq, Show)

data Token = Token { ty :: TokenType
                   , lexeme :: String
                   , line :: Int
                   }

instance Show Token where
    show t = (show $ ty t) ++ " " ++ (lexeme t) ++ " " ++ (fromMaybe "" (show_literal $ ty t))
        where
            show_literal (Identifier s) = Just s
            show_literal (String_ s) = Just s
            show_literal (Number d) = Just $ show d
            show_literal _ = Nothing

keyword :: String -> Maybe TokenType
keyword = \case
    "and"    -> Just And
    "class"  -> Just Class
    "else"   -> Just Else
    "false"  -> Just False_
    "for"    -> Just For
    "Fun"    -> Just Fun
    "if"     -> Just If
    "nil"    -> Just Nil
    "or"     -> Just Or
    "print"  -> Just Print
    "return" -> Just Return
    "super"  -> Just Super
    "this"   -> Just This
    "true"   -> Just True_
    "var"    -> Just Var
    "while"  -> Just While
    _        -> Nothing

scan :: String -> ([Token], [Diagnostic])
scan s =
    let (tokens, diags) = scanInner 1 [] [] s
     in (reverse tokens, reverse diags)
    where
        scanInner :: Int -> [Token] -> [Diagnostic] -> String -> ([Token], [Diagnostic])
        scanInner line tokens diags = \case
            "" -> (tokens, diags)
            '\n':rest    -> scanInner (line + 1) tokens diags rest
            '\t':rest    -> skip rest
            '\r':rest    -> skip rest
            ' ':rest     -> skip rest
            '(':rest     -> emit LeftParen "(" rest
            ')':rest     -> emit RightParen ")" rest
            '{':rest     -> emit LeftBrace "{" rest
            '}':rest     -> emit RightBrace "}" rest
            ',':rest     -> emit Comma "," rest
            '.':rest     -> emit Dot "." rest
            '-':rest     -> emit Minus "-" rest
            '+':rest     -> emit Plus "+" rest
            ';':rest     -> emit Semicolon ";" rest
            '*':rest     -> emit Star "*" rest
            '!':'=':rest -> emit BangEqual "!=" rest
            '!':rest     -> emit Bang "!" rest
            '=':'=':rest -> emit EqualEqual "==" rest
            '=':rest     -> emit Equal "=" rest
            '<':'=':rest -> emit LessEqual "<=" rest
            '<':rest     -> emit Less "<" rest
            '>':'=':rest -> emit GreaterEqual ">=" rest
            '>':rest     -> emit Greater ">" rest
            '/':'/':rest -> lineComment rest
            '/':rest     -> emit Slash "/" rest
            '"':rest     -> string line "" rest

            c:rest | isDigit c -> number "" False (c:rest)
            c:rest | isIdentStart c -> identifier [c] rest

            c:rest   -> diag line ("Unexpected token " ++ show c) rest

            where
                next = scanInner line
                skip = next tokens diags
                mkToken ty lexeme = Token ty lexeme line
                emitLine line' ty lexeme = scanInner line' (mkToken ty lexeme:tokens) diags
                emit = emitLine line
                diag line' msg = next tokens (mkDiagnostic line' "" msg:diags)
                lineComment = \case
                    '\n':rest -> scanInner (line + 1) tokens diags rest
                    _:rest    -> lineComment rest
                    ""        -> next tokens diags ""

                string line' buf ('"':rest) =
                    let buf' = reverse buf
                     in emitLine line' (String_ buf') (('"':buf') ++ "\"") rest
                string line' buf ('\n':rest) = string (line' + 1) ('\n':buf) rest
                string line' buf (c:rest) = string line' (c:buf) rest
                string line' _buf "" = diag line' "Unterminated string literal" ""

                number buf seenDot = \case
                    '.':rest | not seenDot -> number ('.':buf) True rest
                    c:rest | isDigit c -> number (c:buf) seenDot rest
                    rest ->
                        let buf' = reverse buf
                         in emit (Number $ read $ buf') buf' rest

                isIdentStart c = isAscii c && (isAlpha c || c == '_')
                isIdent c = isAscii c && (isAlphaNum c || c == '_')

                identifier buf = \case
                    c:rest | isIdent c -> identifier (c:buf) rest
                    rest ->
                        let buf' = reverse buf
                            tt = fromMaybe (Identifier buf') $ keyword buf'
                         in emit tt buf' rest
