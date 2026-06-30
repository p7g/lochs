module Lochs.Scanner (Token(..), TokenType(..), scan) where

import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit)
import Data.Maybe (fromMaybe)

import Lochs.Diagnostic (Diagnostic, mkDiagnostic)

data TokenType
    -- Single-character tokens
    = TLeftParen | TRightParen | TLeftBrace | TRightBrace
    | TComma | TDot | TMinus | TPlus | TSemicolon | TSlash | TStar

    -- One- or two-character tokens
    | TBang | TBangEqual | TEqual | TEqualEqual
    | TGreater | TGreaterEqual | TLess | TLessEqual

    -- Literals
    | TIdentifier String | TString String | TNumber Double

    -- Keywords
    | TAnd | TClass | TElse | TFalse | TFun | TFor | TIf | TNil | TOr
    | TPrint | TReturn | TSuper | TThis | TTrue | TVar | TWhile

    | TEOF
    deriving (Eq, Show)

data Token = Token { ty     :: TokenType
                   , lexeme :: String
                   , line   :: Int
                   }

instance Show Token where
    show t = (show $ ty t) ++ " " ++ (lexeme t) ++ " " ++ (fromMaybe "" (show_literal $ ty t))
        where
            show_literal (TIdentifier s) = Just s
            show_literal (TString s) = Just s
            show_literal (TNumber d) = Just $ show d
            show_literal _ = Nothing

keyword :: String -> Maybe TokenType
keyword = \case
    "and"    -> Just TAnd
    "class"  -> Just TClass
    "else"   -> Just TElse
    "false"  -> Just TFalse
    "for"    -> Just TFor
    "Fun"    -> Just TFun
    "if"     -> Just TIf
    "nil"    -> Just TNil
    "or"     -> Just TOr
    "print"  -> Just TPrint
    "return" -> Just TReturn
    "super"  -> Just TSuper
    "this"   -> Just TThis
    "true"   -> Just TTrue
    "var"    -> Just TVar
    "while"  -> Just TWhile
    _        -> Nothing

scan :: String -> ([Token], [Diagnostic])
scan s =
    let (tokens, diags) = scanInner 1 [] [] s
     in (reverse tokens, reverse diags)
    where
        scanInner :: Int -> [Token] -> [Diagnostic] -> String -> ([Token], [Diagnostic])
        scanInner line tokens diags = \case
            "" -> (mkToken TEOF "" : tokens, diags)
            '\n':rest    -> scanInner (line + 1) tokens diags rest
            '\t':rest    -> skip rest
            '\r':rest    -> skip rest
            ' ':rest     -> skip rest
            '(':rest     -> emit TLeftParen "(" rest
            ')':rest     -> emit TRightParen ")" rest
            '{':rest     -> emit TLeftBrace "{" rest
            '}':rest     -> emit TRightBrace "}" rest
            ',':rest     -> emit TComma "," rest
            '.':rest     -> emit TDot "." rest
            '-':rest     -> emit TMinus "-" rest
            '+':rest     -> emit TPlus "+" rest
            ';':rest     -> emit TSemicolon ";" rest
            '*':rest     -> emit TStar "*" rest
            '!':'=':rest -> emit TBangEqual "!=" rest
            '!':rest     -> emit TBang "!" rest
            '=':'=':rest -> emit TEqualEqual "==" rest
            '=':rest     -> emit TEqual "=" rest
            '<':'=':rest -> emit TLessEqual "<=" rest
            '<':rest     -> emit TLess "<" rest
            '>':'=':rest -> emit TGreaterEqual ">=" rest
            '>':rest     -> emit TGreater ">" rest
            '/':'/':rest -> lineComment rest
            '/':rest     -> emit TSlash "/" rest
            '"':rest     -> string line "" rest

            c:rest | isDigit c -> number "" False (c:rest)
            c:rest | isIdentStart c -> identifier [c] rest

            c:rest -> diag line ("Unexpected token " ++ show c) rest

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
                     in emitLine line' (TString buf') (('"':buf') ++ "\"") rest
                string line' buf ('\n':rest) = string (line' + 1) ('\n':buf) rest
                string line' buf (c:rest) = string line' (c:buf) rest
                string line' _buf "" = diag line' "Unterminated string literal" ""

                number buf seenDot = \case
                    '.':rest | not seenDot -> number ('.':buf) True rest
                    c:rest | isDigit c -> number (c:buf) seenDot rest
                    rest ->
                        let buf' = reverse buf
                         in emit (TNumber $ read $ buf') buf' rest

                isIdentStart c = isAscii c && (isAlpha c || c == '_')
                isIdent c = isAscii c && (isAlphaNum c || c == '_')

                identifier buf = \case
                    c:rest | isIdent c -> identifier (c:buf) rest
                    rest ->
                        let buf' = reverse buf
                            tt = fromMaybe (TIdentifier buf') $ keyword buf'
                         in emit tt buf' rest
