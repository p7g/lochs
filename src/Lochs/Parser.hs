module Lochs.Parser (ParseResult(..), parse) where

import Control.Monad (ap, guard, when)

import Lochs.AST hiding (Stmt, Expr)
import Lochs.AST qualified as AST
import Lochs.Diagnostic hiding (line)
import Lochs.Scanner

type Stmt = AST.Stmt UnresolvedName
type Expr = AST.Expr UnresolvedName

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

addDiagnostic :: Int -> String -> String -> Parser ()
addDiagnostic line loc message = Parser $ \cs ds -> Success () (ds ++ [diag]) cs
    where diag = mkDiagnostic line loc message

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

identifier :: String -> Parser String
identifier msg = filterMap msg $ \tok ->
    case ty tok of
      TIdentifier ident -> Just ident
      _                 -> Nothing

matchWith :: (Token -> Maybe a) -> Parser (Maybe (Token, a))
matchWith f = do
    maybeTok <- peek
    case maybeTok >>= \tok -> (,) tok <$> f tok of
      Nothing    -> pure Nothing
      r@(Just _) -> item >> pure r

match :: [TokenType] -> Parser (Maybe Token)
match tts = fmap (fmap fst) $ matchWith (guard . (`elem` tts) . ty)

synchronize :: Parser ()
synchronize = peek >>= \case
    Nothing -> pure ()
    Just tok
      | ty tok `elem` [TClass, TFor, TFun, TIf, TPrint, TReturn, TVar, TWhile, TEOF] ->
          pure ()
      | otherwise -> item >> synchronize

program :: Parser [Stmt]
program = loop []
    where loop stmts = do
            stmt <- declaration
            eofTok <- match [TEOF]
            let stmts' = maybe stmts (:stmts) stmt
            case eofTok of
              Just _ -> pure $ reverse stmts'
              Nothing -> loop stmts'

declaration :: Parser (Maybe Stmt)
declaration = tryDecl `catchError` (synchronize >> pure Nothing)
    where tryDecl = do
            Just tok <- peek
            case ty tok of
                TVar -> Just <$> varDecl
                TFun -> Just <$> funDecl "function"
                _    -> Just <$> statement

funParams :: Parser [String]
funParams = token TLeftParen >> loop []
    where loop acc = do
            maybeTok <- peek
            case maybeTok of
              Nothing -> parseError 0 " at end" "Expected right paren"
              Just tok
                | ty tok == TRightParen -> item >> pure (reverse acc)
                | otherwise -> do
                  when (length acc >= 255) $ do
                      addDiagnostic (line tok) "" "Can't have more than 255 parameters"
                  name <- identifier "identifier"
                  comma <- match [TComma]
                  let acc' = name : acc
                  case comma of
                    Just _ -> loop acc'
                    Nothing -> token TRightParen >> pure (reverse acc')

funDecl :: String -> Parser Stmt
funDecl kind = do
    tok <- token TFun
    name <- identifier (kind ++ " name")
    params <- funParams
    (_, body) <- block
    pure $ FunDecl (line tok) name params body

varDecl :: Parser Stmt
varDecl = do
    tok <- token TVar
    name <- identifier "identifier"
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
      TLeftBrace -> uncurry Block <$> block
      TIf        -> ifStatement
      TWhile     -> whileStatement
      TFor       -> forStatement
      TBreak     -> breakStatement
      TContinue  -> continueStatement
      TReturn    -> returnStatement
      _          -> exprStatement

printStatement :: Parser Stmt
printStatement = do
    tok <- token TPrint
    expr <- expression
    _ <- token TSemicolon
    pure $ PrintStmt (line tok) expr

block :: Parser (Int, [Stmt])
block = token TLeftBrace >> loop []
    where loop stmts = do
            Just tok' <- peek
            case ty tok' of
              TRightBrace -> item >> pure ((line tok'), (reverse stmts))
              _           -> do
                  stmt <- declaration
                  case stmt of
                    Just stmt' -> loop (stmt':stmts)
                    Nothing    -> do
                        p <- peek
                        case p of
                          Nothing -> pure $ ((line tok'), (reverse stmts))
                          Just tok
                            | ty tok == TEOF -> pure $ ((line tok'), (reverse stmts))
                            | otherwise      -> loop stmts

ifStatement :: Parser Stmt
ifStatement = do
    iftok <- token TIf
    _ <- token TLeftParen
    cond <- expression
    _ <- token TRightParen
    body <- statement
    elsetok <- match [TElse]
    elsebody <- traverse (const statement) elsetok
    pure $ IfStmt (line iftok) cond body elsebody

whileStatement :: Parser Stmt
whileStatement = do
    whiletok <- token TWhile
    _ <- token TLeftParen
    cond <- expression
    _ <- token TRightParen
    body <- statement
    pure $ WhileStmt (line whiletok) cond body

desugarContinue :: Expr -> Stmt -> Stmt
desugarContinue incr stmt = case stmt of
    ContinueStmt line -> Block line [ExprStmt (exprLine incr) incr, stmt]
    Block line stmts  -> Block line (map (desugarContinue incr) stmts)
    IfStmt line cond cons alt ->
        IfStmt line cond (desugarContinue incr cons) (fmap (desugarContinue incr) alt)
    WhileStmt _ _ _   -> stmt
    BreakStmt _       -> stmt
    ReturnStmt _ _    -> stmt
    PrintStmt _ _     -> stmt
    ExprStmt _ _      -> stmt
    VarDecl _ _ _     -> stmt
    FunDecl _ _ _ _   -> stmt

forStatement :: Parser Stmt
forStatement = do
    fortok <- token TFor
    _ <- token TLeftParen

    initializer <- peek >>= \case
        Nothing -> pure Nothing
        Just tok -> case ty tok of
            TVar       -> Just <$> varDecl
            TSemicolon -> pure Nothing
            _          -> Just <$> exprStatement

    cond <- match [TSemicolon] >>= \case
        Just tok -> pure $ Literal (line tok) (LitBool True)
        Nothing  -> do
            expr <- expression
            _ <- token TSemicolon
            pure expr

    inc <- match [TLeftParen] >>= \case
        Just _ -> pure Nothing
        _      -> Just <$> expression

    _ <- token TRightParen
    body <- statement >>= \s ->
        case inc of
          Just i -> pure $ Block (stmtLine s) [desugarContinue i s, ExprStmt (exprLine i) i]
          Nothing -> pure s

    let w = WhileStmt (line fortok) cond body
    pure $ case initializer of
      Just i  -> Block (line fortok) [i, w]
      Nothing -> w

breakStatement :: Parser Stmt
breakStatement = do
    tok <- token TBreak
    _ <- token TSemicolon
    pure $ BreakStmt (line tok)

continueStatement :: Parser Stmt
continueStatement = do
    tok <- token TContinue
    _ <- token TSemicolon
    pure $ ContinueStmt (line tok)

returnStatement :: Parser Stmt
returnStatement = do
    tok <- token TReturn
    s <- match [TSemicolon]
    case s of
      Just _ -> pure $ ReturnStmt (line tok) Nothing
      _ -> do
          e <- expression
          _ <- token TSemicolon
          pure $ ReturnStmt (line tok) (Just e)

exprStatement :: Parser Stmt
exprStatement = do
    expr <- expression
    _ <- token TSemicolon
    pure $ ExprStmt (exprLine expr) expr

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment = do
    lhs <- logicalOr
    equals <- match [TEqual]
    case equals of
      Nothing -> pure lhs
      Just _  -> do
          value <- assignment
          case lhs of
            Variable l n -> pure $ Assign l n value
            _            -> do
                addDiagnostic (exprLine lhs) "" ("Can't assign to " ++ show lhs)
                pure $ Assign 0 (UnresolvedName "dummy") value

type BinaryLike op = Int -> Expr -> op -> Expr -> Expr

binary :: BinaryLike op -> Parser Expr -> (TokenType -> Maybe op) -> Parser Expr
binary mkExpr next tokToOp = next >>= loop
    where loop lhs = matchWith (tokToOp . ty) >>= \case
              Nothing        -> pure lhs
              Just (tok, op) -> do
                rhs <- next
                loop $ mkExpr (line tok) lhs op rhs

logicalOr :: Parser Expr
logicalOr = binary Logical logicalAnd $ \tt -> LogicalOr <$ guard (tt == TOr)

logicalAnd :: Parser Expr
logicalAnd = binary Logical equality $ \tt -> LogicalAnd <$ guard (tt == TAnd)

equality :: Parser Expr
equality = binary Binary comparison $ \case
      TEqualEqual -> Just BinEq
      TBangEqual -> Just BinNe
      _ -> Nothing

comparison :: Parser Expr
comparison = binary Binary term $ \case
    TGreater      -> Just BinGt
    TGreaterEqual -> Just BinGte
    TLess         -> Just BinLt
    TLessEqual    -> Just BinLte
    _             -> Nothing

term :: Parser Expr
term = binary Binary factor $ \case
    TMinus -> Just BinSub
    TPlus  -> Just BinAdd
    _      -> Nothing

factor :: Parser Expr
factor = binary Binary unary $ \case
    TSlash -> Just BinDiv
    TStar  -> Just BinMul
    _      -> Nothing

unary :: Parser Expr
unary = do
    m <- matchWith $ \tok ->
        case ty tok of
          TBang  -> Just UnaryNot
          TMinus -> Just UnaryNeg
          _      -> Nothing
    maybe call (\(tok, op) -> unary >>= pure . Unary (line tok) op) m

call :: Parser Expr
call = primary >>= oneCall
    where
        oneCall callee = do
            lparen <- match [TLeftParen]
            case lparen of
              Nothing -> pure callee
              Just tok -> do
                  args <- arguments []
                  oneCall (Call (line tok) callee args)
        arguments acc = do
            maybeRparen <- peek
            case maybeRparen of
              Nothing -> parseError 0 " at end" "Expected right paren"
              Just tok
                | ty tok == TRightParen -> do
                    _ <- item
                    when (length acc > 255) $
                        addDiagnostic (line tok) "" "Can't have more than 255 arguments"
                    pure $ reverse acc
                | otherwise -> do
                    acc' <- expression >>= pure . (:acc)
                    nextTok <- peek
                    case nextTok of
                      Just tok'
                        | ty tok' == TComma -> item >> arguments acc'
                        | ty tok' == TRightParen -> arguments acc'
                        | otherwise -> parseError (line tok') "" "Expected comma or right paren"
                      Nothing -> parseError 0 " at end" "Expected comma or right paren"

primary :: Parser Expr
primary = peek >>= \case
    Nothing -> parseError 0 " at end" "Expected token"
    Just tok -> case ty tok of
        TTrue         -> item >> pure (Literal l (LitBool True))
        TFalse        -> item >> pure (Literal l (LitBool False))
        TNil          -> item >> pure (Literal l LitNil)
        TNumber n     -> item >> pure (Literal l (LitNumber n))
        TString s     -> item >> pure (Literal l (LitString s))
        TIdentifier i -> item >> pure (Variable l (UnresolvedName i))
        TFun          -> funExpr
        TLeftParen    -> do
            _ <- token TLeftParen
            expr <- expression
            _ <- token TRightParen
            pure $ Grouping l expr
        _          -> unexpectedToken tok (Right "expression")
      where l = line tok

funExpr :: Parser Expr
funExpr = do
    tok <- token TFun
    params <- funParams
    (_, body) <- block
    pure $ Fun (line tok) params body
