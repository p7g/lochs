module Main (main) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (catchIOError, isEOFError)

import Lochs.Eval
import Lochs.Parser (parse)
import Lochs.Scanner (scan)

main :: IO ()
main = getArgs >>= \case
    [filename] -> mkEnv >>= runFile filename
    [] -> mkEnv >>= runPrompt
    _ -> do
        hPutStrLn stderr "Usage: lochs [script]"
        exitWith $ ExitFailure 64

runFile :: String -> Env -> IO ()
runFile file env = do
    code <- readFile file
    hadError <- run env code
    when hadError $ exitWith (ExitFailure 65)

runPrompt :: Env -> IO ()
runPrompt env = catchIOError loop handler
    where
        loop = do
            putStr "> "
            hFlush stdout
            line <- getLine
            _hadError <- run env line
            loop
        handler e = if isEOFError e then putStrLn "" else ioError e

run :: Env -> String -> IO Bool
run env code = do
    let (tokens, diagnostics) = scan code
    case diagnostics of
      d:ds -> do
        traverse_ (putStrLn . show) (d:ds)
        pure True
      _ -> do
          let (stmts, diags) = parse tokens
          traverse_ (putStrLn . show) diags
          if null diags
             then exec env stmts >>= \case
                Left  d  -> putStrLn (show d) >> pure True
                Right () -> pure False
             else pure True
