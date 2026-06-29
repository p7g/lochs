module Main (main) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (catchIOError, isEOFError)

import Lochs.Scanner (scan)

main :: IO ()
main = getArgs >>= \case
    [filename] -> runFile filename
    [] -> runPrompt
    _ -> do
        hPutStrLn stderr "Usage: lochs [script]"
        exitWith $ ExitFailure 64

runFile :: String -> IO ()
runFile file = do
    code <- readFile file
    hadError <- run code
    when hadError $ exitWith (ExitFailure 65)

runPrompt :: IO ()
runPrompt = catchIOError loop handler
    where
        loop = do
            putStr "> "
            hFlush stdout
            line <- getLine
            _hadError <- run line
            loop
        handler e = if isEOFError e then putStrLn "" else ioError e

run :: String -> IO Bool
run code = do
    let (tokens, diagnostics) = scan code
    traverse_ (putStrLn . show) diagnostics
    traverse_ (putStrLn . show) tokens
    return $ not (null diagnostics)
