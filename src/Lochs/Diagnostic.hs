module Lochs.Diagnostic (Diagnostic, mkDiagnostic, message, line) where

data Diagnostic = Diagnostic { line :: !Int
                             , where_ :: !String
                             , message :: !String
                             }

instance Show Diagnostic where
    show d = "[line " ++ (show $ line d) ++ "] Error" ++ (where_ d) ++ ": " ++ (message d)

mkDiagnostic :: Int -> String -> String -> Diagnostic
mkDiagnostic = Diagnostic
