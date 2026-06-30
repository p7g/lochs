module Lochs.Runtime (Value(..), stringify) where

data Value = VBool Bool
           | VNumber Double
           | VString String
           | VNil
           deriving (Show)

stringify :: Value -> String
stringify = \case
    VBool b   -> if b then "true" else "false"
    VNumber n -> show n
    VString s -> show s
    VNil      -> "nil"
