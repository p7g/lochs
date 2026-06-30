module Lochs.Runtime (Value(..), isEqual, isTruthy, stringify) where

data Value = VBool Bool
           | VNumber Double
           | VString String
           | VNil
           deriving (Eq)

instance Show Value where
    show = \case
        VBool   b -> show b
        VNumber n -> show n
        VString s -> s
        VNil      -> "nil"

stringify :: Value -> String
stringify = \case
    VBool b   -> if b then "true" else "false"
    VNumber n -> show n
    VString s -> show s
    VNil      -> "nil"

isTruthy :: Value -> Bool
isTruthy = \case
    VNil    -> False
    VBool v -> v
    _       -> True

isEqual :: Value -> Value -> Bool
isEqual = (==)
