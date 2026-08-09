module Lochs.Runtime where

import Data.IORef (IORef)
import Data.List (stripPrefix)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Unique (Unique)

import Lochs.AST (Stmt)

data Env = Env
    { values :: IORef (Map.Map String (IORef Value))
    , parent :: Maybe Env
    }

data Value = VBool   !Bool
           | VNumber !Double
           | VString !String
           | VNil
           | VNativeFunction !NativeFunctionID
           | VLochsFunction Unique Env String ![String] ![Stmt]

instance Eq Value where
    (VBool a) == (VBool b) = a == b
    (VNumber a) == (VNumber b) = a == b
    (VString a) == (VString b) = a == b
    VNil == VNil = True
    (VNativeFunction a) == (VNativeFunction b) = a == b
    (VLochsFunction a _ _ _ _) == (VLochsFunction b _ _ _ _) = a == b
    _ == _ = False

data NativeFunctionID = FClock
                      deriving (Eq)

nativeFunctionArity :: NativeFunctionID -> Int
nativeFunctionArity FClock = 0

data Callable = NativeFunction { arity :: !Int, funId :: NativeFunctionID }
              | LochsFunction  { arity :: !Int, env :: !Env, params :: ![String], body :: ![Stmt] }

instance Show NativeFunctionID where
    show = \case
        FClock -> "clock"

instance Show Value where
    show = \case
        VBool   b -> if b then "true" else "false"
        VNumber n -> show n
        VString s -> s
        VNil      -> "nil"
        VNativeFunction f -> "<fn " ++ show f ++ ">"
        VLochsFunction _ _ name _ _ -> "<fn " ++ name ++ ">"

typeName :: Value -> String
typeName = \case
    VBool   _ -> "boolean"
    VNumber _ -> "number"
    VString _ -> "string"
    VNil      -> "nil"
    VNativeFunction _ -> "function"
    VLochsFunction _ _ _ _ _ -> "function"

stringify :: Value -> String
stringify = \case
    VNumber n -> fromMaybe s (stripSuffix ".0" s)
        where
            s = show n
            stripSuffix suffix str =
                reverse <$> stripPrefix (reverse suffix) (reverse str)
    v -> show v

isTruthy :: Value -> Bool
isTruthy = \case
    VNil    -> False
    VBool v -> v
    _       -> True

isEqual :: Value -> Value -> Bool
isEqual = (==)
