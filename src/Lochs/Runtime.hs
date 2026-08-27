{-# LANGUAGE Strict, MagicHash #-}

module Lochs.Runtime where

import Data.Array.Dynamic.L qualified as DA
import GHC.Exts (MutableArray#, RealWorld)
import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Unique (Unique)

import Lochs.AST (ResolvedName, resolvedNameText)

data    Scope     = Scope (MutableArray# RealWorld Value)
newtype LocalEnv  = LocalEnv (DA.Array Scope)

data Flow = Normal
          | Break
          | Continue
          | Return Value

newtype EvalContext = EvalContext { ctxLocal :: LocalEnv }

type ExprC = EvalContext -> IO Value
type StmtC = EvalContext -> IO Flow

data Class = Class
    { classId :: Unique
    , className :: ResolvedName
    , classArity :: Int
    , classMethods :: IntMap Value
    }

instance Eq Class where
    Class a _ _ _ == Class b _ _ _ = a == b

instance Show Class where
    show (Class _ n _ _) = resolvedNameText n

data Value = VBool   Bool
           | VNumber {-# UNPACK #-} Double
           | VString String
           | VNil
           | VNativeFunction
               { funId :: NativeFunctionID
               , arity :: Int
               }
           | VLochsFunction
               { unique :: Unique
               , closure :: {-# NOUNPACK #-} LocalEnv
               , name :: Maybe ResolvedName
               , funParams :: [ResolvedName]
               , funBody :: StmtC
               , numVars :: Int
               , arity :: Int
               , isInit :: Bool
               }
           | VUninit
           | VClass Class
           | VInstance Unique Class (IORef (IntMap Value))

instance Eq Value where
    (VBool a) == (VBool b) = a == b
    (VNumber a) == (VNumber b) = a == b
    (VString a) == (VString b) = a == b
    VNil == VNil = True
    (VNativeFunction a _) == (VNativeFunction b _) = a == b
    (VLochsFunction a _ _ _ _ _ _ _) == (VLochsFunction b _ _ _ _ _ _ _) = a == b
    (VClass a) == (VClass b) = a == b
    (VInstance a _ _) == (VInstance b _ _) = a == b
    VUninit == _ = error "uninit in Eq"
    _ == VUninit = error "uninit in Eq"
    _ == _ = False

data NativeFunctionID = FClock
                      deriving (Eq)

nativeFunction :: NativeFunctionID -> Value
nativeFunction FClock = VNativeFunction FClock 0

instance Show NativeFunctionID where
    show = \case
        FClock -> "clock"

instance Show Value where
    show = \case
        VBool   b -> if b then "true" else "false"
        VNumber n -> show n
        VString s -> s
        VNil      -> "nil"
        VNativeFunction f _ -> "<fn " ++ show f ++ ">"
        VLochsFunction _ _ name _ _ _ _ _ ->
            "<fn " ++ fromMaybe "<anon>" (resolvedNameText <$> name) ++ ">"
        VUninit -> "<uninit>"
        VClass c -> show c
        VInstance _ c _ -> show c ++ " instance"

typeName :: Value -> String
typeName = \case
    VBool   _ -> "boolean"
    VNumber _ -> "number"
    VString _ -> "string"
    VNil      -> "nil"
    VNativeFunction _ _ -> "function"
    VLochsFunction _ _ _ _ _ _ _ _ -> "function"
    VUninit -> error "getting type name of uninit"
    VClass _ -> "class"
    VInstance _ c _ -> show c

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
    VUninit -> error "truthiness check on uninit"
    _       -> True

isEqual :: Value -> Value -> Bool
isEqual = (==)
