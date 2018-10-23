module RuntimeStructs (
    StackFrame(..),
    StackFrameArg(..),
    CallStack(..),
    Value(..),
    Context(..),
    Susp(..),
    FunctionsMap(..)
) where
import Grammar 
import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Text.Read

-- Runtime data structures
data Value =  VI Integer 
            | VC Susp 
    deriving Show

type StackFrame = (FN, [StackFrameArg])
data StackFrameArg = StrictArg { val :: Value }
                   | ByNameArg { expr :: Expr }
                   | LazyArg   { expr :: Expr, isEvaluated :: Bool, cachedVal :: Maybe Value }
    deriving Show

type FunctionsMap = Map.Map String ([Formal], Expr)


-- cactus stack = stack + heap
type Context = (StackFrame, [(CaseID, Susp)])
type CallStack = [Context]

data Susp = Susp (CN, [Expr]) CallStack  -- Constructor carry the environment so far
    deriving Show