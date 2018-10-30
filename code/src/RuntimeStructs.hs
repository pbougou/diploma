module RuntimeStructs (
    StackFrame(..),
    StackFrameArg(..),
    CallStack(..),
    Value(..),
    Context(..),
    Susp(..),
    FunctionsMap(..),
    NRFrames,
    showStack,
    showContext,
    Globals,
    Env, Code, FreeVars, Block,
    BTag(..)
) where
import Grammar 
import Heap
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

-- Top level functions
--      Map each function to its heap address, where function's closure is stored
type Globals = Map.Map FN Addr
-- Local environment : Map: [variables -> values]
type Env = Map VN Value
-- Heap building block
type Code = Expr 
type FreeVars = [VN]
data BTag =   Lambda
            | Constructor
type Block = (Code, BTag, FreeVars)
-- Runtime statistics
type NRFrames = Integer

instance Show Susp where
    showsPrec p (Susp (cn, exprs) st) = 
        ("[ suspension of " ++) 
        . (show cn ++) 
        . (": " ++) 
        . (show exprs ++) 
        . (" | " ++) 
        . (show st ++)
        . (" | " ++)
        . (" ] " ++)

-- Pretty printer for context
showContext :: Context -> String
showContext (ar, susps) = 
    "====================================================================\nActivation record\n" ++
    show ar ++
    "\nSuspensions\n" ++
    show susps  ++
    "\n"

-- Pretty printer for callstack
showStack :: CallStack -> String
showStack = L.foldr (\s acc -> acc ++ showContext s) ""
