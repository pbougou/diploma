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
    FreeVars,
    Env,
    VMValue(..),
    FValue,
    Stack,
    AR,
    VMSTate(..),
    Code,
    Block(..),
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
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- STG-Machine
-- Local environment : Map: [variables -> values]
type Env = Map VN VMValue
-- No # arguments for now
data VMValue =  VInt Integer
              | VAddr Addr
  deriving Show
type FValue = VMValue
-------------------------------------------------
-- 1. CAF: top-level function with no arguments
-- 2. HFun: top-level function with arguments
-- 3. HCons: constructor suspension
-- 4. Thunk: function built by let(updatable)
-------------------------------------------------
data Block = 
        CAF Code
    |   FunH [Formal] Code
    |   Thunk FreeVars Code [FValue]
    |   Cons [VN] Tag [VMValue]
  deriving Show
type Stack = [AR]
type AR = (FN, [VMValue])
data VMSTate = VMSTate {
  globals :: Globals,
  heap :: Heap Block,
  stack :: Stack,
  nrFrames :: NRFrames
}
-- Heap building block
type Code = Expr 
data BTag =   Lambda
            | Constructor
    deriving Show
-- type Block = (Code, BTag, FreeVars, [Formal], [VMValue])
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

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