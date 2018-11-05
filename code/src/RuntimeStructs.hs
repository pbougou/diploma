module RuntimeStructs (
    StackFrame(..),
    StackFrameArg(..),
    CallStack(..),
    Value(..),
    Context(..),
    Susp(..),
    FunctionsMap(..),
    NRFrames,
    showStack
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

instance Show StackFrameArg where
  show (StrictArg val) = show val
  show (ByNameArg e) = show e
  show (LazyArg e True cachedVal) =
    case cachedVal of
      Just val -> "{cached : " ++ show cachedVal ++ " }"
      Nothing -> error $ "Corrupt lazy argument of expression: " ++ (show e)
  show (LazyArg expr False cachedVal) =
    case cachedVal of
      Nothing -> "{ unevaluated : " ++ show expr ++ " }"
      Just val -> error $ "Corrupt lazy argument: forgot its memoized value " ++ (show val)

type FunctionsMap = Map.Map String ([Formal], Expr)

-- cactus stack = stack + heap
type Context = (StackFrame, [(CaseID, Susp)])
type CallStack = [Context]

data Susp = Susp (CN, [Expr]) CallStack  -- Constructor carry the environment so far

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

showStackFrame :: StackFrame -> String
showStackFrame (fn, args) =
  "** Stack frame [" ++ fn ++ "] **\n" ++ concatMap (\s -> show s ++ "\n") args
  
