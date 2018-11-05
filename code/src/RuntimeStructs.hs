module RuntimeStructs (
    Frame(..),
    FrameArg(..),
    CallStack(..),
    Depth,
    Value(..),
    Susp(..),
    FunctionsMap(..),
    Mem(..),
    NRFrames,
    push,
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

data Frame = Frame FN [FrameArg] [(CaseID, Susp)]

instance Show Frame where
  show (Frame fn args susps) =
    "Activation record [ " ++ show fn ++ "]\n" ++
    "Args:\n" ++ (concatMap showLine args) ++
    "Suspensions:\n" ++ (concatMap showLine susps)

data FrameArg = StrictArg { val :: Value }
              | ByNameArg { expr :: Expr }
              | LazyArg   { expr :: Expr, isEvaluated :: Bool, cachedVal :: Maybe Value }

instance Show FrameArg where
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

type FrameId = Int
data Mem = Mem { memFrames :: Map.Map FrameId Frame, lastFrameId :: FrameId }

push :: Mem -> Frame -> Mem
push mem f =
  let lastId = (lastFrameId mem) + 1
      frames' = Map.insert lastId f (memFrames mem)
  in  Mem { memFrames = frames', lastFrameId = lastId }

getFrame :: Mem -> FrameId -> Frame
getFrame mem i =
  case Map.lookup i (memFrames mem) of
    Just frame -> frame
    Nothing -> error $ "Internal error: no frame in memory for id " ++ (show i)

type Depth = Int
type FunctionsMap = Map.Map String ([Formal], Expr, Depth)

-- cactus stack = stack + heap
type CallStack = [Frame]

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

-- Pretty printer for callstack
showStack :: CallStack -> String
showStack = L.foldr (\s acc -> acc ++ show s) ""

showList :: String -> [String] -> String
showList delim l =
  concat $ L.intersperse delim l

showLine :: (Show s) => s -> String
showLine s = show s ++ "\n"
