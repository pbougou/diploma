module RuntimeStructs (
    Frame(..),
    FrameArg(..),
    Depth,
    Value(..),
    Susp(..),
    FrameId,
    FunctionsMap(..),
    Mem(..),
    NRFrames,
    cTOPFRAMEID,
    getFrame,
    push,
    showStack,
    updFrame
) where
import Grammar 
import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Text.Read
import Data.Maybe

-- Runtime data structures
data Value =  VI Integer 
            | VC Susp 
    deriving Show

type FrameId = Int
data Frame = Frame { fName :: FN, fArgs :: [FrameArg], fSusps :: [(CaseID, Susp)], fPrev :: FrameId }

type Address = Int
data Heap = Heap { heap :: Map.Map Address Expr, lastAddress :: Address} 

data TArg = Value | Address

instance Show Frame where
  show (Frame fn args susps prev) =
    "* Activation record [ function: " ++ show fn ++ ", previous frame: " ++ show prev ++ " ]\n" ++
    "Args:\n" ++ concatMap showLine args ++
    "Suspensions:\n" ++ concatMap showLine susps

data FrameArg = StrictArg { val :: Value }
              | ByNameArg { expr :: Expr }
              | LazyArg   { expr :: Expr, isEvaluated :: Bool, cachedVal :: Maybe Value }

instance Show FrameArg where
  show (StrictArg val) = show val
  show (ByNameArg e) = show e
  show (LazyArg e True cachedVal) =
    case cachedVal of
      Just val -> "{ cached : " ++ show cachedVal ++ " }"
      Nothing -> error $ "Corrupt lazy argument of expression: " ++ show e
  show (LazyArg expr False cachedVal) =
    case cachedVal of
      Nothing -> "{ unevaluated : '" ++ show expr ++ "' }"
      Just val -> error $ "Corrupt lazy argument: forgot its memoized value " ++ show val

data Mem = Mem { memFrames :: Map.Map FrameId Frame, lastFrameId :: FrameId }

instance Show Mem where
  show (Mem frames frameId) =
    let fStrings = L.concatMap (\(k, v) -> show k ++ " -> " ++ show v ++ "\n") $ Map.assocs frames
    in  "***** Memory ******\n" ++ "* Last frame ID : " ++ show frameId ++ "\n" ++ fStrings

push :: Mem -> Frame -> Mem
push mem f =
  let lastId = lastFrameId mem + 1
      frames' = Map.insert lastId f (memFrames mem)
  in  Mem { memFrames = frames', lastFrameId = lastId }

getFrame :: Mem -> FrameId -> Frame
getFrame mem i = fromMaybe (error $ "Internal error: no frame in memory for id " ++ show i) (Map.lookup i (memFrames mem))

updFrame :: Mem -> FrameId -> Frame -> Mem
updFrame mem frameId frame =
  mem{memFrames = Map.insert frameId frame (memFrames mem)}

cTOPFRAMEID :: FrameId
cTOPFRAMEID = 0

frameTrace :: Mem -> FrameId -> [Frame]
frameTrace mem fId =
  case Map.lookup fId (memFrames mem) of
    Just frame@(Frame _ _ _ prevId) -> frame : frameTrace mem prevId
    Nothing ->
      if fId == cTOPFRAMEID then [] else error $ "Frame trace error for id " ++ show fId

type Depth = Int
type FunctionsMap = Map.Map String ([Formal], Expr, Depth)

data Susp = Susp (CN, [Expr]) FrameId  -- Constructor carry the environment so far

-- Runtime statistics
type NRFrames = Integer

instance Show Susp where
    showsPrec p (Susp (cn, exprs) frameId) =
        ("[ suspension of " ++)
        . (show cn ++)
        . (" : exprs = " ++)
        . (show exprs ++)
        . (" | frameId = " ++)
        . (show frameId ++)
        . (" ] " ++)

-- Pretty printer for callstack
showStack :: Mem -> FrameId -> String
showStack mem fId = L.foldr (\s acc -> acc ++ show s) "" $ frameTrace mem fId

showList :: String -> [String] -> String
showList = L.intercalate

showLine :: (Show s) => s -> String
showLine s = show s ++ "\n"
