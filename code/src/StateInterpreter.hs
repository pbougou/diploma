module StateInterpreter (
  FrameArg(..),
  Frame(..),
  FunctionsMap(..),
  run,
  eval,
  makeFrameArgs,
  replaceNth,
  updateL,
  functionMap
) where

import Grammar as G
import RuntimeStructs

import AuxAnalysis

import Debug.Trace

import Data.Maybe

import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L

import Data.Map.Strict hiding(foldr)
import qualified Data.Map.Strict as Map

import Control.Monad.State

import System.IO.Unsafe

-- The state of a call: its stack, the frames counter, and a helper
-- integer for pretty printing.
type CallState = (Mem, FrameId, NRFrames, Int)

-- Programs are considered type safe  ===  (No type check)
-- Also do not name your functions cons 
run :: Program -> (Value, FrameId, NRFrames)
run ast = 
    let functions = functionMap ast Map.empty
        frame0 = Frame "main" [] [] cTOPFRAMEID
        mem0 = push (Mem Map.empty 0) frame0
        state0 = (mem0, lastFrameId mem0, 1, 0)
    in  case Map.lookup "main" functions of
            Nothing              -> error "main not found"
            Just (actuals, expr, _) -> let (v, (_, s0, s1, _)) = runState (eval expr functions) state0
                                       in  (v, s0, s1)

eval :: Expr                        -- expression to be evaluated
    ->  FunctionsMap                -- dictionary (K: function name, V: (formals, body))
    ->  State CallState             -- State: execution stack, number of stackframes allocated
              Value                 -- Value: evaluation result
eval e funs = do
  st@(mem, frameId, nFrames, indent) <- get
  let thisFrame@(Frame funName funArgs susps prevFrameId) = getFrame mem frameId
  let debugPrefix = show nFrames ++ ". " ++ L.replicate (indent*4) ' '
  trace (debugPrefix ++ "expr = " ++ show e ++ ", frame#" ++ show frameId ++ ":\n" ++ showStack mem frameId) $
    case e of
      EInt n -> 
        return (VI n)
      UnaryOp unaryArithm e ->  do
        VI v <- eval e funs
        case unaryArithm of 
          EUnPlus -> 
            return $ VI v
          EUnMinus ->
            return $ VI (-v)
      BinaryOp binArithm l r -> do
        VI vl <- eval l funs
        VI vr <- eval r funs
        case binArithm of
          EAdd -> return $ VI (vl + vr)
          ESub -> return $ VI (vl - vr)
          EMul -> return $ VI (vl * vr)
          EDiv ->
            if vr == 0  then error "Division by zero"
                        else return $ VI (vl `div` vr)
          EMod ->
            if vr == 0  then error "Modulo zero"
                        else return $ VI (vl `mod` vr)
      Eif c l r -> do
        VI vc <- eval c funs
        if vc /= 0  then eval l funs
                    else eval r funs
      EVar var -> do
        -- Case variable is in formal parameteres of a function
          let i = case Map.lookup funName funs of
                    Nothing -> error "No function definition found"
                    Just (formals, _, _) -> 
                      let justVars = Data.List.map fst formals
                          errorFun = "function = " ++ funName
                          errorVar = "\nvariable not in formals: Var = " ++ var
                          errorVars = "\nformals = " ++ show justVars 
                          errorMem = "\nmemory dump: \n" ++ show mem
                          errorMsg = error (errorFun ++ errorVar ++ errorVars ++ errorMem)
                      in  fromMaybe errorMsg (elemIndex var justVars)
              (v, s) = 
                if i > length funArgs then error "i: out of bounds"
                else  case funArgs !! i of
                        StrictArg v   -> (v, st)
                        ByNameArg e   -> 
                          let (v, _) = runState (eval e funs) (mem, prevFrameId, nFrames, indent)
                          in  (v, st)
                        LazyArg e b val -> 
                            if b then (fromJust val, st)
                                  else  let (v', (mem', _, n', _)) = runState (eval e funs) (mem, prevFrameId, nFrames, indent)
                                            funArgs' = replaceNth i (LazyArg e True (Just v')) funArgs
                                            frame' = thisFrame { fArgs = funArgs' }
                                        in  (v', (updFrame mem' frameId frame', frameId, n', indent))
          put s
          trace (debugPrefix ++ "Variable [" ++ var ++ "] lookup: " ++ show v ++ "\n") $
            return v
      Call funName actuals -> do
        let (formals, funBody, depth) = fromMaybe (error $ "Call function: " ++ funName ++ " does not exist") (Map.lookup funName funs)
            (stackFrame, (mem', _, stNum', _)) = makeFrameArgs actuals formals funs ([], st)
            -- Push frame and enter (use its frame id)
            mem'' = push mem' (Frame funName stackFrame [] frameId)
        put (mem'', lastFrameId mem'', stNum' + 1, indent)
        eval funBody funs
      -------------------------------------------------
      --------------DATA DECONSTRUCTION----------------
      -- Assume: 
      --  a. cases = [Cons x y, Nil] or [Nil, Cons x y]
      --  b. also pattern is exhaustive
      -------------------------------------------------
      -- Note: case forces evaluation of data 
      -------------------------------------------------
      CaseF cid e cases -> do
        let (evalE, st@(mem', savedFrameId, n, _)) = runState (eval e funs) (mem, frameId, nFrames, indent + 1)
        let 
          -- nextST: next stack state
          -- nextE:  expression to be evaluated next
          (nextE, nextST) = 
            case evalE of
          -- 1. evalE is an expression 
              VI i -> let nextE = fromMaybe (error "Case: Patterns must be integers") (L.lookup (IPat i) cases)
                      in  (nextE, st)
          -- 2. evalE is a constructor 
              VC c@(Susp (cn, _) _) ->
                      let patterns = L.map fst cases
                          pattIndex = indexOfPattern cn patterns 0
                          (_, ne) = cases !! pattIndex
                          frame' = thisFrame{fSusps = (cid, c) : susps}
                          st' = (updFrame mem' frameId frame', frameId, n, indent)
                      in  (ne, st')
        put nextST
        eval nextE funs
      ConstrF tag exprs -> return (VC (Susp (tag, exprs) frameId))
      CProj cid cpos -> do 
        let susp@(Susp (_, el) savedFrameId) = 
              fromMaybe (error $ "CProj - not in susps, susps = " ++ show susps ++ ", memory dump: \n" ++ show mem) (L.lookup cid susps)
            nextE =
              -- TODO: must be fixed in the grammar
              if cpos >= length el then ConstrF "Nil" []
              else el !! cpos
            (val, (mem', _, nFrames', _)) = runState (eval nextE funs) (mem, savedFrameId, nFrames, indent)
            -- TODO: review the following
            -- el' =
            --   case val of
            --     VI v -> replaceNth cpos (EInt v) el
            --     VC c -> el -- error $ "Constructor " ++ show c
            -- newSusp = Susp (cn, el') savedFrameId'
        put (mem', frameId, nFrames', indent)
        -- trace ("CProj: val = " ++ show val ++ ", \nsusp = " ++ show newSusp ++ ",\nsusps = " ++ show susps) $
        return val


{-
    
    TailCall funName actuals -> do
      ---------------------------------------------
      -- funName: callee's function name          |
      -- actuals: callee's actual parameters      |
      -- formals: callee's formal parameters      |
      -- callerName: caller's name                |
      -- callerFormals: caller's formals          |
      ---------------------------------------------
      -- st:    state (callstack, e.r. in monad)  |
      -- oldSt: current call stack                |
      -- fSt:   top frame                         |
      -- st':   next frames                       |
      -- n:     number of stackFrames used so far |
      ---------------------------------------------
      st@(oldSt@(fSt : st'), n) <- get
      let (callerName, stArgs) = fSt

          (callerFormals, _) = case Map.lookup callerName funs of
                                  Nothing     -> error $ "Call function: " ++ callerName ++ " does not exist"
                                  Just (f, e) -> (f, e)

          (formals, funBody) = case Map.lookup funName funs of
                                  Nothing     -> error $ "Call function: " ++ funName ++ " does not exist"
                                  Just (f, e) -> (f, e)

          -- Runtime check if mutation is possible and mutate
          checkMutate :: [Expr]                 -- list of actual parameters
                        -> [Formal]             -- callee's formal parameters
                        -> [FrameArg]      -- previous stackframe to be mutated
                        -> (CallStack, Integer) -- new stack
          checkMutate actuals@(a : as) formals@(f : fs) args 
            -- Case 1: True if actuals not dependent by caller's formals
            | actualsFS callerFormals actuals =
                  let (args', (s : ss, frNum)) = makeFrame actuals formals funs ([], st)
                      frame' = (funName, args')
                  in  (frame' : ss, frNum)   -- make stack frame and throw old

            --------------------------------------------------------------
            -- Case 2: If all actuals are variables (or values-integers)
            --  Case 2a, 2b are handled in mutate
            --------------------------------------------------------------
            -- Case 3: Actual parameter is expr in CBV position
            --------------------------------------------------------------
            | otherwise =  
              let (args', nextST)    = mutate callerFormals formals args 0 funs actuals ([], st)
                  (newStack, newNum) = nextST
              in  ((funName, args') : tail newStack, newNum)

      put (checkMutate actuals formals stArgs)

      eval funBody funs

--  case 2a, case 2b mutation
mutate :: [Formal]                                  -- caller's formals
        -> [Formal]                                 -- callee's formals
        -> [FrameArg]                          -- current stack frame
        -> Int                                      -- index in actuals
        -> FunctionsMap                             -- map K: function name, V: (formals, body)
        -> [Expr]                                   -- actuals
        -> ([FrameArg], (CallStack, Integer))  -- acc (stackFrame, state)
        -> ([FrameArg], (CallStack, Integer))  -- new (stackFrame, state)
mutate _        _        _    _  _    []       (args', st') = (reverse args', st')
mutate callerFs calleeFs args ix funs (a : as) (args', st)  =
  let (_, tp) = calleeFs !! ix 
  in  case a of
-- Case 2a, 2b  
        EVar v -> 
          let Just tp' = L.lookup v callerFs
              ix'      = fromJust $ L.elemIndex (v, tp') callerFs
              arg      = args !! ix'
              (arg', st') = 
                if tp == tp' then (arg, st) -- 2a
                else case tp of             -- 2b
                      CBV   ->  case tp' of
                                  CBV     -> (arg, st)  
                                  CBN     ->  let ByNameArg e = arg
                                                  (v, st') = runState (eval e funs) st
                                              in  (StrictArg v, st') 
                                  G.Lazy  -> let LazyArg e b v = arg 
                                             in if b then (StrictArg (fromJust v), st) 
                                                else  let (v, st') = runState (eval e funs) st 
                                                      in  (StrictArg v, st')
                      CBN    -> case tp' of
                                  CBV   ->  let StrictArg v = arg 
                                            in  (ByNameArg (EInt v), st)
                                  CBN    -> (arg, st)  
                                  G.Lazy -> let LazyArg e b v = arg 
                                            in  if b then (ByNameArg (EInt (fromJust v)), st)  
                                                else (ByNameArg e, st)  
                      G.Lazy -> case tp' of
                                  CBV     ->  let StrictArg v = arg 
                                              in  (LazyArg (EInt v) True (Just v), st)  
                                  CBN     ->  let ByNameArg e = arg 
                                              in  (LazyArg e False Nothing, st)  
                                  G.Lazy  ->  (arg, st) 
          in  mutate callerFs calleeFs args (ix + 1) funs as (arg' : args', st')                 

        EInt n -> 
          let arg' = case tp of 
                        CBV     -> StrictArg { val = n } 
                        CBN     -> ByNameArg { expr = a }
                        G.Lazy  -> LazyArg { expr = a, isEvaluated = False, cachedVal = Nothing } 
          in  mutate callerFs calleeFs args (ix + 1) funs as (arg' : args', st)

-- Case 3: We know if it is an expression, it should be in CBV position
        _      ->
          let (v', st') = runState (eval a funs) st
              arg'           = StrictArg v'
          in  mutate callerFs calleeFs args (ix + 1) funs as (arg' : args', st')
          -- error $ "It should not be an expression: " ++ show a 
-}  

makeFrameArgs :: [Expr]
              -> [Formal]
              -> FunctionsMap
              -> ([FrameArg], CallState)
              -> ([FrameArg], CallState)
makeFrameArgs [] [] _ (args, st) = (reverse args, st)
makeFrameArgs (actual : actuals) (formal : formals) funs (frames, st) =
  case snd formal of
    CBV    ->
      let (v, st') = runState (eval actual funs) st
          frames'  = StrictArg { val = v } : frames
      in  makeFrameArgs actuals formals funs (frames', st')
    CBN    ->
      let frames' = ByNameArg { expr = actual } : frames
      in  makeFrameArgs actuals formals funs (frames', st)
    G.Lazy ->
      let frames' = LazyArg { expr = actual, isEvaluated = False, cachedVal = Nothing } : frames
      in  makeFrameArgs actuals formals funs (frames', st)
-- TODO: Fix Nil
makeFrameArgs actuals@_ formals@_ funs st = makeFrameArgs [ConstrF "Nil" []] formals funs st
  -- error ("makeFrameArgs: Inexhaustive patterns actuals = " ++ show actuals ++ ", formals = " ++ show formals)


-- construct dictionary(map) where 
  -- K: function name, 
  -- V: (formal parameters, expression)
functionMap
  -- + program from parsing
  :: Program
  -- + map K V where K: function name, V: (formals, expr)
     -> Map.Map String ([(String, Type)], Expr, Depth)
  -- - map K V where K: function name, V: (formals, expr)
     -> Map.Map String ([(String, Type)], Expr, Depth)
functionMap program _map =
    foldr (\(Fun x y z) w ->
            case Map.lookup x _map of
              Nothing -> Map.insert x (y, z, calcDepth z) w
              Just s  -> error $ "redeclaration of function " ++ show s) _map program

calcDepth :: Expr -> Depth
calcDepth (Call fn actuals) = calcDepthMax actuals
calcDepth EVar {} = 0
calcDepth EInt {} = 0
calcDepth (ConstrF _ el) = calcDepthMax el
calcDepth (CaseF _ e brs) = calcDepthMax (e : L.map snd brs)
calcDepth CProj {} = 0
calcDepth (UnaryOp _ e) = calcDepth e
calcDepth (BinaryOp _ e1 e2) = calcDepthMax [e1, e2]
calcDepth (Eif e0 e1 e2) = calcDepthMax [e0, e1, e2]

calcDepthMax :: [Expr] -> Depth
calcDepthMax el = L.foldl max 0 $ L.map calcDepth el

-- replaceNth :: Integer -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0    = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

-- Function that updates a value paired with a given key
--  Updates the first key found
updateL _ _ [] = []
updateL key val ((k, v) : xs) 
  | key == k  = (k, val) : xs
  | otherwise = (k, v) : updateL key val xs 

indexOfPattern cn patterns i = 
  case patterns of
    [] -> error $ "Patterns are assumed exhaustive: " ++ cn ++ " not in patterns"
    patt : patts -> 
      case patt of
        CPat cn' vars -> 
          if cn == cn' then i else indexOfPattern cn patts (i + 1)
        _ -> error "Searching for integer patterns in constructor case"
