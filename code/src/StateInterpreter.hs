{-# LANGUAGE CPP #-}
module StateInterpreter (
  FrameArg(..),
  Frame(..),
  FunctionsMap(..),
  run,
  eval,
  makeArgs,
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
type Ident = Int
type CallState = (Mem, FrameId, NRFrames, Ident)

-- Programs are considered type safe  ===  (No type check)
run :: Program -> (Value, FrameId, NRFrames)
run ast = 
    let functions = functionMap ast Map.empty
        frame0 = Frame "main" [] [] cTOPFRAMEID
        mem0 = push (Mem Map.empty 0) frame0
        state0 = (mem0, lastFrameId mem0, 1, 0)
    in  case Map.lookup "main" functions of
            Nothing -> error "main not found"
            Just (actuals, expr, _) -> 
              let (v, (_, s0, s1, _)) = runState (eval expr functions) state0
              in  (v, s0, s1)

eval :: Expr            -- expression to be evaluated
    ->  FunctionsMap    -- dictionary (K: function name, V: (formals, body))
    ->  State CallState -- State: execution stack, number of stackframes allocated
              Value     -- Value: evaluation result
eval e funs = do
#if defined(DEBUG)        -- | Debugging information starts.
  st@(mem, frameId, nFrames, indent) <- get
  let thisFrame@(Frame caller funArgs susps prevFrameId) = getFrame mem frameId
  let lineFill = "================================================================\n"
  let debugPrefix = show nFrames ++ ". " ++ L.replicate (indent * 4) ' '
  trace (lineFill ++ debugPrefix ++ "expr = " ++ show e ++ ", frame#" ++ show frameId ++ ":\n" ++ showStack mem frameId) $
#else                     -- | Debugging information ends.
    st@(mem, frameId, nFrames, indent) <- get
    let thisFrame@(Frame caller funArgs susps prevFrameId) = getFrame mem frameId
#endif
    case e of
      EInt n -> return (VI n)
      UnaryOp unaryArithm e ->  do
        VI v <- eval e funs
        return $ VI (
          case unaryArithm of 
            EUnPlus -> v
            EUnMinus -> -v)
      BinaryOp binArithm l r -> do
        VI vl <- eval l funs
        VI vr <- eval r funs
        return $ VI (
          case binArithm of
            EAdd -> vl + vr
            ESub -> vl - vr
            EMul -> vl * vr
            EDiv -> vl `safeDiv` vr
            EMod -> vl `safeMod` vr)
      Eif c l r -> do
        VI vc <- eval c funs
        if vc /= 0  
          then eval l funs
          else eval r funs
      EVar var -> do
        -- Case: variable is in formal parameters of a function
          let i = case Map.lookup caller funs of
                    Nothing -> error "No function definition found"
                    Just (formals, _, _) -> 
                      let justVars = Data.List.map fst formals
                          errorFun = "Function = " ++ caller
                          errorVar = "\nVariable not in formals: Var = " ++ var
                          errorVars = "\nFormals = " ++ show justVars 
                          errorMem = "\nMemory Dump: \n" ++ show mem
                          errorMsg = error (errorFun ++ errorVar ++ errorVars ++ errorMem)
                      in  fromMaybe errorMsg (elemIndex var justVars)
              (v, s) = 
                if i > length funArgs then error ("Variable lookup: i = " ++ show i ++ ": out of bounds")
                else  case funArgs !! i of
                        StrictArg v   -> (v, st)
                        ByNameArg e   -> 
                          let (v, _) = runState (eval e funs) (mem, prevFrameId, nFrames, indent)
                          in  (v, st)
                        LazyArg e b val -> 
                            if b  
                              then (fromJust val, st)
                              else  let (v', (mem', _, n', _)) = runState (eval e funs) (mem, prevFrameId, nFrames, indent)
                                        funArgs' = replaceNth i (LazyArg e True (Just v')) funArgs
                                        frame' = thisFrame { fArgs = funArgs' }
                                    in  (v', (updFrame mem' frameId frame', frameId, n', indent))
          put s
#if defined(DEBUG)          -- | Debugging information starts.
          trace (debugPrefix ++ "Variable [" ++ var ++ "] lookup: " ++ show v ++ "\n") $
            return v
#else                       -- | Debugging information ends.
          return v
#endif
      Call callee actuals -> do
        let errorFunStr = "Call function: " ++ callee ++ " does not exist"
            errorFun = error errorFunStr
            
            (formals, funBody, depth) = fromMaybe errorFun (Map.lookup callee funs)
            (stackFrame, (mem', _, stNum', _)) = makeArgs actuals formals funs ([], st)
            -- Push frame and enter (use its frame id)
            mem'' = push mem' (Frame callee stackFrame [] frameId)
        put (mem'', lastFrameId mem'', stNum' + 1, indent)
        v <- eval funBody funs
        (mem''', _, stNum'', indent') <- get
        put (mem''', frameId, stNum'', indent')
        return v

      ---------------------------------------------------
      --------------DATA DECONSTRUCTION------------------
      -- Assume:                                        |
      --  a. cases = [Cons x y, Nil] or [Nil, Cons x y] |
      --  b. also pattern is exhaustive                 |
      ---------------------------------------------------
      -- Note: case forces evaluation of data           |
      ---------------------------------------------------
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
                          newSusp = (cid, c)
                          frame' = 
                            case L.lookup cid susps of -- used for TCO
                              Nothing -> thisFrame { fSusps = newSusp : susps }
                              Just _  -> thisFrame { fSusps = updateL cid c susps }
                          st' = (updFrame mem' frameId frame', frameId, n, indent)
                      in  (ne, st')
        put nextST
        eval nextE funs
      Nil -> return (VC (Susp ("Nil", []) frameId))
      ConstrF tag exprs -> return (VC (Susp (tag, exprs) frameId))
      CProj cid cpos -> do -- forces evaluation
        let cprojStr = "CProj: Not in suspensions, susps = " ++ show susps
            memoryDumpMsg = "\n Memory dump: \n" ++ show mem
            errorSuspsMsg = error $ cprojStr ++ memoryDumpMsg
            susp@(Susp (_, el) savedFrameId) = fromMaybe errorSuspsMsg (L.lookup cid susps)
            nextE = el !! cpos
            (val, (mem', _, nFrames', _)) = runState (eval nextE funs) (mem, savedFrameId, nFrames, indent)
        put (mem', frameId, nFrames', indent)
        return val
--------------------------------------------------------------
-- | After analysis annotations, runtime handle of tail-calls.
--------------------------------------------------------------
      TailCall callee actuals -> do -- actuals, formals belong to callee function
        let errorFnStr fn = "Call function: " ++ fn ++ " does not exist"
            errorFn fn = error (errorFnStr fn)

            (formals, funBody, _) = fromMaybe (errorFn callee) (Map.lookup callee funs)

        put (checkMutate actuals formals callee funs funArgs st)
        eval funBody funs

-- Runtime check if mutation is possible and mutate
checkMutate :: [Expr]            -- list of actual parameters
              -> [Formal]        -- callee's formal parameters
              -> FN
              -> FunctionsMap
              -> [FrameArg]      -- previous stackframe to be mutated
              -> CallState       
              -> CallState
checkMutate actuals@(a : as) formals@(f : fs) callee funs args thisState = 
  let (mem, frameId, nFrames, indent) = thisState
      thisFrame@(Frame fn fnArgs susps prevFrameId) = getFrame mem frameId
      (callerFormals, _, _) = fromMaybe (error $ "Function " ++ fn ++ " not found") (Map.lookup fn funs)
  in  
      -- Case 1: True if actuals not dependent by caller's formals
      if actualsFS formals actuals 
          then  let (args', nextState) = makeArgs actuals formals funs ([], thisState)
                    (mem', frameId', n, ident') = nextState
                    newFrame = Frame callee args' susps prevFrameId 
                    nextState' = (updFrame mem' frameId newFrame, frameId, n, indent)
                in  nextState'

--   --------------------------------------------------------------
--   -- Case 2: If all actuals are variables (or values-integers) |
--   --  Case 2a, 2b are handled in mutate                        |
--   --------------------------------------------------------------
--   -- Case 3: Actual parameter is expr in CBV position          |
--   --------------------------------------------------------------
          else  let (args', nextState) = mutate callerFormals formals args 0 funs actuals ([], thisState)
                    (mem', frameId, nFrames', indent') = thisState
                    newFrame = Frame callee args' susps prevFrameId
                in  (updFrame mem' frameId newFrame, frameId, nFrames', indent')


--  case 2a, case 2b mutation
mutate :: [Formal]                 -- caller's formals
        -> [Formal]                -- callee's formals
        -> [FrameArg]              -- current stack frame
        -> Int                     -- index in actuals
        -> FunctionsMap            -- map K: function name, V: (formals, body)
        -> [Expr]                  -- actuals
        -> ([FrameArg], CallState) -- acc (stackFrame, state)
        -> ([FrameArg], CallState) -- new (stackFrame, state)
mutate _        _        _    _  _    []       (args', st') = (reverse args', st')
mutate callerFs calleeFs args ix funs (a : as) (args', st)  =
  let (_, (tp, _)) = calleeFs !! ix 
  in  case a of
-- Case 2a, 2b  
        EVar v -> 
          let Just (tp', dom) = L.lookup v callerFs
              ix'      = fromJust $ L.elemIndex (v, (tp', dom)) callerFs
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
                                            in  case v of 
                                                  VI v' -> (ByNameArg (EInt v'), st)
                                                  VC c  -> error ("TCO: CBN - CBV, constructor = " ++ show c)
                                  CBN    -> (arg, st)  
                                  G.Lazy -> let LazyArg e b v = arg 
                                            in  if b 
                                                  then  case fromJust v of
                                                          VI v' -> (ByNameArg (EInt v'), st) 
                                                          VC c  -> error ("TCO: CBN - Lazy, constructor = " ++ show c)
                                                  else (ByNameArg e, st)  
                      G.Lazy -> case tp' of
                                  CBV     ->  error "TCO Lazy-CBV ????"
                                              -- let StrictArg v = arg 
                                              -- in  (LazyArg (EInt v) True (Just v), st)  
                                  CBN     ->  let ByNameArg e = arg 
                                              in  (LazyArg e False Nothing, st)  
                                  G.Lazy  ->  (arg, st) 
          in  mutate callerFs calleeFs args (ix + 1) funs as (arg' : args', st')                 

        EInt n -> 
          let arg' = case tp of 
                        CBV     -> StrictArg { val = VI n } 
                        CBN     -> ByNameArg { expr = a }
                        G.Lazy  -> LazyArg { expr = a, isEvaluated = False, cachedVal = Nothing } 
          in  mutate callerFs calleeFs args (ix + 1) funs as (arg' : args', st)

-- Case 3: We know if it is an expression, it should be in CBV position
        _      ->
          let (v', st') = runState (eval a funs) st
              arg' = StrictArg v'
          in  mutate callerFs calleeFs args (ix + 1) funs as (arg' : args', st')

makeArgs :: [Actual]
              -> [Formal]
              -> FunctionsMap
              -> ([FrameArg], CallState)
              -> ([FrameArg], CallState)
makeArgs [] [] _ (args, st) = (reverse args, st)
makeArgs (actual : actuals) (formal : formals) funs (fArgs, st) =
  case fst $ snd formal of
    CBV    ->
      let (v, st') = runState (eval actual funs) st
          fArgs'  = StrictArg { val = v } : fArgs
      in  makeArgs actuals formals funs (fArgs', st')
    CBN    ->
      let fArgs' = ByNameArg { expr = actual } : fArgs
      in  makeArgs actuals formals funs (fArgs', st)
    G.Lazy ->
      let fArgs' = LazyArg { expr = actual, isEvaluated = False, cachedVal = Nothing } : fArgs
      in  makeArgs actuals formals funs (fArgs', st)
makeArgs actuals@_ formals@_ _ _ =
  let errorStrMsg = "makeArgs: Inexhaustive patterns... "
      errorStrActuals = " actuals = " ++ show actuals
      errorStrFormals = " formals = " ++ show formals
      errorMsg = error errorStrMsg
  in  errorMsg


-- construct dictionary(map) where 
  -- K: function name, 
  -- V: (formal parameters, expression)
functionMap
  -- + program from parsing
  :: Program
  -- + map K V where K: function name, V: (formals, expr)
     -> Map.Map String ([(String, (EvalOrder, Type))], Expr, Depth)
  -- - map K V where K: function name, V: (formals, expr)
     -> Map.Map String ([(String, (EvalOrder, Type))], Expr, Depth)
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

safeDiv vl vr = 
  if vr == 0  
    then error "Division by zero"
    else vl `div` vr

safeMod vl vr = 
  if vr == 0  
    then error "Modulo zero"
    else vl `mod` vr