module StateInterpreter (
  StackFrameArg(..),
  StackFrame(..),
  FunctionsMap(..),
  CallStack(..),
  run,
  eval,
  -- makeStackFrame,
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

-- Programs are considered type safe  ===  (No type check)
-- Also do not name your functions cons 
run :: Program -> (Value, CallStack, Integer)
run ast = 
    let functions = functionMap ast Map.empty
    in  case Map.lookup "main" functions of
            Nothing              -> error "main not found"
            Just (actuals, expr) -> let (v, s) = runState (eval expr functions) ([(("main", []),[])], 1)
                                    in  (v, fst s, snd s)

eval :: Expr                       -- expression to be evaluated
    ->  FunctionsMap               -- dictionary (K: function name, V: (formals, body))
    ->  State 
              (CallStack, Integer) -- State: execution stack, number of stackframes allocated 
              Value                -- Value: evaluation result
-- eval e funs | trace ("expr = " ++ show e ) False = undefined
eval e funs = do
  (stack, nFrames) <- get
  -- trace ("stack = " ++ show stack) $
  trace ("expr = " ++ show e ++ "\nstack = " ++ show stack) $
  -- trace "" $
    case e of
      EInt n -> 
        trace "EInt" $
          return (VI n)
      EUnPlus e -> 
        trace "EUnPlus" $
          eval e funs
      EUnMinus e -> do
        VI v <- eval e funs
        trace "EUnMinus" $
          return $ VI (-v)
      EAdd l r -> do
        VI vl <- eval l funs
        VI vr <- eval r funs
        trace "EAdd" $
          return $ VI (vl + vr)
      ESub l r -> do
        VI vl <- eval l funs
        VI vr <- eval r funs
        trace "ESub" $        
          return $ VI (vl - vr)
      EMul l r -> do
        VI vl <- eval l funs
        VI vr <- eval r funs
        trace "EMul" $
          return $ VI (vl * vr)
      EDiv l r -> do
        VI vl <- eval l funs
        VI vr <- eval r funs
        trace "EDiv" $
          if vr == 0 then error "Division by zero"
                     else return $ VI (vl `div` vr)
      EMod l r -> do
        VI vl <- eval l funs
        VI vr <- eval r funs
        trace "EMod" $
          if vr == 0  then error "Modulo zero"
                      else return $ VI (vl `mod` vr)
      Eif c l r -> do
        VI vc <- eval c funs
        trace "EIf" $
          if vc /= 0 then eval l funs
                     else eval r funs
      -------------------------------------------------
      -- Assume: 
      --  a. cases = [Cons x y, Nil] or [Nil, Cons x y]
      --  b. also pattern is exhaustive
      -------------------------------------------------
      -- Note: case forces evaluation of data 
      CaseF cid e cases -> do
        curST <- get 
        let (evalE, st@((ar, susps) : st', n)) = runState (eval e funs) curST
        put st
        let 
          -- nextST: next stack state
          -- nextE:  expression to be evaluated next
          (nextE, nextST) = 
            case evalE of
          -- 1. evalE is an expression 
              VI i -> let nextE = fromMaybe (error "Case patterns must be integers") (L.lookup (EInt i) cases)
                      in  (nextE, st)
          -- 2. evalE is a constructor 
              VC c -> let Susp (cn, _) _ = c
                          nilIX = fromMaybe (error "Non-exhaustive patterns: Nil does not exist") (L.elemIndex (ConstrF "Nil" []) (L.map fst cases))
                          consIX = 1 - nilIX
                          ne = 
                            if nilIX > length cases || consIX > length cases then error $ "Index out of bounds: Index = " ++ show nilIX
                            else case cn of
                                  "Nil" -> snd (cases !! nilIX)
                                  "Cons" -> snd (cases !! consIX)   -- Cons bound-1 bound-2
                          st'' = ((ar, (cid, c) : susps) : st', n) 
                      in  (ne, st'')
        put nextST
        trace "Case" $
          eval nextE funs
      EVar var -> do
        -- Case variable is in formal parameteres of a function
          (st'', n) <- get
          let topFR@(ar, susps) = head st''
              (funName, stArgs) = ar
              i = case Map.lookup funName funs of
                    Nothing -> error "No function definition found"
                    Just (formals, _) -> 
                      let justVars = Data.List.map fst formals
                      in  fromMaybe (error $ "variable not in formals: Var = " ++ var) (elemIndex var justVars)
              (v, s) = 
                if i > length stArgs then error "i: out of bounds"
                else  case stArgs !! i of
                        StrictArg v   -> (v, Nothing)
                        ByNameArg e   -> 
                          let (v, s) = runState (eval e funs) (tail st'', n) 
                          in  (v, Just s)
                        LazyArg e b val -> 
                            if b then (fromJust val, Nothing)
                                 else   let (v', (newSt, n')) = runState (eval e funs) (tail st'', n)
                                            stArgs' = replaceNth i (LazyArg e True (Just v')) stArgs
                                        in  (v', Just (((funName, stArgs'), susps) : newSt, n'))
          case s of
              Just s' -> put s'
              Nothing -> modify id
          trace "EVar" $
            return v
      Call funName actuals -> do
            st@(stack@((ar, susps) : _), n) <- get
            let (formals, funBody) = 
                  case Map.lookup funName funs of
                    Nothing     -> error $ "Call function: " ++ funName ++ " does not exist"
                    Just (f, e) -> (f, e)
                (stackFrame, (stack', stNum')) = makeStackFrame actuals formals funs ([], st)
                newAR = (funName, stackFrame)
                -- Add frame to stack
                newSt = (newAR, []) : stack'
            put (newSt, stNum' + 1)
            trace "Call" $
              eval funBody funs
      ConstrF tag exprs -> do 
        (st, _) <- get 
        trace "ConstrF" $
          return $ VC (Susp (tag, exprs) st)
      CProj cid cpos -> do 
        (st, n) <- get
        let (ar, susps) = head st
            (cn, el, stSusp) = 
              case L.lookup cid susps of 
                Nothing -> error $ "CProj - not in susps, susps = " ++ show susps 
                Just (Susp (cn, el) stSusp) -> (cn, el, stSusp)
            nextE = if cpos >= length el then ConstrF "Nil" [] else el !! cpos
            (val, (stSusp', n')) = runState (eval nextE funs) (stSusp, 0)
            el' = 
              case val of
                VI v -> replaceNth cpos (EInt v) el
                VC c -> error $ "Constructor " ++ show c
            newSusp = Susp (cn, el') stSusp'
            newSusps = updateL cid newSusp susps 
        put ((ar, newSusps) : tail st, n + n') 
        stack <- get
        trace ("CProj: val = " ++ show val ++ ", \nsusp = " ++ show newSusp ++ ",\nsusps = " ++ show susps) $ 
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
                        -> [StackFrameArg]      -- previous stackframe to be mutated
                        -> (CallStack, Integer) -- new stack
          checkMutate actuals@(a : as) formals@(f : fs) args 
            -- Case 1: True if actuals not dependent by caller's formals
            | actualsFS callerFormals actuals =
                  let (args', (s : ss, frNum)) = makeStackFrame actuals formals funs ([], st)
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
        -> [StackFrameArg]                          -- current stack frame
        -> Int                                      -- index in actuals
        -> FunctionsMap                             -- map K: function name, V: (formals, body)
        -> [Expr]                                   -- actuals
        -> ([StackFrameArg], (CallStack, Integer))  -- acc (stackFrame, state)
        -> ([StackFrameArg], (CallStack, Integer))  -- new (stackFrame, state)
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

makeStackFrame :: [Expr] 
                -> [Formal] 
                -> FunctionsMap 
                -> ([StackFrameArg], (CallStack, Integer))
                -> ([StackFrameArg], (CallStack, Integer))
makeStackFrame [] [] _ (args, st) = (reverse args, st)
makeStackFrame (actual : actuals) (formal : formals) funs (frames, st) = 
  case snd formal of 
    CBV    -> 
      let (v, st') = runState (eval actual funs) st
          frames'  = StrictArg { val = v } : frames
      in  makeStackFrame actuals formals funs (frames', st')
    CBN    -> 
      let frames' = ByNameArg { expr = actual } : frames
      in  makeStackFrame actuals formals funs (frames', st)
    G.Lazy -> 
      let frames' = LazyArg { expr = actual, isEvaluated = False, cachedVal = Nothing } : frames
      in  makeStackFrame actuals formals funs (frames', st)



-- construct dictionary(map) where 
  -- K: function name, 
  -- V: (formal parameters, expression)
functionMap
  -- + program from parsing
  :: Program
  -- + map K V where K: function name, V: (formals, expr)
     -> Map.Map String ([(String, Type)], Expr)
  -- - map K V where K: function name, V: (formals, expr)
     -> Map.Map String ([(String, Type)], Expr)
functionMap program _map =
    foldr (\(Fun x y z) w ->
            case Map.lookup x _map of
              Nothing -> Map.insert x (y, z) w
              Just s  -> error $ "redeclaration of function " ++ show s) _map program


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