module StateInterpreter (
  StackFrameArg(..),
  StackFrame(..),
  FunctionsMap(..),
  CallStack(..),
  run,
  eval,
  makeStackFrame,
  replaceNth,
  functionMap
) where

import Grammar as G
import AuxAnalysis

import Debug.Trace

import Data.Maybe

import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L

import Data.Map.Strict hiding(foldr)
import qualified Data.Map.Strict as Map

import Control.Monad.State

import System.IO.Unsafe


run :: Program -> (Integer, CallStack, Integer)
run ast = 
    let functions = functionMap ast Map.empty
    in  case Map.lookup "main" functions of
            Nothing              -> error "main not found"
            Just (actuals, expr) -> let (v, s) = runState (eval expr functions) ([("main", [])], 1)
                                    in  (v, fst s, snd s)  

eval :: Expr                       -- expression to be evaluated
    ->  FunctionsMap               -- dictionary (K: function name, V: (formals, body))
    ->  State 
              (CallStack, Integer) -- State: execution stack, number of stackframes allocated 
              Integer              -- Value: evaluation result
eval e funs =
--   trace ("eval " ++ " " ++ " " ++ show e) $
  case e of
    EInt n -> return n
    EUnPlus e -> eval e funs
    EUnMinus e -> do
      v <- eval e funs
      return $ -v
    EAdd l r -> do
      vl <- eval l funs
      vr <- eval r funs
      return $ vl + vr
    ESub l r -> do
      vl <- eval l funs
      vr <- eval r funs
      return $ vl - vr
    EMul l r -> do
      vl <- eval l funs
      vr <- eval r funs
      return $ vl * vr
    EDiv l r -> do
      vl <- eval l funs
      vr <- eval r funs
      if vr == 0 then error "Division by zero"
                 else return $ vl `div` vr
    EMod l r -> do
      vl <- eval l funs
      vr <- eval r funs
      if vr == 0 then error "Modulo zero"
                 else return $ vl `mod` vr
    Eif c l r -> do
      vc <- eval c funs
      if vc /= 0 then eval l funs
                 else eval r funs
    Call funName actuals -> do
        st <- get
        let (formals, funBody) = 
              case Map.lookup funName funs of
                Nothing     -> error $ "Call function: " ++ funName ++ " does not exist"
                Just (f, e) -> (f, e)
                        
            (stackFrame, (_, stNum')) = makeStackFrame actuals formals funs ([], st)
        
            -- Add frame to stack
            newSt = (funName, stackFrame) : fst st
        put (newSt, stNum' + 1)

        eval funBody funs
        -- trace ("call:  " ++ show newSt) eval funBody funs
    EVar var -> do
        (st'', n) <- get
        let ar = head st''
            (funName, stArgs) = ar

            i = case Map.lookup funName funs of
                Nothing -> error "Var function lookup: Something is really wrong"
                Just (formals, _) -> let justVars = Data.List.map fst formals in
                  fromMaybe (error "variable not in formals") (elemIndex var justVars)

            (v, s) = 
                case stArgs !! i of
                    StrictArg v   -> (v, Nothing)
                    ByNameArg e   -> (evalState (eval e funs) (tail st'', n), Nothing)
                    LazyArg e b val -> 
                        if b then (fromJust val, Nothing)
                             else let (v', (newSt, n')) = runState (eval e funs) (tail st'', n)
                                      stArgs'     = replaceNth i (LazyArg e True (Just v')) stArgs
                                  in  (v', Just ((funName, stArgs') : newSt, n))

        byNameSt <- get
        case s of
            Just s' -> put s'
            Nothing -> put byNameSt

        return v
        -- trace ("var lookup: " ++ show byNameSt) $ return v
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


makeStackFrame :: [Expr] 
                -> [Formal] 
                -> FunctionsMap 
                -> ([StackFrameArg], (CallStack, Integer))
                -> ([StackFrameArg], (CallStack, Integer))
makeStackFrame [] [] _ (args, st) = (reverse args, st)
makeStackFrame (actual : actuals) (formal : formals) funs (frames, st) = 
  case snd formal of 
    CBV          -> 
      let (v, st') = runState (eval actual funs) st
          frames'  = StrictArg { val = v } : frames
      in  makeStackFrame actuals formals funs (frames', st')
    CBN          -> 
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


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0    = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

