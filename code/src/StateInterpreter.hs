{-# LANGUAGE LambdaCase #-}

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
  functionMap,
  interpret,
  step
) where

import Grammar as G
import RuntimeStructs

import AuxAnalysis

import Debug.Trace

import Data.Maybe

import Data.List(map, elemIndex, lookup, foldr, unzip, filter)
import qualified Data.List as L

import Data.Map.Strict hiding(foldr)
import qualified Data.Map.Strict as Map

import Control.Monad.State.Strict

import System.IO.Unsafe
import Heap
import IntermediateTransform

-- -- Local environment : Map: [variables -> values]
-- type Env = Map VN VMValue
-- -- No # arguments for now
-- data VMValue =  VInt Integer
--               | VAddr Addr
--   deriving Show
-- type FValue = VMValue
-- -------------------------------------------------
-- -- 1. CAF: top-level function with no arguments
-- -- 2. HFun: top-level function with arguments
-- -- 3. HCons: constructor suspension
-- -- 4. Thunk: function built by let(updatable)
-- -------------------------------------------------
-- data HNode = 
--         CAF Code
--     |   FunH [Formal] Code
--     |   Thunk FreeVars Code [Formal] [FValue]
--     |   Cons [VN] [VMValue]
--   deriving Show
-- type Stack = [AR]
-- type AR = (FN, [VMValue])
-- data VMSTate = VMSTate {
--   globals :: Globals,
--   heap :: Heap Block,
--   stack :: Stack,
--   nrFrames :: NRFrames
-- }
dom :: [VN] -> Env -> [VMValue]
dom [] env = []
dom (v : vs) env = 
  let Just val = Map.lookup v env
  in  val : dom vs env


interpret :: Program -> VMValue
interpret ast = 
  let (g, heapInit) = buildHeapGlobals ast
      Just mainAddr = Map.lookup "main" g
      Left (CAF funbody) = hLookup heapInit mainAddr
      envInit = Map.empty
      stackInit = [("main", [])]
      initState = VMSTate { globals = g, heap = heapInit, stack = stackInit, nrFrames = 1 }
      value = evalState (step funbody envInit) initState
  in  value

step :: Expr
        -> Env
        -> State VMSTate VMValue
step expr env = do
  state@(VMSTate g h s n) <- get
  trace ( let (_, _, heap) = h in
          "\n****************************EXPRESSION******************************\n" ++ 
          show expr ++
          "\n*************************** GLOBALS*********************************\n" ++
          show (assocs g) ++
          "\n****************************HEAP************************************\n" ++
          show (assocs heap) ++
          "\n****************************STACK***********************************\n" ++ 
          show s ++
          "\n****************************LOCAL_ENV*******************************\n" ++
          show (assocs env)) $
    case expr of
      EInt n -> 
        return (VInt n)
      UnaryOp unaryArithm e ->  do
        VInt v <- step e env
        case unaryArithm of 
          EUnPlus -> 
            return $ VInt v
          EUnMinus ->
            return $ VInt (-v)
      BinaryOp binArithm l r -> do
        VInt vl <- step l env
        VInt vr <- step r env
        case binArithm of
          EAdd -> return $ VInt (vl + vr)
          ESub -> return $ VInt (vl - vr)
          EMul -> return $ VInt (vl * vr)
          EDiv ->
            if vr == 0  then error "Division by zero"
                        else return $ VInt (vl `div` vr)
          EMod ->
            if vr == 0  then error "Modulo zero"
                        else return $ VInt (vl `mod` vr)
      Eif c l r -> do
        c' <- step c env
        let vc = case c' of
                    VInt v -> v
                    VAddr addr -> error "IF: Good"
        if vc /= 0  then step l env
                    else step r env
      Call funName actuals -> do
        state@(VMSTate g h s n) <- get
        -- 1. build argument stack
        -- 2. build new local environment
        let Just funAddr = Map.lookup funName g
            FunH formals funbody = 
              case hLookup h funAddr of
                Left l -> l
                Right r -> error ("Function call " ++ r)

            -- Assuming partial application case does not exist
            --  This means that len(actuals) = len(formals)
            -- !!!!!!!!!!! New state !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            buildArgsEnv :: [Formal] 
                            -> [Actual] 
                            -> ([(VN, VMValue)], [VMValue]) 
                            -> ([(VN, VMValue)], [VMValue])
            buildArgsEnv [] [] (newEnv, newAR) = (newEnv, reverse newAR)
            buildArgsEnv (f : fs) (a : as) (newEnv, newAR) =
              let (vn, annoType) = f
                  (val, newState) = 
                    case annoType of
                      -- strict argument forces evaluation
                      CBV -> runState (step a env) state
                      CBN -> error "ByNameArg is not implemented yet"
                      -- lazy argument exists in local environmnet
                      --    a let block should have already built it
                      G.Lazy -> 
                        -- lazy arguments are atoms
                        --  atom ::= var | number
                        case a of
                          EInt n -> (VInt n, state)
                          EVar v -> 
                            (fromMaybe (error $ "Local env: " ++ vn) (Map.lookup v env), state)
                          _ -> runState (step a env) state
                  newEnv' = (vn, val) : newEnv
                  newAR' = val : newAR
              in  buildArgsEnv fs as (newEnv', newAR')
          
            (env', ar) = 
              case actuals of
                [] -> ([], []) 
                _  -> buildArgsEnv formals actuals ([], [])
        VMSTate mg mh ms mn <- get
        let s' = (funName, ar) : ms
        put VMSTate { globals = mg, heap = mh, stack = s', nrFrames = mn }
        trace ("\n\n\nHeap = " ++ show (fst3 mh)) (step funbody (fromList env'))
      EVar var -> do
        state@(VMSTate g h s n) <- get
        -- Case env[var] of
        --    * VInt i -> return i
        --    * VAddr addr -> 
        --        ** this happens because variable was bound by a let case
        --        ** force evaluation of thunk in address env[var]
        --        ** env[var] = eval(heap(env[var])) 

        return (case Map.lookup var env of
                  Just var' ->
                    case var' of
                      VInt i -> VInt i
                      VAddr addr -> 
                        let  
                            (Thunk varsf e fvalues) = 
                              case hLookup h addr of
                                Left l -> l
                                Right r -> error ("Variable: " ++ r) 
                            tempEnv = fromList (zip varsf fvalues)
                            evaluated = evalState (step e tempEnv) state
                            env' = Map.insert var evaluated
                        in  evaluated
                  Nothing -> error ("Unbound variable: " ++ var))
      
      Let bindings expr -> do
        state@(VMSTate g h s n) <- get
        let 
            extendHeap :: Bindings -> (Env, Heap Block)
            extendHeap binds = 
              let bindsAssoc = assocs binds
                  extendHeap' :: [(VN, (FreeVars, Expr))] -> (Env, Heap Block) -> (Env, Heap Block) 
                  extendHeap' [] (locals, h) = (locals, h)
                  extendHeap' (b : bs) (locals, h) = 
                    let (vn, (varsf, e)) = b
                        (h', addr) = hAlloc h (Thunk varsf e (dom varsf env)) -- (e, Lambda, varsf, [], dom varsf env)
                        locals' = Map.insert vn (VAddr addr) locals
                    in  extendHeap' bs (locals', h')
              in  extendHeap' bindsAssoc (env, h) 
            (env', heap') = extendHeap bindings
        put VMSTate { globals = g, heap = heap', stack = s, nrFrames = n }
        step expr env'
      -- Lazy data construction
      ConstrF tag exprs -> do
        state@(VMSTate g h s n) <- get
        -- assume exprs are only variables
        -- Find variables in local environment and 
        -- allocate in heap a constructor building block
        let 
            envOfFreevars :: [Expr] -> [(VN, FValue)]
            envOfFreevars = 
              L.map (\case
                          EVar var -> 
                            let Just val = Map.lookup var env
                            in  (var, val)
                          val@(EInt y) -> error "It should have been filtered"
                          _ -> error "Constructor arguments are atoms")
            
            noIntsExprs = L.filter (\case EInt y -> False
                                          _ -> True) exprs
            (fvars, fvalues) = L.unzip (envOfFreevars noIntsExprs)
            (heap', addr) = hAlloc h (Cons fvars tag fvalues)
        put VMSTate { globals = g, heap = heap', stack = s, nrFrames = n } 
        return (VAddr addr)
      CaseF cid e cases -> do
        let (evalE, st) = runState (step e env) state
          -- nextST: next stack state
          -- nextE:  expression to be evaluated next
            (nextE, nextST) = 
              case evalE of
          -- 1. evalE is an expression 
                VInt i -> 
                  let nextE = fromMaybe (error "Case: Patterns must be integers") (L.lookup (IPat i) cases)
                  in  (nextE, st)
          -- 2. evalE is a constructor 
                VAddr c ->  let Cons varsf tag fvalues = 
                                  case hLookup h c of
                                    Left l -> l
                                    Right r -> error ("Case: " ++ r)
                                patterns = L.map fst cases
                                pattIndex = indexOfPattern tag patterns 0
                                (patt, ne) = cases !! pattIndex

                                -- bindPatVars :: Pattern -> [(VN, VMValue)]
                                -- bindPatVars pat = 
                                --   case pat of
                                --     IPat _ -> error "Constructor pattern: Integer literal"
                                --     CPat tag vars -> L.map bindPatVar vars
                                -- bindPatVar :: VN -> VMValue
                                -- bindPatVar var = 
                                --   case Map.lookup var env of
                                --     Just val -> val
                                --     Nothing -> error ""

                                -- st'' = ((ar, (cid, c) : susps) : st', n) 
                            in  (ne, state)
        put nextST
        step nextE env

fst3 (_, _, x) = x

-- Programs are considered type safe  ===  (No type check)
-- Also do not name your functions cons 
run :: Program -> (Value, CallStack, Integer)
run ast = 
    let functions = functionMap ast Map.empty
    in  case Map.lookup "main" functions of
            Nothing              -> error "main not found"
            Just (actuals, expr) -> let (v, s) = runState (eval expr functions) ([(("main", []),[])], 1)
                                    in  (v, fst s, snd s)

eval :: Expr                        -- expression to be evaluated
    ->  FunctionsMap                -- dictionary (K: function name, V: (formals, body))
    ->  State 
              (CallStack, NRFrames) -- State: execution stack, number of stackframes allocated 
              Value                 -- Value: evaluation result
eval e funs = do
  (stack, nFrames) <- get
  trace ( "\n****************************EXPRESSION******************************\n" ++ 
          show e ++
          "\n********************************STACK*******************************\n" ++ 
          showStack stack) $
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
                        let (v, s) = runState (eval e funs) ({- tail -} st'', n) 
                        in  (v, Just s)
                      LazyArg e b val -> 
                          if b then (fromJust val, Nothing)
                                else  let (v', (newSt, n')) = runState (eval e funs) (tail st'', n)
                                          stArgs' = replaceNth i (LazyArg e True (Just v')) stArgs
                                      in  (v', Just (((funName, stArgs'), susps) : newSt, n'))
        case s of
          Just s' -> put s'
          Nothing -> modify id
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
        curST@((ar, susps) : st', n) <- get 
        let (evalE, st) = runState (eval e funs) curST
        put st
        let 
          -- nextST: next stack state
          -- nextE:  expression to be evaluated next
          (nextE, nextST) = 
            case evalE of
          -- 1. evalE is an expression 
              VI i -> let nextE = fromMaybe (error "Case: Patterns must be integers") (L.lookup (IPat i) cases)
                      in  (nextE, st)
          -- 2. evalE is a constructor 
              VC c -> let Susp (cn, _) _ = c
                          patterns = L.map fst cases
                          pattIndex = indexOfPattern cn patterns 0
                          (_, ne) = cases !! pattIndex
                          st'' = ((ar, (cid, c) : susps) : st', n) 
                      in  (ne, st'')
        put nextST
        eval nextE funs
      ConstrF tag exprs -> do 
        (st, _) <- get 
        return $ VC (Susp (tag, exprs) st)
      CProj cid cpos -> do 
        (st, n) <- get
        let (ar, susps) = head st
            (cn, el, stSusp) = 
              case L.lookup cid susps of 
                Nothing -> error $ "CProj - not in susps, susps = " ++ show susps 
                Just (Susp (cn, el) stSusp) -> (cn, el, stSusp)
            len = length el
            nextE = 
              if cpos >= len then ConstrF "Nil" [] 
              else el !! cpos
            (val, (stSusp', n')) = runState (eval nextE funs) (stSusp, 0)
            el' = 
              case val of
                VI v -> replaceNth cpos (EInt v) el
                VC c -> el -- error $ "Constructor " ++ show c
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

indexOfPattern cn patterns i = 
  case patterns of
    [] -> error $ "Patterns are assumed exhaustive: " ++ cn ++ " not in patterns"
    patt : patts -> 
      case patt of
        CPat cn' vars -> 
          if cn == cn' then i else indexOfPattern cn patts (i + 1)
        _ -> error "Searching for integer patterns in constructor case"