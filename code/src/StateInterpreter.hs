module StateInterpreter (
  StackFrameArg(..),
  StackFrame(..),
  FunctionsMap(..),
  CallStack(..),
  run,
  eval,
  replaceNth,
  functionMap
) where

import Grammar
import PPrint

import Debug.Trace

import Data.Maybe
import Data.List
import Data.Map.Strict hiding(foldr)
import qualified Data.Map.Strict as Map

import Control.Monad.State

import System.IO.Unsafe

type StackFrame = (String, [StackFrameArg])
data StackFrameArg = StrictArg { val :: Integer }
                   | ByNameArg { expr :: Expr }
                   | LazyArg   { expr :: Expr, isEvaluated :: Bool, cachedVal :: Maybe Integer }
  deriving Show

type FunctionsMap = Map.Map String ([Formal], Expr)
type CallStack = [StackFrame]


run :: Program -> (Integer, CallStack, Integer)
run ast = 
    let functions = functionMap ast Map.empty
    in  case Map.lookup "main" functions of
            Nothing              -> error "main not found"
            Just (actuals, expr) -> let (v, s) = runState (eval expr functions) ([("main", [])], 0)
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
            
            makeStackFrame :: [Expr] -> [Formal] -> [StackFrameArg]
            makeStackFrame [] [] = []
            makeStackFrame (actual : actuals) (formal : formals) = 
                case snd formal of 
                    CBV          -> StrictArg {
                                      val = evalState (eval actual funs) st
                                    }
                    CBN          -> ByNameArg { expr = actual }
                    Grammar.Lazy -> LazyArg { expr = actual, isEvaluated = False, cachedVal = Nothing }
                : makeStackFrame actuals formals
            
            stackFrame = makeStackFrame actuals formals
        
        -- Add frame to stack
        (st', sts') <- get
        let newSt = (funName, stackFrame) : st' 
        put (newSt, sts' + 1)

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
    TailCall funName actuals -> error $ "Tail call not implemented yet " ++ funName ++ " " ++ show actuals 

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