module Interpreter (
  StackFrameArg(..),
  StackFrame,
  ArithmeticException(..),
  eval,
  evalFrameArg,
  functionMap,
  run
) where

import Debug.Trace

import Data.Maybe
import Data.List

import Data.Map.Strict hiding (foldr)
import qualified Data.Map.Strict as Map

import Control.Lens
import qualified Control.Lens as Lens

import Control.Monad.Except

import Grammar
import PPrint

type StackFrame = (String, [StackFrameArg])
data StackFrameArg = StrictArg { val :: Integer }
                   | ByNameArg { expr :: Expr }
                   | LazyArg   { expr :: Expr, isEvaluated :: Bool, cachedVal :: Maybe Integer }
  deriving Show
  
-- exception for division by zero
newtype ArithmeticException = DivideByZero String
instance Show ArithmeticException where
  show (DivideByZero s) = s ++ ": Divide by zero"
-- result of eval can be either arithmetic exception or a value(integer)
type EvalMonad = Either ArithmeticException

run :: Program -> Integer
run ast =
  let functionsMap = functionMap ast Map.empty
      -- handle result of eval
      result :: EvalMonad Integer -> Integer
      result (Right n)        = n
      result (Left exception) = error $ show exception
  in case ast of
        Fun x formals expr ->
          case x of
            "main"    -> result $ eval expr functionsMap [("main", [])]
            _ -> error "main not found"
        Seq functions ->
          case Map.lookup "main" functionsMap of
            Nothing              -> error "main not found"
            Just (actuals, expr) -> 
              case actuals of
                [] -> result $ eval expr functionsMap [("main", [])]
                _  -> error "main takes no arguments"



eval 
  -- expression to be evaluated
  :: Expr 
      -- map of functions:
      --    map of functions where keys are function names and values are a tuple of formal parameters 
      --    and function body
      -> Map.Map String ([Formal], Expr) 
      -- call stack
      -> [StackFrame]
      -- output: it can be a value(integer) or an exception 
      -> EvalMonad Integer
eval e fm st | trace ("eval " ++ " " ++ show st) False = undefined
eval e fm st =
  -- trace ("eval " ++ " " ++ show st ++ " " ++ show e) $
  case e of
    EVar var -> do
      let ar = head st -- assume stack is not empty during evaluation
          (fName, stfr) = ar
          i = case Map.lookup fName fm of
                Nothing -> error "Var function lookup: Something is really wrong"
                Just (formals, _) -> let justVars = Data.List.map fst formals in
                  fromMaybe (error "variable not in formals") (elemIndex var justVars)
      evalFrameArg i stfr fm st
    EInt n -> return n
    EUnPlus e -> eval e fm st
    EUnMinus e -> do
      v <- eval e fm st
      return $ -v
    EAdd l r -> do
      vl <- eval l fm st
      vr <- eval r fm st
      return $ vl + vr
    ESub l r -> do
      vl <- eval l fm st
      vr <- eval r fm st
      return $ vl - vr
    EMul l r -> do
      vl <- eval l fm st
      vr <- eval r fm st
      return $ vl * vr
    EDiv l r -> do
      vl <- eval l fm st
      vr <- eval r fm st
      if vr == 0 then throwError $ DivideByZero "div"
                 else return $ vl `div` vr
    EMod l r -> do
      vl <- eval l fm st
      vr <- eval r fm st
      if vr == 0 then throwError $ DivideByZero "mod"
                 else return $ vl `mod` vr
    Eif c l r -> do
      vc <- eval c fm st
      if vc /= 0 then eval l fm st
                 else eval r fm st
    Call fName actuals -> do
      let
          -- transforms actuals to arguments(CBV, CBN, Lazy) in stack frame
          call :: [Expr] -> [Formal] -> [StackFrame] -> [StackFrameArg]
          call []                 []                 _  = []
          call (actual : actuals) (formal : formals) st =
            case snd formal of
              CBV -> StrictArg {
                val = case eval actual fm st of
                  Right res -> res
                  Left exception -> error $ "Error: " ++ show exception }
              CBN          -> ByNameArg { expr = actual }
              Grammar.Lazy -> LazyArg { expr = actual, isEvaluated = False, cachedVal = Nothing }
            : call actuals formals st

          (formals, fexpr) = 
            case Map.lookup fName fm of
              Nothing -> error $ "Call function: " ++ fName ++ " does not exist"
              Just (f, e) -> (f, e)
          stackFrame = call actuals formals st
      eval fexpr fm ((fName, stackFrame) : st)

-- mutate :: TailCallInformation -> StackFrame -> StackFrame

-- st -> (mutate (head st)) ++ (tail st)

evalFrameArg 
  -- position of variable in function' s formal parameters
  :: Int 
  -- current stack frame 
  -> [StackFrameArg] 
  -- functions' map
  -> Map String ([Formal], Expr) 
  -> [StackFrame]
  -- result either an exception or a value(integer)
  -> EvalMonad Integer
-- evalFrameArg i frame functionsMap st | trace (show frame) False = undefined 
evalFrameArg i frame functionsMap st = evalArg i (frame !! i)
  where
    evalArg :: Int -> StackFrameArg -> EvalMonad Integer
    evalArg i argType =
      case argType of
        StrictArg v -> return v
        ByNameArg e -> eval e functionsMap $ tail st
        LazyArg e b v ->
          if b then return $ fromJust v
               else eval e functionsMap $ tail st
                  -- let first : rest = st
                  -- eval e functionsMap $ tail st
                  -- eval e functionsMap ((fst first, frame & Lens.element i .~ LazyArg e True v') : rest)



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
  case program of
    Fun fName formals expr -> Map.insert fName (formals, expr) _map
    Seq programs ->
      foldr (\(Fun x y z) w ->
            case Map.lookup x _map of
              Nothing -> Map.insert x (y, z) w
              Just s  -> error $ "redeclaration of function " ++ show s
      ) _map programs





