module Interpreter (
  StackFrameArg(..),
  ArithmeticException(..),
  eval,
  result
) where

import Control.Monad.Except
import Grammar

type StackFrame = [StackFrameArg]
data StackFrameArg = StrictArg { val :: Int }
                   | ByNameArg { expr :: Expr }
                   | LazyArg   { expr :: Expr, isEvaluated :: Bool, cachedVal :: Int }


data ArithmeticException = DivideByZero String

instance Show ArithmeticException where
  show (DivideByZero s) = s ++ ": Divide by zero"

type EvalMonad = Either ArithmeticException

eval :: Expr -> EvalMonad Integer
eval e =
  case e of
    EInt n -> do
      return n
    EAdd l r -> do
      vl <- eval l
      vr <- eval r
      return $ vl + vr
    ESub l r -> do
      vl <- eval l
      vr <- eval r
      return $ vl - vr
    EMul l r -> do
      vl <- eval l
      vr <- eval r
      return $ vl * vr
    EDiv l r -> do
      vl <- eval l
      vr <- eval r
      if vr == 0 then throwError $ DivideByZero "div"
                 else return $ vl `div` vr
    EMod l r -> do
      vl <- eval l
      vr <- eval r
      if vr == 0 then throwError $ DivideByZero "mod"
                 else return $ vl `mod` vr
    Eif c l r -> do
      vc <- eval c
      if vc /= 0 then do { vl <- eval l; return vl }
                 else do { vr <- eval r; return vr }


result (Right n)        = putStrLn $ show n
result (Left exception) = putStrLn $ "Exception: " ++ show exception


