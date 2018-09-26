module Main where

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Parser
import Grammar
import StateInterpreter
import TailCalls

import Text.Parsec.String

main :: IO ()
main = do
  s <- getContents
  p <- parseProgram s   -- :: IO Program
  let p' = correctCaseP p 0
  let   -- ap                            = spotTCs p
        (result, stack, framesNum)    = run p
        -- (result', stack', framesNum') = run ap

  print p'   -- ast 
  print $ "Result is: " ++ show result ++ ", StackFrames used: " ++ show framesNum 

{-
  print ap  -- TC annotated AST
  
  print $ "Result is: " ++ show result ++ ", StackFrames used: " ++ show framesNum 
  print $ "CallStack: " ++ show stack'
  print $ "Result' is: " ++ show result' ++ ", StackFrames' used: " ++ show framesNum' 
  print $ "CallStack: " ++ show stack
-}


