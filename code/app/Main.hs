module Main where

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Parser
import Grammar
import PPrint
import StateInterpreter
import TailCalls

import Text.Parsec.String

main :: IO ()
main = do
  s <- getContents
  p <- parseProgram s -- :: IO Program
  let ap                    = spotTCs p
      (result, stack, framesNum) = run p 
  print p   -- ast 
  print ap  -- tail call annotation
  print $ "Result is: " ++ show result ++ ", StackFrames used: " ++ show framesNum 



