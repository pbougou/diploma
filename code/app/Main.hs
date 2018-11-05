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
    p <- parseProgram s         -- :: IO Program
    let p'   = correctCaseP p   -- annotate case with ids
        p''  = scopingP p'      -- transform to CProj
        p''' = wrapConsP p''    -- transform constructors
        -- Evaluation 
<<<<<<< HEAD
        -- (result, stack, framesNum)    = run p''
=======
        (result, stack, framesNum)    = run p'''
>>>>>>> 7b63704214a8717b671ad83bd0d0266e13e3e352
        -- ap                         = spotTCs p
        -- (result', stack', framesNum') = run ap
        result = interpret p

    -- print p'   -- ast 
    putStrLn "================================="
    putStrLn "=====Abstract Syntax Tree========"
    putStrLn "================================="
    print p
    putStrLn "================================="
    putStrLn "===========Interpeter============"
    putStrLn "================================="
    putStrLn $ "Result is: " ++ show result -- ++ ", StackFrames used: " ++ show framesNum 

{-
  print ap  -- TC annotated AST
  
  print $ "Result is: " ++ show result ++ ", StackFrames used: " ++ show framesNum 
  print $ "CallStack: " ++ show stack'
  print $ "Result' is: " ++ show result' ++ ", StackFrames' used: " ++ show framesNum' 
  print $ "CallStack: " ++ show stack
-}


