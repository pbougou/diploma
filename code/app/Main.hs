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
    p <- parseProgram s                         -- :: IO Program
    let p'   = correctCaseP p                   -- annotate case with ids
        p''  = scopingP p'                      -- transform to CProj
        p''' = wrapConsP p''                    -- transform constructors 
        (result, stack, framesNum) = run p'''   -- Evaluation
        ap = spotTCs p                          -- trace tail call positions
        (result', stack', framesNum') = run ap  

    -- print p'   -- ast 
    putStrLn "================================="
    putStrLn "=====Abstract Syntax Tree========"
    putStrLn "================================="
    print p''
    putStrLn "================================="
    putStrLn "===========Interpeter============"
    putStrLn "================================="
    putStrLn $ "Result is: " ++ show result ++ ", frames used: " ++ show framesNum 
    putStrLn "================================="
    putStrLn "==========TC-POSITIONS==========="
    putStrLn "================================="
    print ap
    putStrLn "================================="
    putStrLn "===========InterpetTC============"
    putStrLn "================================="
    putStrLn $ "Result is: " ++ show result' ++ ", frames used: " ++ show framesNum'

{-
  print ap  -- TC annotated AST
  
  print $ "Result is: " ++ show result ++ ", frames used: " ++ show framesNum 
  print $ "CallStack: " ++ show stack'
  print $ "Result' is: " ++ show result' ++ ", frames' used: " ++ show framesNum' 
  print $ "CallStack: " ++ show stack
-}


