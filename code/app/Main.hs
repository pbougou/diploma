module Main where

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Parser
import Grammar
import StateInterpreter
import IntermediateTrans
import TailCalls

import Text.Parsec.String

main :: IO ()
main = do
    s <- getContents
    p <- parseProgram s :: IO Program
    let p'   = correctCaseP p                 -- | annotate case with ids
        p''  = scopingP p'                    -- | transform to CProj
        p''' = wrapConsP p''                  -- | transform constructors 
                                              -- | builtin wrapper functions for constructors
        (result, stack, framesNum) = run p''  -- | Evaluation
        ap = analysis p''
        ap' = wrapConsP ap
        (result', stack', framesNum') = run ap'  
    
    putStrLn "================================="
    putStrLn "=====Abstract Syntax Tree========"
    putStrLn "================================="
    print p'' -- annotated source program with cons wrappers
    putStrLn "================================="
    putStrLn "===========Interpeter============"
    putStrLn "================================="
    putStrLn $ "Result is: " ++ show result ++ ", frames used: " ++ show framesNum 
    -- putStrLn "================================="
    -- putStrLn "==========TC-POSITIONS==========="
    -- putStrLn "================================="
    -- print ap
    -- putStrLn "================================="
    -- putStrLn "===========InterpetTC============"
    -- putStrLn "================================="
    -- putStrLn $ "Result is: " ++ show result' ++ ", frames used: " ++ show framesNum'



