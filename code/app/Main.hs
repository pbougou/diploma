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
    let p'   = correctCaseP p             -- | annotate case with ids
        p''  = scopingP p'                -- | transform to CProj
        (result, _, nr_frames) = run p''  -- | Evaluation
        ap = analysis p''
        (resultTC, _, nr_frames_tc) = run ap 
        -- (tcocands, _) = spotTCs p''
        -- (resultTCOCands, _, nr_frames') = run tcocands
    putStrLn "================================="
    putStrLn "=====Abstract Syntax Tree========"
    putStrLn "================================="
    print p'' -- annotated source program with cons wrappers
    putStrLn "================================="
    putStrLn "===========Interpeter============"
    putStrLn "================================="
    putStrLn $ "Result is: " ++ show result ++ ", frames used: " ++ show nr_frames
    putStrLn "================================="
    putStrLn "==========TC-POSITIONS==========="
    putStrLn "================================="
    print ap
    putStrLn "================================="
    putStrLn "===========InterpetTC============"
    putStrLn "================================="
    putStrLn $ "Result is: " ++ show resultTC ++ ", frames used: " ++ show nr_frames_tc



