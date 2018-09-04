module Main where

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Parser
import Grammar
import PPrint
-- import Interpreter hiding (run)
import StateInterpreter

import Text.Parsec.String

main :: IO ()
main = do
  s <- getContents
  p <- parseProgram s -- :: IO Program
  print $ run p



