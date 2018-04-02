module Main where

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Parser
import Grammar
import PPrint
import Interpreter
import Text.Parsec.String

main :: IO ()
main = do
  s <- getContents
  p <- parseProgram s -- :: IO Program
  run p



