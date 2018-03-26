module Main where

import Parser
import Grammar
import PPrint
import Interpreter
import Text.Parsec.String

evalExpr = do
  p <- parseExpr "if (1 * 2 + 3 - 1) / 2 - 2 then 42 else 0"
  result $ eval p


main = do
  s <- getContents
  p <- parseProgram s -- :: IO Program
  print p

