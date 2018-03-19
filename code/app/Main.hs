module Main where

import Parser
import Grammar
import PPrint
import Text.Parsec.String

main = do
  s <- getContents
  p <- parseProgram s -- :: IO Program
  print p

