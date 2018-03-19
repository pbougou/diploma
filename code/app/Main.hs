module Main where

import Parser

main = do
  s <- getContents
  return $ parseProgram s
