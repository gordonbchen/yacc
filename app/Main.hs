module Main (main) where

import Lib (clex)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  cProgram <- readFile $ head args
  print cProgram

  let tokens = clex cProgram
  print tokens
