module Main (main) where

import Lib (clex, parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  cProgram <- readFile $ head args
  print cProgram

  let tokens = clex cProgram
  print tokens

  let ast = parse tokens
  print ast
