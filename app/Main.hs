module Main (main) where

import Lib (clex, compile, parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  [cFile, asmFile] <- getArgs
  cProgram <- readFile cFile
  print cProgram

  let tokens = clex cProgram
  print tokens

  let ast = parse tokens
  print ast

  let asm = compile ast
  print asm
  writeFile asmFile asm
