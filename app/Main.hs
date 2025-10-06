module Main (main) where

import Lib (clex, compile, parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  [cFile, asmFile] <- getArgs
  cProgram <- readFile cFile
  putStrLn cProgram

  let tokens = clex cProgram
  print tokens

  let ast = parse tokens
  print ast

  let asm = compile ast
  putStrLn asm
  writeFile asmFile asm
