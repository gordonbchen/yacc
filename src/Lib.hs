module Lib (clex) where

import Data.Char (isAlpha, isDigit)

data Token
  = OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | SemiColon
  | IntKeyword
  | ReturnKeyword
  | IntLiteral Int
  | Identifier String
  deriving (Show)

clex :: String -> [Token]
clex = clex' []

clex' :: String -> String -> [Token]
clex' buf [] = maybeToken buf
clex' buf (h : t)
  | h `elem` [' ', '\n', '\t'] = maybeToken buf ++ clex' [] t
  | h `elem` "{}();" = maybeToken buf ++ maybeToken [h] ++ clex' [] t
  | otherwise = clex' (buf ++ [h]) t

maybeToken :: String -> [Token]
maybeToken buf = [toToken buf | not (null buf)]

toToken :: String -> Token
toToken "{" = OpenBrace
toToken "}" = CloseBrace
toToken "(" = OpenParen
toToken ")" = CloseParen
toToken ";" = SemiColon
toToken "int" = IntKeyword
toToken "return" = ReturnKeyword
toToken buf
  | all isDigit buf = IntLiteral (read buf :: Int)
  | all isAlpha buf = Identifier buf
  | otherwise = error ("Failed to convert to token: " ++ buf)
