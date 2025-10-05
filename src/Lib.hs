module Lib where

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
  deriving (Show, Eq)

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

data Expr = IntConst Int
  deriving (Show)

data Statement = Return Expr
  deriving (Show)

data Function = Function {fname :: String, fbody :: [Statement]}
  deriving (Show)

data Program = Program [Function]
  deriving (Show)

parse :: [Token] -> Program
parse tokens = case parseFunction tokens of
  (function, []) -> Program [function]
  _ -> error "Tokens after function"

parseFunction :: [Token] -> (Function, [Token])
parseFunction (IntKeyword : Identifier funcName : OpenParen : CloseParen : OpenBrace : t) =
  case parseStatement t of
    (_, []) -> error "No tokens after Statement"
    (statement, CloseBrace : remTok) -> (Function {fname = funcName, fbody = [statement]}, remTok)
    _ -> error "No CloseBrace after Statement"
parseFunction _ = error "Invalid Function"

parseStatement :: [Token] -> (Statement, [Token])
parseStatement (ReturnKeyword : t) =
  case parseExpr t of
    (_, []) -> error "No tokens after Return Expr"
    (expr, SemiColon : remTok) -> (Return expr, remTok)
    _ -> error "Expected SemiColon after Return Expr"
parseStatement _ = error "Invalid Statement"

parseExpr :: [Token] -> (Expr, [Token])
parseExpr (IntLiteral intVal : t) = (IntConst intVal, t)
parseExpr _ = error "Invalid Expr"

compile :: Program -> String
compile (Program funcs) = foldr (\line acc -> line ++ '\n' : acc) "" compiledFuncs
  where
    compiledFuncs = concatMap compileFunction funcs

compileFunction :: Function -> [String]
compileFunction (Function name body) = ["\t.globl " ++ name, name ++ ":"] ++ compiledBody
  where
    compiledBody = concatMap (map indent . compileStatement) body
    indent = (:) '\t'

compileStatement :: Statement -> [String]
compileStatement (Return expr) = ["movl $" ++ compileExpr expr ++ ", %eax", "ret"]

compileExpr :: Expr -> String
compileExpr (IntConst intVal) = show intVal
