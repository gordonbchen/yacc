module Lib where

import Data.Char (isAlpha, isDigit)

-- ---------- LEXING ----------
data Token
  = OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | SemiColon
  | Minus
  | Tilde
  | Exclam
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
  | h `elem` " \n\t" = maybeToken buf ++ clex' [] t
  | h `elem` "{}();-~!" = maybeToken buf ++ maybeToken [h] ++ clex' [] t
  | otherwise = clex' (buf ++ [h]) t

maybeToken :: String -> [Token]
maybeToken buf = [toToken buf | not (null buf)]

toToken :: String -> Token
toToken "{" = OpenBrace
toToken "}" = CloseBrace
toToken "(" = OpenParen
toToken ")" = CloseParen
toToken ";" = SemiColon
toToken "-" = Minus
toToken "~" = Tilde
toToken "!" = Exclam
toToken "int" = IntKeyword
toToken "return" = ReturnKeyword
toToken buf
  | all isDigit buf = IntLiteral (read buf :: Int)
  | all isAlpha buf = Identifier buf
  | otherwise = error ("Failed to convert to token: " ++ buf)

-- ---------- PARSING ----------
data UnaryOperator = NegOp | BitCompOp | LogicalNotOp
  deriving (Show)

data Expr = IntConst Int | UnaryOp UnaryOperator Expr
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
parseExpr (Minus : t) = parseUnaryOp NegOp t
parseExpr (Tilde : t) = parseUnaryOp BitCompOp t
parseExpr (Exclam : t) = parseUnaryOp LogicalNotOp t
parseExpr _ = error "Invalid Expr"

parseUnaryOp :: UnaryOperator -> [Token] -> (Expr, [Token])
parseUnaryOp op t = (UnaryOp op expr, remToks)
  where
    (expr, remToks) = parseExpr t

-- ---------- COMPILING ----------
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
compileStatement (Return expr) = compileExpr expr ++ ["ret"]

compileExpr :: Expr -> [String]
-- movl $2, %eax -> store 2 into eax, lower 32 bits of rax (return value)
compileExpr (IntConst intVal) = ["movl $" ++ show intVal ++ ", %eax"]
compileExpr (UnaryOp NegOp expr) = compileExpr expr ++ ["neg %eax"]
compileExpr (UnaryOp BitCompOp expr) = compileExpr expr ++ ["not %eax"]
-- !expr = 1 if expr == 0 else 0.
-- cmpl $0, %eax -> compute 0 - eax, set flags
-- movl $0 %eax -> zero out eax
-- sete %al -> set al (lsb of eax) to 1 if ZF = 1 (0 == eax)
compileExpr (UnaryOp LogicalNotOp expr) = compileExpr expr ++ ["cmpl $0, %eax", "movl $0, %eax", "sete %al"]
