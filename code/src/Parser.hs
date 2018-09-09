module Parser (
  parseExpr,
  parseProgram,
  program
 ) where

import Grammar
import Lexer

import Text.Parsec.Expr
import Text.Parsec
import Text.Parsec.String

import Data.Functor
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import Control.Monad

eint = EInt <$> integer

evar = EVar <$> identifier

binaryOp ch fun = Infix $ reservedOp ch $> fun
unaryOp ch fun = Prefix $ reservedOp ch $> fun
opAssoc = [
            [
              unaryOp "+" EUnPlus,
              unaryOp "-" EUnMinus
            ],
            [
              binaryOp "*" EMul AssocLeft,
              binaryOp "/" EDiv AssocLeft,
              binaryOp "%" EMod AssocLeft
            ],
            [
              binaryOp "+" EAdd AssocLeft,
              binaryOp "-" ESub AssocLeft
            ]
          ]
opExpr = buildExpressionParser opAssoc term 

ifExpr =
  reserved "if" *> (
    Eif <$>
      expression <*> (reserved "then" *> expression) <*> (reserved "else" *> expression))

callExpr = do
  reserved "call"
  (x : xs) <- sepBy1 expression whitespace
  case x of (EVar y)  -> if head y /= '#' && head y /= '!' then return $ Call y xs
                         else fail "call: function name starts with a letter"
            _ -> error "Function call"


functionDef = reserved "fun" *> do
    ids <- sepBy1 identifier whitespace
    reservedOp "="
    e <- expression
    case ids of []       -> error "Function must have a name"
                (x : xs) -> if head x /= '#' && head x /= '!' then return $ Fun x (getFormals xs) e
                            else fail "function name starts with a letter"
  where
    getFormals = 
      map (
        \x -> (case head x of
                  '#'  -> (tail x, CBN)
                  '!'  -> (tail x, CBV)
                  _    -> (x, Lazy)))

sequenceOfFns = sepBy1 functionDef semi

term = eint
   <|> evar
   <|> parens expression

expression = callExpr
   <|> ifExpr
   <|> opExpr
   <|> term


program = sequenceOfFns

parseExpr :: String -> IO Expr
parseExpr s =
  case parse (expression <* eof) "" s of
    Right e  -> return e
    Left err -> error $ "parsing expression " ++ show err

parseProgram :: String -> IO Program
parseProgram prog = case parse (program <* eof) "" prog of
                      Right e  -> return e
                      Left err -> print err >> fail "parse error"

