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

import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import Control.Monad

eint = EInt <$> integer

evar = EVar <$> identifier

binaryOp ch fun assoc = Infix (reservedOp ch *> return (\x y -> fun x y)) assoc
opAssoc = [
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
    (\x y z -> Eif x y z) <$>
      expr <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
  )

callExpr = do
  reserved "call"
  (x : xs) <- sepBy1 expr whitespace
  case x of (EVar y)  -> if head y /= '#' && head y /= '!' then return $ Call y xs
                         else fail "call: function name starts with a letter"
            otherwise -> error "Function call"


functionDef = reserved "fun" *> do
    ids <- sepBy1 identifier whitespace
    reservedOp "="
    e <- expr
    case ids of []       -> error "Function must have a name"
                (x : xs) -> if head x /= '#' && head x /= '!' then return $ Fun x (getFormals xs) e
                            else fail "function name starts with a letter"
  where
    getFormals l = map (\x -> (case head x of
                                  '#'  -> (tail x, CBN)
                                  '!'  -> (tail x, CBV)
                                  otherwise -> (x, Lazy)
                              )) l

sequenceOfFns = do
  l <- sepBy1 functionDef semi
  return $ Seq l

term = eint
   <|> evar
   <|> parens expr

expr = callExpr
   <|> ifExpr
   <|> opExpr
   <|> term


program = sequenceOfFns

--parseExpr :: String -> IO Expr
parseExpr s =
  case parse (expr <* eof) "" s of
    Right e  -> return e
--    Left err -> return

--parseProgram :: String -> IO Program
parseProgram prog = case parse (program <* eof) "" prog of
                      Right e  -> return e
                      Left err -> print err >> fail "parse error"

