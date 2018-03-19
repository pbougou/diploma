module Parser(
  parseExpr,
  parseProgram
 ) where

import Grammar
import Lexer

import Text.Parsec.Expr hiding (Empty)
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
  case x of (EVar y)  -> return $ Call y xs
            otherwise -> error "Function call"


functionDef = reserved "fun" *> do
    ids <- sepBy1 identifier whitespace
    reservedOp "="
    e <- expr
    case ids of []       -> error "Function must have a name"
                (x : xs) -> return $ Fun x (getFormals xs) e
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

--parseExpr :: String -> Either ParseError Expr
parseExpr s =
  case parse (expr <* eof) "" s of
    Right e  -> return e
    Left err -> print err >> fail "parse error"

parseProgram :: String -> Either ParseError Program
parseProgram = parse (program <* eof) ""