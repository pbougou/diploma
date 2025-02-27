module Parser (
  parseExpr,
  parseProgram,
  program,
  sequenceOfFns,
  tint,
  tcons,
  types,
  parseType
 ) where
import Debug.Trace
import Grammar as G
import Lexer
import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L
import Text.Parsec.Expr
import Text.Parsec
import Text.Parsec.String
import Data.Functor
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import Control.Monad
import Control.Monad.State
import qualified Control.Monad.State as ST
import Control.Arrow

eint = EInt <$> integer

evar = EVar <$> identifier

binaryOp ch fun = Infix $ reservedOp ch $> fun
unaryOp ch fun = Prefix $ reservedOp ch $> fun
opAssoc = [
            [
              unaryOp "+" (UnaryOp EUnPlus),
              unaryOp "-" (UnaryOp EUnMinus)
            ],
            [
              binaryOp "*" (BinaryOp EMul) AssocLeft,
              binaryOp "/" (BinaryOp EDiv) AssocLeft,
              binaryOp "%" (BinaryOp EMod) AssocLeft
            ],
            [
              binaryOp "+" (BinaryOp EAdd) AssocLeft,
              binaryOp "-" (BinaryOp ESub) AssocLeft
            ]
          ]
opExpr = buildExpressionParser opAssoc term 

cond = 
      opExpr
  <|> eint
  <|> evar
  <|> parens cond

ifExpr =
  reserved "if" *> (
    Eif <$>
      cond <*> (reserved "then" *> expression) <*> (reserved "else" *> expression))

callExpr = do
  reserved "call"
  (x : xs) <- sepBy1 expression whitespace
  case x of (EVar y)  ->  if head y /= '#' && head y /= '!' 
                            then return $ Call y xs
                            else fail "call: function name starts with a letter"
            _         -> error "Function call"


functionDef = do
    typed <- signature
    _ <- semi
    reserved "fun"
    ids <- sepBy1 identifier whitespace
    reservedOp "="
    e <- expression
    case ids of []       -> error "Function must have a name"
                (x : xs) -> if head x /= '#' && head x /= '!' 
                              then return $ Fun x (getFormals typed xs) e
                              else fail "function name starts with a letter"
  where
    -- TODO: Save result type [DONE].
    getFormals [t] [] = [("", (Lazy, t))]
    getFormals (t : ts) (f : fs) =
      let (vn, eo) = case head f of { '#' -> (tail f, CBN); '!' -> (tail f, CBV); _ -> (f, Lazy) }
      in  (vn, (eo, t)) : getFormals ts fs
    getFormals _ _ = error "Wrong signature"

-------------------------------------------------
-- Support for constructors and pattern matching
-------------------------------------------------
constructor = do 
  reserved "Cons" 
  hd <- expression
  tl <- expression 
  return $ ConstrF "Cons" (hd : [tl])

nilConstr = do
  reserved "Nil"
  return Nil

caseF = do
  reserved "case"
  e <- expression
  reserved "of"
  lines <- many line
  return $ CaseF 42 e lines

consPattern = do
  reserved "Cons"
  EVar a <- evar
  EVar b <- evar
  return CPat { tag = "Cons", vars = [a, b] }

nilPattern = do
  reserved "Nil"
  return CPat { tag = "Nil", vars = [] }

intPattern = do
  patt <- eint
  let v = case patt of
            EInt v' -> v'
            _       -> error "Parser: Pattern should be an integer"
  return IPat { pattVal = v }

line = do
  patt <- consPattern <|> nilPattern <|> intPattern -- pattern 
  reserved "->"
  e <- expression
  return (patt, e)

---------------------------------------------------------------
-- Support type information parsing
---------------------------------------------------------------
tint = do
  reserved "Int"
  return TInt

tcons = do
  reserved "Cons"
  ltype <- tint <|> tcons
  return (TCons ltype)

types = 
  tint
  <|> tcons

signature = do
  reserved "fun"
  fname <- identifier
  reservedOp "::"
  sepBy1 types separrow

separrow = reserved "->"

sequenceOfFns = sepBy1 functionDef semi

term = 
      eint
  <|> evar
  <|> parens expression

expression = 
      callExpr
  <|> constructor
  <|> nilConstr
  <|> caseF
  <|> ifExpr
  <|> opExpr
  <|> term


program = sequenceOfFns

parseType :: String -> IO [Type]
parseType t =
  case parse (signature <* eof) "" t of
    Right t' -> return t'
    Left err -> error "type parsing"

parseExpr :: String -> IO Expr
parseExpr s =
  case parse (expression <* eof) "" s of
    Right e  -> return e
    Left err -> error $ "parsing expression " ++ show err

parseProgram :: String -> IO Program
parseProgram prog = case parse (program <* eof) "" prog of
                      Right e  -> return e
                      Left err -> print err >> fail "parse error"


