module Lexer
( -- names,
  -- operators,
  identifier,
  symbol,
  reserved,
  reservedOp,
  parens,
  brackets,
  commaSep,
  integer,
  whitespace,
  semi,
  lexer
) where

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

--names     = words "fun if then else let in"
--operators = words "+ - * / % ="

lexer =
  Token.makeTokenParser emptyDef
  {
    Token.commentStart = "{-",
    Token.commentEnd = "-}",
    Token.nestedComments = True,
    Token.commentLine = "--",
    Token.identStart = letter <|> char '#' <|> char '!',
    Token.identLetter = alphaNum <|> char '_' <|> char '\'',
    Token.reservedNames = ["fun", "if", "then", "else", "call", "Cons", "Nil", "case", "of", "let", "in"],
    Token.reservedOpNames = ["+", "-", "*", "/", "%", "->", "_", "="]
  }

identifier = Token.identifier lexer
symbol     = Token.symbol lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
brackets   = Token.brackets lexer
commaSep   = Token.commaSep lexer
integer    = Token.integer lexer
semi       = Token.semi lexer
whitespace = Token.whiteSpace lexer
