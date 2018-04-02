module Grammar(
    Expr (..),
    Program(..),
    Formal,
    Type(..)
  ) where

data Type   = CBV | CBN | Lazy
type Formal = (String, Type)

data Program = Fun String [(String,Type)] Expr
             | Seq [Program]

data Expr =
    Call String [Expr]
  | EVar String
  | EInt Integer
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EMod Expr Expr
  | Eif Expr Expr Expr

