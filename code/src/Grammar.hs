module Grammar(
    Expr (..),
    Program(..),
    Formal(..),
    Type(..)
  ) where

data Type   = CBV | CBN | Lazy deriving Show
type Formal = (String, Type)

data Program = Fun String [Formal] Expr
             | Seq [Program]
--    deriving Show

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
--    deriving Num

