module Grammar(
    Expr (..),
    Program(..),
    FDef(..),
    Formal,
    Type(..)
  ) where

data Type   = CBV | CBN | Lazy
type Formal = (String, Type)

type Program = [FDef]
data FDef    = Fun String [(String,Type)] Expr

data Expr =
    TailCall String [Expr]    -- used for tco
  | Call String [Expr]
  | EVar String
  | EInt Integer
  | EUnPlus Expr
  | EUnMinus Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EMod Expr Expr
  | Eif Expr Expr Expr
    deriving Eq
