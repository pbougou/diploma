module Grammar(
    Expr (..),
    Program(..),
    FDef(..),
    Formal,
    Type(..),
    StackFrame(..),
    StackFrameArg(..),
    FunctionsMap(..),
    CallStack(..),
    List(..)
  ) where

import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Text.Read

data Type   = CBV | CBN | Lazy
    deriving Eq
type Formal = (String, Type)

type Program = [FDef]
data FDef    = Fun String [(String,Type)] Expr

data List a = Cons a (List a)
            | Nil
    deriving Show

data Expr =
    TailCall String [Expr]    -- used for tco
  | Call String [Expr]
  | EVar String
  | EInt Integer
  | List 
  | EUnPlus Expr
  | EUnMinus Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EMod Expr Expr
  | Eif Expr Expr Expr
    deriving Eq

type StackFrame = (String, [StackFrameArg])
data StackFrameArg = StrictArg { val :: Integer }
                   | ByNameArg { expr :: Expr }
                   | LazyArg   { expr :: Expr, isEvaluated :: Bool, cachedVal :: Maybe Integer }
    deriving Show
    
type FunctionsMap = Map.Map String ([Formal], Expr)
type CallStack = [StackFrame]

instance Show Expr where
  showList [] = ("" ++)
  showList (l : ls) = showsPrec 1 l . (" " ++) . showList ls
  showsPrec p (Call x l) =
      ("call " ++) . (x ++) . (" " ++) .showsPrec 0 l

  showsPrec p (TailCall x l) =
      ("TC-call " ++) . (x ++) . (" " ++) .showsPrec 0 l
  showsPrec p (Eif x y z) =
    showParen (p > 0) $
        ("if " ++) . showsPrec 0 x .
        (" then " ++) . showsPrec 0 y .
        (" else " ++) . showsPrec 0 z
  showsPrec p (EVar x)   = (x ++)
  showsPrec p (EInt x)   = (show x ++)
  showsPrec p (EUnMinus x) = (" - " ++) . showsPrec 1 x
  showsPrec p (EUnPlus x) = (" + " ++) . showsPrec 1 x
  showsPrec p (EAdd x y) =
      showParen (p > 0) $
          showsPrec 2 x . (" + " ++) . showsPrec 2 y
  showsPrec p (ESub x y) =
      showParen (p > 0) $
          showsPrec 2 x . (" - " ++) . showsPrec 2 y
  showsPrec p (EMul x y) = showsPrec 1 x . (" * " ++) . showsPrec 1 y
  showsPrec p (EDiv x y) = showsPrec 1 x . (" / " ++) . showsPrec 1 y
  showsPrec p (EMod x y) = showsPrec 1 x . (" % " ++) . showsPrec 1 y

instance Show FDef where
  showList [] = ("" ++)
  showList (l : ls) = showsPrec 0 l .(";\n" ++) .showList ls
  showsPrec p (Fun x l e) = ("fun " ++) . (x ++) . (" " ++) . (l' ++) .
                            ("=\n    " ++) . showsPrec 0 e
     where l' = showFormals l
           showFormals =
             L.foldr (\x y -> let ch = case snd x of { CBV  -> "!"; CBN  -> "#"; Lazy -> "" }
             in ch ++ fst x ++ " " ++ y) ""

instance Show Type where
  showsPrec p t = case t of CBV  -> ("! " ++)
                            CBN  -> ("# " ++)
                            Lazy -> ("" ++)
