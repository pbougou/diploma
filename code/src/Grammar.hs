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
    Value(..),
    Context(..),
    Susp(..)
  ) where

import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Text.Read

data Value =  VI Integer 
            | VC Susp
    deriving Show

data Type   = CBV | CBN | Lazy
    deriving Eq
type Formal = (String, Type)

type Program = [FDef]
data FDef    = Fun String [(String,Type)] Expr

type CaseID = Integer
data Expr =
    TailCall String [Expr]    -- used for tco
  | Call String [Expr] 
  | EVar String
  | EInt Integer
  | ConstrF [Expr]
  | CaseF CaseID Expr [(Expr, Expr)]
  | EUnPlus Expr
  | EUnMinus Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EMod Expr Expr
  | Eif Expr Expr Expr
    deriving Eq

-- Runtime data structures
type FN = String -- function name
type StackFrame = (FN, [StackFrameArg])
data StackFrameArg = StrictArg { val :: Value }
                   | ByNameArg { expr :: Expr }
                   | LazyArg   { expr :: Expr, isEvaluated :: Bool, cachedVal :: Maybe Value }
    deriving Show

type FunctionsMap = Map.Map String ([Formal], Expr)
type Context = (StackFrame, [(CaseID, Susp)])
type CallStack = [Context]

type CN = String
data Susp = Susp (CN, [Expr]) CallStack  -- Constructor carry the environment so far
    deriving Show

instance Show Expr where
  showList [] = ("" ++)
  showList list@(l : ls) = case list of [_] -> shows l
                                        _   -> showsPrec 1 l . (" " ++) . showList ls
  showsPrec p (Call x l) =
      ("call " ++) . (x ++) . (" " ++) .shows l

  showsPrec p (TailCall x l) =
      ("TC-call " ++) . (x ++) . (" " ++) .shows l
  showsPrec p (Eif x y z) =
    showParen (p > 0) $
        ("if " ++) . shows x .
        (" then " ++) . shows y .
        (" else " ++) . shows z
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
  showsPrec p (ConstrF exps) = ("Cons " ++) . (case exps of { [] -> ("Nil" ++); _ -> showList exps })
  showsPrec p (CaseF caseID e lines) =
    ("case <" ++) . (show caseID  ++) . ("> " ++) . shows e . (" of " ++) . ("\n\t" ++) . showLines lines
        where 
            showLines [] = ("" ++)
            showLines (line : lines) = shows (fst line) . 
                                         (" -> " ++) . 
                                         shows (snd line) . 
                                         ("\n\t" ++) . 
                                         showLines lines

instance Show FDef where
  showList [] = ("" ++)
  showList (l : ls) = shows l .(";\n" ++) .showList ls
  showsPrec p (Fun x l e) = ("fun " ++) . (x ++) . (" " ++) . (l' ++) .
                            ("=\n    " ++) . shows e
     where l' = showFormals l
           showFormals =
             L.foldr (\x y -> let ch = case snd x of { CBV  -> "!"; CBN  -> "#"; Lazy -> "" }
             in ch ++ fst x ++ " " ++ y) ""

instance Show Type where
  showsPrec p t = case t of CBV  -> ("! " ++)
                            CBN  -> ("# " ++)
                            Lazy -> ("" ++)
