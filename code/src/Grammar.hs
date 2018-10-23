module Grammar (
    Expr (..),
    Program(..),
    FDef(..),
    Formal,
    Type(..),
    Scopes(..),
    CaseID, VN, FN, CN, CPos, Tag, Scrutinee
  ) where

import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Text.Read

type CN = String -- constructor name
type FN = String -- function name
type VN = String -- variable name

data Type = CBV | CBN | Lazy
    deriving Eq
type Formal = (VN, Type) -- variable with strictness annotation
data FDef = Fun FN [Formal] Expr
type Program = [FDef]

--  case e of (patt -> expr)+
type CaseID = Integer
type CPos = Int

type Tag = String
type Scrutinee = Expr
data Expr =
    TailCall FN [Expr]    -- used for tco
  | Call FN [Expr] 
  | EVar VN
  | EInt Integer
  | ConstrF Tag [Expr]
  | CaseF CaseID Scrutinee [(Expr, Expr)]
  | CProj CaseID CPos     -- bound variables from case
  | EUnPlus Expr
  | EUnMinus Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EMod Expr Expr
  | Eif Expr Expr Expr
    deriving Eq

-- Symbol table with name scopes
type Scopes = Map FN [(CaseID, [(VN, Expr)])]


instance Show Expr where
    showList [] = ("" ++)
    showList list@(l : ls) = case list of   [_] -> showsPrec 1 l
                                            _   -> showsPrec 1 l . (" " ++) . showList ls
    showsPrec p (Call x l) =
        showParen (p > 0) $
            ("call " ++) . (x ++) . (" " ++) . showsPrec 1 l
    showsPrec p (TailCall x l) =
        ("TC-call " ++) . (x ++) . (" " ++) . shows l
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
    showsPrec p (ConstrF tag exps) = 
        let ending = if length exps < 2 then " Nil" else ""
        in  showParen (p > 0) $
                case exps of { [] -> (tag ++); _ -> (tag ++) . (" " ++) . showList exps . (ending ++)}
    showsPrec p (CaseF caseID e lines) =
        showParen (p > 0) $
            ("case <" ++) . (show caseID  ++) . ("> " ++) . shows e . (" of " ++) . ("{ " ++) . showLines lines . (" }" ++)
            where   showLines [] = ("" ++)
                    showLines [line] =  shows (fst line) . 
                                        (" -> " ++) . 
                                        showsPrec 1 (snd line)
                    showLines (line : lines) =  shows (fst line) . 
                                                (" -> " ++) . 
                                                shows (snd line) . 
                                                ("; " ++) . 
                                                showLines lines
    showsPrec p (CProj cid cpos) = showParen True (("CProj " ++ show cid ++ " " ++ show cpos) ++)

instance Show FDef where
    showList [] = ("" ++)
    showList [l] = shows l
    showList (l : ls) = shows l .(";\n" ++) .showList ls
    showsPrec p (Fun x l e) = ("fun " ++) . (x ++) . (" " ++) . (l' ++) .
                                ("=\n    " ++) . shows e
        where 
            l' = showFormals l
            showFormals =
                L.foldr (\x y -> 
                    let ch = case snd x of { CBV  -> "!"; CBN  -> "#"; Lazy -> "" }
                    in  ch ++ fst x ++ " " ++ y) ""

instance Show Type where
    showsPrec p t = case t of   
                        CBV  -> ("! " ++)
                        CBN  -> ("# " ++)
                        Lazy -> ("" ++)
