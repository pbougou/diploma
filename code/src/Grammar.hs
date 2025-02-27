module Grammar (
    Expr (..),
    Program(..),
    FDef(..),
    Formal,
    Type(..),
    EvalOrder(..),
    Scopes(..),
    Pattern(..),
    ArithmOp(..),
    UnaryArithm(..),
    CaseID, VN, FN, CN, CPos, Tag, Scrutinee, Branch, Actual
  ) where
import Data.List(map, elemIndex, lookup, foldr, unzip)
import qualified Data.List as L
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Text.Read

type CN = String -- constructor name
type FN = String -- function name
type VN = String -- variable name

data Type = TInt | TCons Type deriving Eq
data EvalOrder = CBV | CBN | Lazy
    deriving Eq
type Formal = (VN, (EvalOrder, Type)) -- formal variable with strictness annotation
type Funbody = Expr
data FDef = Fun { function :: FN, formals :: [Formal], body :: Funbody }
type Program = [FDef]

--  case e of { (patt1 -> expr1); (patt2 -> expr2) }
type CaseID = Integer
type CPos = Int

type Tag = String
type Scrutinee = Expr
type Branch = (Pattern, Expr)
data Pattern = CPat { tag :: CN, vars :: [VN] }
             | IPat { pattVal :: Integer }
    deriving Eq
type Actual = Expr
data ArithmOp = EAdd | ESub | EMul | EDiv | EMod
    deriving Eq
data UnaryArithm = EUnMinus | EUnPlus
    deriving Eq
data Expr =
    TailCall FN [Expr]    -- used for tco
  | Call FN [Expr] 
  | EVar VN
  | EInt Integer
  | ConstrF Tag [Expr]
  | Nil                   -- Nil constructor
  | CaseF CaseID Scrutinee [Branch]
  | CProj CaseID CPos     -- bound variables from case
  | UnaryOp UnaryArithm Expr
  | BinaryOp ArithmOp Expr Expr
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
    showsPrec p (UnaryOp unArithm x) = 
        case unArithm of
            EUnMinus -> (" - " ++) . showsPrec 1 x
            EUnPlus -> (" + " ++) . showsPrec 1 x
    showsPrec p (BinaryOp binArithm x y) = 
        case binArithm of
            EAdd -> showParen (p > 0) $
                        showsPrec 2 x . (" + " ++) . showsPrec 2 y
            ESub -> showParen (p > 0) $
                        showsPrec 2 x . (" - " ++) . showsPrec 2 y
            EMul -> showsPrec 1 x . (" * " ++) . showsPrec 1 y 
            EDiv -> showsPrec 1 x . (" / " ++) . showsPrec 1 y
            EMod -> showsPrec 1 x . (" % " ++) . showsPrec 1 y
    showsPrec p Nil = (" Nil " ++)
    showsPrec p (ConstrF tag exps) = 
        showParen (p > 0) $
            (tag ++) . (" " ++) . showList exps
    showsPrec p (CaseF caseID e lines) =
        showParen (p > 0) $
            ("case <" ++) . (show caseID  ++) . ("> " ++) . shows e . (" of " ++) . 
            ("{ " ++) . showLines lines . (" }" ++)
            where   showLines [] = ("" ++)
                    showLines [line] =  shows (fst line) .  
                                        showsPrec 1 (snd line)
                    showLines (line : lines) =  shows (fst line) . 
                                                shows (snd line) . 
                                                ("; " ++) . 
                                                showLines lines
    showsPrec p (CProj cid cpos) = showParen True (("CProj " ++ show cid ++ " " ++ show cpos) ++)

instance Show FDef where
    showList [] = ("" ++)
    showList [l] = shows l
    showList (l : ls) = shows l .(";\n" ++) .showList ls
    showsPrec p (Fun x l e) = 
        -- Print function signature.
        ("fun " ++) . (x ++) . (" :: " ++) . 
        showList sign . ("\n" ++) .
        -- Print function.
        ("fun " ++) . (x ++) . (" " ++) . (l' ++) .
        ("=\n    " ++) . shows e
        where 
            (l', sign) = showFormals l
            showFormals =
                L.foldr (\(fn, (ord, tp)) (y, tpAcc) -> 
                    let ch = case ord of { CBV  -> "!"; CBN  -> "#"; Lazy -> "" }
                    in  (ch ++ fn ++ " " ++ y, tp : tpAcc)) ("", [])

instance Show EvalOrder where
    showsPrec p eo = 
        case eo of   
            CBV  -> ("! " ++)
            CBN  -> ("# " ++)
            Lazy -> ("" ++)

instance Show Type where
    showList [] = shows " "
    showList [t] = shows t
    showList (t : ts) = shows t . (" -> " ++) . showList ts

    showsPrec p t = 
        case t of
            TInt -> ("int" ++)
            TCons t' -> ("list " ++) . showsPrec p t' 

instance Show Pattern where 
    showsPrec p patt = 
        case patt of
            IPat val -> (show val ++)
            CPat cn vars -> 
                (case cn of
                    "Nil" -> ("Nil" ++)
                    "Cons" -> (cn ++) 
                            . (" " ++) 
                            . (show vars ++)) . (" -> " ++)