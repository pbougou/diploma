module Grammar (
    Expr (..),
    Program(..),
    FDef(..),
    Formal,
    Type(..),
    Scopes(..),
    Pattern(..),
    ArithmOp(..),
    UnaryArithm(..),
    CaseID, VN, FN, CN, CPos, Tag, Scrutinee, Branch
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
type Formal = (VN, Type) -- formal variable with strictness annotation
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

data ArithmOp = EAdd | ESub | EMul | EDiv | EMod
    deriving Eq
data UnaryArithm = EUnMinus | EUnPlus
    deriving Eq
type Actual = Expr
-- Assumming local variables have different names and different from the formal parameter of the function
-- Binds a local variable(key: unique variable name) with a FΟ λambda (value: lambda)
--      * All let expressions contain bindings
--      * Bindings: 
--          1. LHS: variable name, unique names in a program
--          2. RHS: expressions
--      * The use of LETs: {
--          A. For two cases of expressions:
--              1. function applications
--              2. constructor applications
--          B. Explanation: 
--              * The two expressions above should contain only variable names.
--              * Their actual expression are RHS of let bindings.
--          C. LET is a builder of heap objects
--          D. Local variables: ρ(k: name -> v: Heap address), ρ: local environment
--        }
type Bindings = Map.Map VN Expr
data Expr =
    TailCall FN [Expr]    -- used for tco
  | Call FN [Actual] 
  | EVar VN
  | EInt Integer
  | ConstrF Tag [Expr]
  | Let Bindings Expr     -- local definitions
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
    showsPrec p (ConstrF tag exps) = 
        let ending = if length exps < 2 then " Nil" else ""
        in  showParen (p > 0) $
                case exps of { [] -> (tag ++); _ -> (tag ++) . (" " ++) . showList exps . (ending ++)}
    showsPrec p (CaseF caseID e lines) =
        showParen (p > 0) $
            ("case <" ++) . (show caseID  ++) . ("> " ++) . shows e . (" of " ++) . ("{ " ++) . showLines lines . (" }" ++)
            where   showLines [] = ("" ++)
                    showLines [line] =  shows (fst line) .  
                                        showsPrec 1 (snd line)
                    showLines (line : lines) =  shows (fst line) . 
                                                -- (" -> " ++) . 
                                                shows (snd line) . 
                                                ("; " ++) . 
                                                showLines lines
    showsPrec p (CProj cid cpos) = showParen True (("CProj " ++ show cid ++ " " ++ show cpos) ++)
    showsPrec p (Let bindings e) =
        ("let { " ++ ) . (showBindings (assocs bindings) ++) . ("in " ++) . (show e ++)

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

instance Show Pattern where 
    showsPrec p patt = 
        case patt of
            IPat val -> (show val ++)
            CPat cn vars ->
                case cn of 
                    "Nil" -> ("Nil -> " ++)
                    _ ->
                        (cn ++) 
                        . (" " ++) 
                        . (show vars ++) 
                        . (" -> " ++)

showBindings :: [(VN, Expr)] -> String
showBindings [] = "} "
showBindings (bind : binds) =
    let (vn, expr) = bind
    in  vn ++ " = " ++ show expr ++ "; " ++ showBindings binds

showVars :: [VN] -> String
showVars [var] = var
showVars (var : vars) = var ++ " " ++ showVars vars
