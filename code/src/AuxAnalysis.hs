module AuxAnalysis (
    actualsFS,
    searchFS,
    isCBV,
    isVar,
    areVars
) where
import Grammar 
import Data.Maybe
import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L
import Data.Map.Strict
import qualified Data.Map.Strict as Map

-- TailCall FN [Expr]    -- used for tco
-- | Call FN [Expr] 
-- | EVar VN
-- | EInt Integer
-- | ConstrF Tag [Expr]
-- | Nil
-- | CaseF CaseID Scrutinee [Branch]
-- | CProj CaseID CPos     -- bound variables from case
-- | UnaryOp UnaryArithm Expr
-- | BinaryOp ArithmOp Expr Expr
-- | Eif Expr Expr Expr

-- Check if every actual is not dependented
--  Calls searchFS for every formal of caller function
actualsFS :: [Formal]
            -> [Expr]
            -> Bool
actualsFS fsCaller = L.foldr (\e acc -> searchFS fsCaller e && acc) True

-- Assuming: 
--  1. renaming in variables bound by case patterns 
--  2. case expression not in function' s actuals or constructors expressions
searchFS :: [Formal]    -- caller's formals
            -> Expr     -- callee' s actual processed
            -> Bool     -- True if it is not dependent by caller's formals
searchFS fsCaller actual = 
    case actual of
        EVar v ->   
            case L.lookup v fsCaller of
                Nothing -> error ("searchFS: Variable must be in formals, var = " ++ show v)
                Just _  -> False
        EInt n -> True
        Nil    -> True
        UnaryOp unaryArithm e -> searchFS fsCaller e
        BinaryOp binAr e1 e2 ->
            searchFS fsCaller e1 && 
            searchFS fsCaller e2 
        Eif c e1 e2  -> searchFS fsCaller e1 && 
                        searchFS fsCaller c && 
                        searchFS fsCaller e2
        Call _ acts  ->
            L.foldr (\a b -> searchFS fsCaller a && b) True acts
        ConstrF tag exprs -> 
            L.foldr (\a b -> searchFS fsCaller a && b) True exprs
        CaseF _ scr brs -> 
            L.foldr ((\a b -> searchFS fsCaller a && b) . snd) True brs
        CProj {} -> False
        TailCall _ _ -> error "Tail Call: This should be unreached"

isCBV :: Expr 
        -> [Expr] 
        -> [Formal] 
        -> Bool
isCBV e es fs =
    let Just ix = elemIndex e es
        (_, (tp, _)) = fs !! ix
    in case tp of
        CBV -> True
        _   -> False

-- Check if all actuals all variables or numbers
areVars :: [Expr] -> Bool
areVars = L.foldr (\a b -> isVar a && b) True

-- Check if an actual parameter is a value or a variable
isVar :: Expr -- actuals
        -> Bool -- True if is value or variable
isVar e = 
    case e of 
        EVar _ -> True
        EInt _ -> True
        Nil    -> True
        CProj{}-> True 
        _      -> False
