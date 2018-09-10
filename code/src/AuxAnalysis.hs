module AuxAnalysis (
    searchFS,
    isCBV,
    isVar
) where

import Grammar 

import Data.Maybe

import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L

import Data.Map.Strict
import qualified Data.Map.Strict as Map



searchFS :: [Formal]    -- caller's formals
            -> Expr     -- actual processed
            -> Bool     -- True if it is not dependent by caller's formals
searchFS fsCaller actual = 
    case actual of
        EVar v       -> case L.lookup v fsCaller of
                            Nothing -> error "Variable must be in formals"
                            Just _  -> False
        EInt n       -> True
        EUnPlus e    -> searchFS fsCaller e
        EUnMinus e   -> searchFS fsCaller e
        EAdd e1 e2   -> searchFS fsCaller e1 && 
                        searchFS fsCaller e2
        ESub e1 e2   -> searchFS fsCaller e1 && 
                        searchFS fsCaller e2
        EMul e1 e2   -> searchFS fsCaller e1 && 
                        searchFS fsCaller e2
        EDiv e1 e2   -> searchFS fsCaller e1 && 
                        searchFS fsCaller e2
        EMod e1 e2   -> searchFS fsCaller e1 && 
                        searchFS fsCaller e2
        Eif c e1 e2  -> searchFS fsCaller e1 && 
                        searchFS fsCaller c && 
                        searchFS fsCaller e2
        Call _ acts  ->
            L.foldr (\a b -> searchFS fsCaller a && b) True acts
        TailCall _ _ -> error "Tail Call: This should be unreached"

isCBV :: Expr 
        -> [Expr] 
        -> [Formal] 
        -> Bool
isCBV e es fs =
    let Just ix = elemIndex e es
        (_, tp) = fs !! ix
    in case tp of
        CBV -> True
        _   -> False

isVar :: [Expr] 
        -> Bool
isVar []       = True 
isVar (e : es) = 
    case e of
        EVar _ -> isVar es
        _      -> False
