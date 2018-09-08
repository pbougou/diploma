{-
--  annotate: Add annotation to AST nodes 
--  for tail call positions
--}
module TailCalls (
    AP(..),        -- annotated program 
    A(..),         -- annotated expression 
    annotateP, 
    spotTCs
) where

import Grammar
import PPrint

import StateInterpreter(replaceNth, functionMap)

import Data.Maybe(fromJust)

import Data.List(map, elemIndex, lookup)
import qualified Data.List as L

import Data.Map.Strict
import qualified Data.Map.Strict as Map

-- Intraprocedural analysis:
--      Spot tail calls locally in function' s body
spotTCs :: Program -> Program
spotTCs = 
    let 
        annotateL :: FDef -> FDef
        annotateL (Fun funName formals body) = Fun funName formals (annotateE True body) 

        annotateE :: Bool -> Expr -> Expr
        annotateE b expr =
            case expr of
                EVar v         -> EVar v
                EInt n         -> EInt n
                EUnPlus e      -> EUnPlus (annotateE False e)
                EUnMinus e     -> EUnMinus (annotateE False e)
                EAdd e1 e2     -> EAdd (annotateE False e1) (annotateE False e2)
                ESub e1 e2     -> ESub (annotateE False e1) (annotateE False e2)
                EMul e1 e2     -> EMul (annotateE False e1) (annotateE False e2)
                EDiv e1 e2     -> EDiv (annotateE False e1) (annotateE False e2)
                EMod e1 e2     -> EMod (annotateE False e1) (annotateE False e2)
                Eif c e1 e2    -> Eif (annotateE False c) (annotateE b e1) (annotateE b e2)
                Call n actuals -> if b then TailCall n actuals else Call n actuals
                TailCall _ _   -> error "Tail Call: This should be unreached"

    in  L.map annotateL


-- Global analysis
data AP = AFun String [(String,Type)] A
    deriving Show 

data A =
    ACall AP [A]          -- function call
  | TCCall AP [A]         -- tail call to function
  | TRCCall String [Expr] -- tail recursive call?
  | AVar String
  | AInt Integer
  | AUnPlus A
  | AUnMinus A
  | AAdd A A
  | ASub A A
  | AMul A A
  | ADiv A A
  | AMod A A
  | AIf A A A
    deriving Show


--      AST transformation: annotate program whether 
--      an expression is in tail position or not
annotateP :: Program -> AP
annotateP p = 
    let funsMap = functionMap p Map.empty
        visited = L.map (\s -> (s, False)) $ Map.keys funsMap

        -- annotate expr
        annotate :: [(String, Bool)] -> Bool -> Expr -> A
        annotate vis b expr = 
            case expr of 
                EVar v         -> AVar v
                EInt n         -> AInt n
                Call n actuals -> 
                    let Just (formals, e) = Map.lookup n funsMap
                    in  if b then if fromJust $ L.lookup n vis then TRCCall n actuals 
                                  else let ix = fromJust $ elemIndex (n, False) vis
                                           vis' = replaceNth ix (n, True) vis
                                       in  TCCall (annotateF (Fun n formals e) vis') (L.map (annotate vis' False) actuals)
                        else ACall (annotateF (Fun n formals e) vis) (L.map (annotate vis False) actuals)
                EUnPlus e      -> AUnPlus (annotate vis b e)
                EUnMinus e     -> AUnMinus (annotate vis b e)
                EAdd e1 e2     -> AAdd (annotate vis b e1) (annotate vis b e2)
                ESub e1 e2     -> ASub (annotate vis b e1) (annotate vis b e2)
                EMul e1 e2     -> AMul (annotate vis b e1) (annotate vis b e2)
                EDiv e1 e2     -> ADiv (annotate vis b e1) (annotate vis b e2)
                EMod e1 e2     -> AMod (annotate vis b e1) (annotate vis b e2)
                Eif c e1 e2    -> AIf (annotate vis False c) (annotate vis b e1) (annotate vis b e2) 

        annotateF :: FDef -> [(String, Bool)] -> AP
        annotateF (Fun n formals body) vis = AFun n formals (annotate vis True body)

    in  case Map.lookup "main" funsMap of
            Just(formals, expr) -> AFun "main" formals (annotate visited False expr)
            Nothing             -> error "main not found"




