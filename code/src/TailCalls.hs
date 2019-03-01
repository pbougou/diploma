---------------------------------------------------------------------
--  Analysis to spot tail call positions
--      New AST node: TailCall
-- TailCall does not allocate new frame. Instead, it uses the current
--      frame. 
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Local (intraprocedural) analysis(spotTCs):
--  Spot tail calls locally in function' s body
--  This analysis reveals tc-positions with *good* potential.
--  *****************What happens with sharing??*********************
--------------------------------------------------------------------- 
--      Rules about tail calls in language with CBV, CBN, Lazy:
--          1. actuals are expressions not dependent by the formals
--          2. actuals are variables
--              a. same evaluation order
--              b. different evaluation order
--          3. actuals are expressions dependent by the formals
--             in cbv position(cbn, lazy are forbidden)
---------------------------------------------------------------------
module TailCalls (
    spotTCs
) where
import Grammar
import AuxAnalysis
import StateInterpreter(replaceNth, functionMap)
import Data.Maybe(fromJust, fromMaybe)
import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Debug.Trace(trace)

-- Returns:
--  1. Tail-call annotated program
--  2. tc-candidate functions
-- A tail-call candidate is a function that contains a tail-call in its body
spotTCs :: Program -> (Program, [FN])
spotTCs fdefs = 
    let funsMap = functionMap fdefs Map.empty 

        annotateP :: [FDef] -> [FDef] -> [FN] -> ([FDef], [FN])
        annotateP [] fdefAcc tcFnAcc = 
            let cutEmpStrs [] = []
                cutEmpStrs (h : t) = 
                    if h == "" then cutEmpStrs t else h : cutEmpStrs t 
                tcFnAcc' = cutEmpStrs tcFnAcc 
            in  (fdefAcc, tcFnAcc')
        annotateP (fdef : fdefs) fdefAcc tcFnAcc = 
            let (fdef', maybeFN) = annotateL fdef
                fn = fromMaybe "" maybeFN
            in  annotateP fdefs (fdef' : fdefAcc) (fn : tcFnAcc)

        annotateL :: FDef -> (FDef, Maybe FN)
        annotateL (Fun funName formals body) = (Fun funName formals annotatedBody, if state then Just funName else Nothing)
            where
                (annotatedBody, state) = runState (annotateE True body) False
 
                annotateE :: Bool -> Expr -> State Bool Expr
                annotateE _ (EVar v) = return (EVar v)
                annotateE _ (EInt n) = return (EInt n)
                annotateE _ (UnaryOp ua e) = do
                    st <- get
                    let (e', st') = runState (annotateE False e) st
                    put (st' || st)
                    return (UnaryOp ua e')
                annotateE _ (BinaryOp ba e1 e2) = do
                    stb <- get
                    let (e1', st') = runState (annotateE False e1) stb
                        (e2', st'') = runState (annotateE False e2) st'
                    put (stb || st' || st'')
                    return (BinaryOp ba e1' e2')
                annotateE b (Eif c e1 e2) = do
                    st <- get
                    let (e1', st') = runState (annotateE False e1) st
                        (e2', st'') = runState (annotateE False e2) st'
                    put (st || st' || st'')
                    return (Eif c e1' e2')
                annotateE True (Call n actuals) = 
                    let Just(fsCallee, _, _) = Map.lookup n funsMap

                        b'  = areVars actuals -- if true all actuals are variables
                            
                        b'' = L.foldr (\e acc -> 
                                        let isDependent = searchFS formals e
                                            isV = isVar e 
                                            cbv = isCBV e actuals fsCallee
                                        in  (isV || cbv || isDependent) && acc) True actuals

                    in  if b' || b'' 
                            then do put True
                                    return (TailCall n actuals)
                            else return (Call n actuals)
                annotateE b (CaseF cid scrutinee branches) = do
                    state <- get
                    let (branches', state') = annotateBs b state branches []
                    put (state || state')
                    return (CaseF cid scrutinee branches')
                annotateE b (ConstrF tag (h : t)) = 
                    return (ConstrF tag (h : t))
                annotateE _ funcall@Call {} = return funcall
                annotateE _ Nil = return Nil
                annotateE _ cproj@CProj {} = return cproj
                annotateE _ (TailCall _ _) = error "Tail Call: This should be unreached" 
                annotateE _ exprs@_ = error ("Unhandled expressions = " ++ show exprs)

                annotateB :: Bool -> Bool -> Branch -> (Branch, Bool) 
                annotateB b st (patt, expr) = ((patt, expr'), st || st')
                    where (expr', st') = runState (annotateE b expr) st
                
                annotateBs :: Bool -> Bool -> [Branch] -> [Branch] -> ([Branch], Bool)
                annotateBs _ st [] brsAcc = (brsAcc, st)
                annotateBs b st (br : brs) brsAcc = 
                    let (br', st') = annotateB b st br
                    in  annotateBs b (st || st') brs (br' : brsAcc) 

    in  annotateP fdefs [] [] -- L.map annotateL fdefs

--------------------------------------------------------------------------------------------------
--2.  Function dependentCandidate: Finds which a candidate TC function 
--------------------------------------------------------------------------------------------------


--------------------------------------------------------------------------------------------------    
-- Global analysis
--------------------------------------------------------------------------------------------------
-- data AP = AFun String [(String,Type)] A
--     deriving Show 

-- data A =
--     ACall AP [A]          -- function call
--   | TCCall AP [A]         -- tail call to function
--   | TRCCall String [Expr] -- tail recursive call?
--   | AVar String
--   | AInt Integer
--   | AUnPlus A
--   | AUnMinus A
--   | AAdd A A
--   | ASub A A
--   | AMul A A
--   | ADiv A A
--   | AMod A A
--   | AIf A A A
--     deriving Show


-- --      AST transformation: annotate program whether 
-- --      an expression is in tail position or not
-- annotateP :: Program -> AP
-- annotateP p = 
--     let funsMap = functionMap p Map.empty
--         visited = L.map (\s -> (s, False)) $ Map.keys funsMap

--         -- annotate expr
--         annotate :: [(String, Bool)] -> Bool -> Expr -> A
--         annotate vis b expr = 
--             case expr of 
--                 EVar v         -> AVar v
--                 EInt n         -> AInt n
--                 Call n actuals -> 
--                     let Just (formals, e) = Map.lookup n funsMap
--                     in  if b then if fromJust $ L.lookup n vis then TRCCall n actuals 
--                                   else let ix = fromJust $ elemIndex (n, False) vis
--                                            vis' = replaceNth ix (n, True) vis
--                                        in  TCCall (annotateF (Fun n formals e) vis') (L.map (annotate vis' False) actuals)
--                         else ACall (annotateF (Fun n formals e) vis) (L.map (annotate vis False) actuals)
--                 EUnPlus e      -> AUnPlus (annotate vis b e)
--                 EUnMinus e     -> AUnMinus (annotate vis b e)
--                 EAdd e1 e2     -> AAdd (annotate vis b e1) (annotate vis b e2)
--                 ESub e1 e2     -> ASub (annotate vis b e1) (annotate vis b e2)
--                 EMul e1 e2     -> AMul (annotate vis b e1) (annotate vis b e2)
--                 EDiv e1 e2     -> ADiv (annotate vis b e1) (annotate vis b e2)
--                 EMod e1 e2     -> AMod (annotate vis b e1) (annotate vis b e2)
--                 Eif c e1 e2    -> AIf (annotate vis False c) (annotate vis b e1) (annotate vis b e2) 

--         annotateF :: FDef -> [(String, Bool)] -> AP
--         annotateF (Fun n formals body) vis = AFun n formals (annotate vis True body)

--     in  case Map.lookup "main" funsMap of
--             Just(formals, expr) -> AFun "main" formals (annotate visited False expr)
--             Nothing             -> error "main not found"




