---------------------------------------------------------------------
-- An analysis algorithm to spot tail call positions.               |
--  New annotation: **TailCall**. When executed,                    |
--  TailCall doesn't allocate new frame. It uses the current frame. |
---------------------------------------------------------------------
---------------------------------------------------------------------
-- 1. Local (intraprocedural) analysis(spotTCs):                    |
--  Spot tail calls locally in function' s body                     |
--  This analysis reveals tc-positions with *good* potential.       |
-- ******************What happens with sharing??*********************
---------------------------------------------------------------------
--      Rules about tail calls in language with CBV, CBN, Lazy:     |
--          1. actuals are expressions not dependent by the formals |
--          2. actuals are variables                                |
--              a. same evaluation order                            |
--              b. different evaluation order                       |
--          3. actuals are expressions dependent by the formals     |
--             in cbv position(cbn, lazy are forbidden)             |
---------------------------------------------------------------------
module TailCalls (
    spotTCs,
    callInFun,
    callInProgram,
    eliminateTCs,
    addNonTCcandi,
    mapM__,
    dataAnalysisP,
    analysis
) where
import Grammar as G
import AuxAnalysis
import StateInterpreter(replaceNth, functionMap)
import Data.Maybe(fromJust, fromMaybe)
import Data.List(map, elemIndex, lookup, foldr, nub, union, intersect)
import qualified Data.List as L
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Debug.Trace(trace)
import Control.Arrow
import RuntimeStructs(FunctionsMap)

---------------------------------------------------------------------
-- Returns:                                                         |
--  1. Tail-call annotated program                                  |
--  2. tc-candidate functions                                       |
-- spotTCs: Locally analyzes lambda's body:                         |
--  1. Annotates function calls with tail-calls                     |
--  2. Returns a list **tail-call candidates**                      |
--     That is: A function that contains a TailCall in its body     |
---------------------------------------------------------------------
spotTCs :: Program -> (Program, [FN])
spotTCs fdefs = 
    let funsMap = functionMap fdefs Map.empty 

        annotateP :: [FDef] -> [FDef] -> [FN] -> ([FDef], [FN])
        annotateP [] fdefAcc tcFnAcc = 
            let 
                cutEmpStrs :: [FN] -> [FN]
                cutEmpStrs [] = []
                cutEmpStrs (h : t) = 
                    if h == "" then cutEmpStrs t else h : cutEmpStrs t 
                tcFnAcc' = cutEmpStrs tcFnAcc 
            in  (fdefAcc, tcFnAcc')
        annotateP (fdef : fdefs) fdefAcc tcFnAcc = 
            let (fdef', maybeFN) = annotateL fdef
                fn = fromMaybe "" maybeFN
            in  annotateP fdefs (fdef' : fdefAcc) (fn : tcFnAcc)

        annotateL :: FDef -> (FDef, Maybe FN)
        annotateL (Fun funName formals body) = (fdef', maybeFN)
            where
                fdef' = Fun funName formals annotatedBody
                maybeFN = if state then Just funName else Nothing
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
                    let (e1', st') = runState (annotateE b e1) st
                        (e2', st'') = runState (annotateE b e2) st'
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

    in  annotateP fdefs [] []

---------------------------------------------------------------------
-- **REVIVING PLAN**:                                               |
--  For some function calls, but not all in general case,           |
--  a tc-candidate can be tc-optimized.                             |
--  Thus, we need a plan B, after the annotation that spotTCs does. |
--  We have two auxiliary functions for this.                       |
--      Aux. function no1. addNonTCcandi                            |
--      Aux. function no2. eliminateTCs                             |
---------------------------------------------------------------------

---------------------------------------------------------------
-- Auxiliary function no.1:                                   |
--  Add the non-tail recursive version of a lambda to program |  
---------------------------------------------------------------
addNonTCcandi :: Program -> FN -> Program
addNonTCcandi p fn = 
    let funsMap = functionMap p Map.empty
        (formals, body, depth) = fromMaybe (error ("Not found fun: " ++ fn)) (Map.lookup fn funsMap)
        nonTCprefix = "nonTC-"
        fdef' = Fun (nonTCprefix ++ fn) formals (eliminateTCs body)
    in  p ++ [fdef']

----------------------------------------------
-- Auxiliary function no.2                   |
--  Eliminate TailCall from a lambda' s body |
----------------------------------------------
eliminateTCs :: Expr -> Expr
eliminateTCs expr = 
    case expr of
        TailCall fn actuals -> Call fn actuals
        Eif c e1 e2 -> Eif c (eliminateTCs e1) (eliminateTCs e2)
        UnaryOp uop e -> UnaryOp uop (eliminateTCs e)
        BinaryOp bop e1 e2 -> BinaryOp bop (eliminateTCs e1) (eliminateTCs e2)
        CaseF cid scr brs -> CaseF cid scr (L.map (second eliminateTCs) brs)
        e@_ -> e

--------------------------------------------------------------------------------------------------
-- Terms:                     **TC-OPTIMIZED** | **TC-CANDIDATE**                                |
--------------------------------------------------------------------------------------------------
-- Core task:                                                                                    |
--  Decide if a function call, given function' s name, can truly be tc-optimized                 |
--  Note: This is for the whole program                                                          |
--------------------------------------------------------------------------------------------------


-------------------------------------------------------------------------
-- Subtask No. 1:                                                       |
-- Given a function' s formal and actual parameters in a function call  |
--  find all variables in a function call                               |
-- Returns all variables in an actual along with the evaluation order.  |
-- Purpose:                                                             |
--  * Variable lookup: We care for its evaluation order                 |
--  * Case lazy: Function call is not TC-OPTIMIZED                      |
-------------------------------------------------------------------------
-- Notes:                                                               |
--  * ? aux. functions                                                  |
--  * Aux. no1 and no2 are co-dependent                                 |
-------------------------------------------------------------------------

-------------------------------------------------------------------------
-- Auxiliary function no.1 for subtask 1:                               |
--  * Find scrutinee' s dependencies                                    |
--      This function should work with nested case expressions          |
--  * We can have a case expr in:                                       |
--      1. Case (obvious), 2. If, 3. BinaryOp, 4. UnaryOp               |
-------------------------------------------------------------------------
findCProjDeps :: Expr -> [(CaseID, [VN])]
findCProjDeps expr =
    case expr of
        CaseF cid scr brs -> 
            let nestedCases = L.foldr (\(_, e) acc -> findCProjDeps e ++ acc) [] brs
            in  (cid, findVarsInExpr scr) : nestedCases
        Eif c el er -> findCProjDeps el ++ findCProjDeps er
        BinaryOp _ el er -> findCProjDeps el ++ findCProjDeps er
        UnaryOp _ e -> findCProjDeps e
        e@_ -> error ("findCProjDeps: Unhandled expr = " ++ show e)

-----------------------------------------------------------------
-- Auxiliary function no.2 for subtask 1:                       |
--  Find free variables in actual parameter                     |
--  * Assuming case expr is forbidden in actuals                |
--  * This function is also used to find free vars in scrutinee |
-----------------------------------------------------------------
findVarsInExpr :: Actual -> [VN]
findVarsInExpr e = 
    let cprojDeps = findCProjDeps e
    in  case e of
            EVar v -> [v]
            EInt n -> []
            CProj cid cpos -> fromJust (L.lookup cid cprojDeps)
            Nil -> []
            UnaryOp _ e -> findVarsInExpr e
            BinaryOp _ el er -> findVarsInExpr el ++ findVarsInExpr er
            Eif c el er -> findVarsInExpr c ++ findVarsInExpr el ++ findVarsInExpr er
            ConstrF _ exprs -> L.foldr (\a b -> findVarsInExpr a ++ b) [] exprs
            Call _ actuals -> L.foldr (\a b -> findVarsInExpr a ++ b) [] actuals 
            CaseF{} -> error "Assuming: Case expression in actual"
            TailCall{} -> error "TailCall should not be in a function' s actuals"

----------------------------------------------------------------------
-- Auxiliary function no.3 for subtask 1:                            |   
--  Given the actual parameters of a function-call,                  |
--  find variable dependencies.                                      |
----------------------------------------------------------------------
actualsDeps :: [Actual] -> [[VN]]
actualsDeps = L.map findVarsInExpr

--------------------------------------------------------------------------------------
-- Given caller' s formals, callee' s formals and dependencies:                      |
--  1. Check if the function belongs to tc-candidates. [x]                           |
--  2. If 1, Find if the tc-candidate function call can be tc-optimized.             |
-- *******************************RETURN VALUE***************************************|
--  Case Empty list is returned:                                                     |
--    If a dependent variable in actuals isn't in lazy order (caller or callee)      |
--  Else: [0-based index of formal in caller]                                        |
-- *********************************PURPOSE******************************************|
--  We need to know the dependencies, when another function call comes up in         |
--  the BinaryOp operation.                                                          |
--------------------------------------------------------------------------------------
funInCandies :: FN -> [FN] -> Bool
funInCandies fn candis = 
    case L.elemIndex fn candis of
        Nothing -> False
        Just _  -> True
-- Dependencies by caller' s formals. What about CAFs?
depsInFCall :: [Formal] -- caller's formals
            -> [Formal] -- callee's formals
            -> [[VN]]   -- variables in actuals
            -> [Int]    -- indexes in caller' s formals
depsInFCall callersf calleesf vars = 
    let 
        depsInFCall' :: [VN] -> Int -> [Int]
        depsInFCall' [] _ = []
        depsInFCall' (v : vs) i = 
            let info@(order, vtype) = fromMaybe (error $ "Var = " ++ v) (L.lookup v callersf)
            in  case vtype of
                    TCons _ -> 
                        case elemIndex (v, info) callersf of
                            Nothing -> error "depsInFCall: index not found"
                            Just ix -> ix : depsInFCall' vs (i + 1) 
                    _ -> depsInFCall' vs (i + 1)
    in  L.nub $ L.foldr (\a b -> depsInFCall' a 0 ++ b) [] vars 

---------------------------------------------------------------------------------------------
-- BinaryOp operation: Sharing problem occurs.                                              |
--  Contains 2 or more functions that _share_ a lazy argument.                              |
--      If the argument is non-numerical and these functions are _TC-optimized_,            |
--      the latest form of the argument is evaluated                                        |
--      and thus the single-evaluation property is viollated.                               |
-- We have 4 cases:                                                                         |
--   Case A: BinaryOp bop fc@(Call fn1 actuals1) er, where er /= Call{}                     |
--      fc can be optimized if and only if:                                                 |
--          not a function call in er that shares a lazy, non-numerical actual with fc      |
--   Case B: BinaryOp bop el fc@(Call fn1 actuals1), where el /= Call{}                     |
--   Case C: BinaryOp bop (Call fn1 actuals1) (Call fn2 actuals2)                           |
--      1st application is a call to a non tail-call version                                |
--      2nd application is a call to a tc-candi                                             |
--   Case D: BinaryOp bop el er, where el /= Call{} and er /= Call {}                       |
-- What el or er can be?                                                                    |
--  The program is considered type-safe. So, el or er can be:                               |
--      1. integer, 2. var, 3. CProj --> variable                                           |
--      4. function call -> this is case C                                                  |
--      5. if expr, 6. BinaryOp                                                             |
--      7. nothing else than 1-6                                                            |
-- Case 1, 2, 3 lead immediately to decide optimization. Case 4, also.                      |
-- Case 5 or 6 need further work.                                                           |
---------------------------------------------------------------------------------------------
-- Q: What happens with Cons????????                                                        |
---------------------------------------------------------------------------------------------

-- Maybe, try to find the last TC-Candidate' s dependencies?
dataAnalysisE :: 
    [Formal]  -- caller' s formals
    -> [FN]   -- TC-Candidates
    -> Expr
    -> State (Program, FunctionsMap, [Int]) Expr
dataAnalysisE callerSign candis e = do
    st0@(progST, funsMap, depST) <- get
    case e of
        BinaryOp bop el er -> do
            let (er', st1) = runState (dataAnalysisE callerSign candis er) st0
                (el', st2) = runState (dataAnalysisE callerSign candis el) st1
            put st2
            return (BinaryOp bop el' er')
        UnaryOp uop e -> do
            let (e', st1) = runState (dataAnalysisE callerSign candis e) st0
            put st1
            return (UnaryOp uop e')
        Eif c el er -> do -- condition needs special treatment [?]
            let (er', st1) = runState (dataAnalysisE callerSign candis er) st0
                (el', st2) = runState (dataAnalysisE callerSign candis el) st1
            put st2
            return (Eif c el' er')
        CaseF cid scr brs -> do
            let (exprPaths, st1) = mapM__ (dataAnalysisE callerSign candis) st0 (L.map snd brs)
                brs' = zip (L.map fst brs) exprPaths
            put st1
            return (CaseF cid scr brs)
        funcall@(Call fn actuals) -> do
            let (fnSign, body, depth) = fromJust (Map.lookup fn funsMap)
                depMember = depsInFCall callerSign fnSign (actualsDeps actuals)
                depIntersect = depST `L.intersect` depMember
                depST' = depST `L.union` depMember
                (funcall1, progST1, funsMap1) = 
                    case depIntersect of
                        [] -> (funcall, progST, funsMap)
                        _  -> 
                            let fn' = "nonTC-" ++ fn
                                funcall' = Call fn' actuals
                                (progST', funsMap') = 
                                    case Map.lookup fn' funsMap of
                                        Nothing -> (addNonTCcandi progST fn, Map.insert fn' (fnSign, eliminateTCs body, depth) funsMap)
                                        Just _ -> (progST, funsMap)
                            in  (funcall', progST', funsMap')
            put (progST1, funsMap1, depST')
            return funcall1
        tailcall@(TailCall n actuals) -> return tailcall
        var@(EVar v) -> return var
        cproj@(CProj cid cpos) -> return cproj
        Nil -> return Nil
        cons@(ConstrF tag exprs) -> return cons
        int@(EInt n) -> return int
        -- _ -> error ("Unhandled expr = " ++ show e)

dataAnalysisP _ [] funsMap = funsMap
dataAnalysisP candies (Fun fn formals body : fdefs) funsMap =
    let p = inverseFunsMap funsMap 
        st0 = (p, funsMap, [])
        (body', st1) = runState (dataAnalysisE formals candies body) st0
        (p', funsMap', _) = st1
        funsMap'' = Map.insert fn (formals, body', 1) funsMap'
    in  dataAnalysisP candies fdefs funsMap''

inverseFunsMap funsMap = 
    let funsList = Map.toList funsMap 
        transF (fn, (formals, body, depth)) = Fun fn formals body
    in  L.map transF funsList

-- Fire the analysis!
analysis :: Program -> Program
analysis p = 
    let (annoProg, candies) = spotTCs p
        funsMap = functionMap annoProg Map.empty
        funsMap' = dataAnalysisP candies annoProg funsMap 
        prog = inverseFunsMap  funsMap'
    in  prog

-- Generic purpose function
mapM__ :: (a -> State s a) -> s -> [a] -> ([a], s)
mapM__ f s [] = ([], s) 
mapM__ f s (h : t) = 
    let (h', s') = runState (f h) s
    in  mapM__ f s' t


-------------------------------------
-- | Make CAFs list.                |
-- | Returns a list of CAFs names.  |
-------------------------------------
makeCAFs :: FunctionsMap -> [FN]
makeCAFs funsMap = 
    let funsList = inverseFunsMap funsMap

        findCAFs :: [FDef] -> [FN]
        findCAFs [] = []
        findCAFs (Fun fn formals _ : fs) =
            case formals of
                [] -> fn : findCAFs fs
                _  -> findCAFs fs
    in  findCAFs funsList


-- Helper testing function for expression
callInFun :: FDef -> [(FN, [(FN, [Actual])])]
callInFun (Fun fn formals body) = [(fn, tcCandInBody body)]
    where
        tcCandInBody :: Expr -> [(FN, [Actual])]
        tcCandInBody e = 
            case e of
                EVar _ -> []
                EInt _ -> []
                CProj{} -> []
                Nil -> []
                UnaryOp _ e1 -> tcCandInBody e1
                BinaryOp _ e1 e2 -> tcCandInBody e1 ++ tcCandInBody e2
                Eif c e1 e2 -> tcCandInBody e1 ++ tcCandInBody e2
                CaseF _ _ brs -> L.foldr (\(x, y) b -> tcCandInBody y ++ b) [] brs
                ConstrF _ exprs -> L.foldr (\a b -> tcCandInBody a ++ b) [] exprs
                Call n actuals -> [(n, actuals)]
                TailCall n actuals -> []

-- Helper testing function for the whole program
callInProgram :: Program -> [(FN, [(FN, [Actual])])]
callInProgram [] = []
callInProgram fdefs = L.foldr ((++) . callInFun) [] fdefs