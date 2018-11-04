module IntermediateTransform (
    buildHeapGlobals
) where
import Heap
import RuntimeStructs
import Grammar
import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Data.Maybe

-- Tranform AST to an equivalent AST with let bindings
--  * In a Cons expr, expression list should have only variables
--  * All actuals in a constructor expression, are local definitions in a top-level function.
transform :: Program -> Program
transform = L.map transformFDef

transformFDef :: FDef -> FDef
transformFDef (Fun fn formals funbody) = 
    Fun fn formals (transformExpr funbody)

transformExpr :: Expr -> Expr
transformExpr e = e


-- Initialize runtime: 
--      * global(top-level) functions (sigma environment)
--      * heap addresses(address to heap building block)
buildHeapGlobals :: Program -> (Globals, Heap Block)
buildHeapGlobals p = 
    let 
        aux :: Program -> Globals -> Heap Block -> (Globals, Heap Block)
        aux [] globals heap = (globals, heap)
        aux (fdef : fdefs) globals heap = 
            let (globals', heap') = processFDef fdef globals heap
            in  aux fdefs globals' heap'
    in  aux p Map.empty hInitial 

processFDef :: FDef -> Globals -> Heap Block -> (Globals, Heap Block)
processFDef (Fun fn formals funbody) globals heap =
    let (heap', addr) = 
            case formals of
                [] -> hAlloc heap (CAF funbody)
                _  -> hAlloc heap (FunH formals funbody)
        globals' = Map.insert fn addr globals
    in  (globals', heap')

