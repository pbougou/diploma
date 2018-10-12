module Parser (
  parseExpr,
  parseProgram,
  program,
  sequenceOfFns,
  correctCaseP,
  scopingP
 ) where

import Debug.Trace

import Grammar
import Lexer

import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L

import Text.Parsec.Expr
import Text.Parsec
import Text.Parsec.String

import Data.Functor
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import Control.Monad

import Control.Monad.State
import qualified Control.Monad.State as ST

import Control.Arrow

eint = EInt <$> integer

evar = EVar <$> identifier

binaryOp ch fun = Infix $ reservedOp ch $> fun
unaryOp ch fun = Prefix $ reservedOp ch $> fun
opAssoc = [
            [
              unaryOp "+" EUnPlus,
              unaryOp "-" EUnMinus
            ],
            [
              binaryOp "*" EMul AssocLeft,
              binaryOp "/" EDiv AssocLeft,
              binaryOp "%" EMod AssocLeft
            ],
            [
              binaryOp "+" EAdd AssocLeft,
              binaryOp "-" ESub AssocLeft
            ]
          ]
opExpr = buildExpressionParser opAssoc term 

ifExpr =
  reserved "if" *> (
    Eif <$>
      expression <*> (reserved "then" *> expression) <*> (reserved "else" *> expression))

callExpr = do
  reserved "call"
  (x : xs) <- sepBy1 expression whitespace
  case x of (EVar y)  -> if head y /= '#' && head y /= '!' then return $ Call y xs
                         else fail "call: function name starts with a letter"
            _ -> error "Function call"


functionDef = reserved "fun" *> do
    ids <- sepBy1 identifier whitespace
    reservedOp "="
    e <- expression
    case ids of []       -> error "Function must have a name"
                (x : xs) -> if head x /= '#' && head x /= '!' then return $ Fun x (getFormals xs) e
                            else fail "function name starts with a letter"
  where
    getFormals = 
      map (
        \x -> (case head x of
                  '#'  -> (tail x, CBN)
                  '!'  -> (tail x, CBV)
                  _    -> (x, Lazy)))

constructor = do 
  reserved "Cons" 
  hd <- expression
  tl <- expression -- constructor <|> nilConstr <|> parens constructor <|> parens nilConstr
  case tl of
    ConstrF "Nil" []  -> return $ ConstrF "Cons" [hd] 
    _           -> return $ ConstrF "Cons" (hd : [tl])

nilConstr = do reserved "Nil" 
               return $ ConstrF "Nil" []

caseF = do
  reserved "case"
  e <- expression
  reserved "of"
  lines <- many line
  return $ CaseF 42 e lines

line = do
  cons <- constructor <|> nilConstr <|> eint
  reserved "->"
  e <- expression
  return (cons, e)

sequenceOfFns = sepBy1 functionDef semi

term = 
      eint
  <|> evar
  <|> parens expression

expression = 
      callExpr
  <|> constructor
  <|> nilConstr
  <|> caseF
  <|> ifExpr
  <|> opExpr
  <|> term
 

program = sequenceOfFns

parseExpr :: String -> IO Expr
parseExpr s =
  case parse (expression <* eof) "" s of
    Right e  -> return e
    Left err -> error $ "parsing expression " ++ show err

parseProgram :: String -> IO Program
parseProgram prog = case parse (program <* eof) "" prog of
                      Right e  -> return e
                      Left err -> print err >> fail "parse error"

correctCaseE :: Expr -> ST.State Integer Expr
-- missing call, tailcall.. later fix
correctCaseE e = do 
  n <- get 
  let (v, s) = case e of
              CaseF _ e' exprs ->
                let (e'', n')     = runState (correctCaseE e') (n + 1)
                    fsts = L.map fst exprs
                    snds = L.map snd exprs
                    (snds', n'') = correctCaseEs snds [] n'
                    exprs' = zip fsts snds'
                in  (CaseF n e'' exprs', n'')
              EAdd el er  -> 
                let (el', n')  = runState (correctCaseE el) n
                    (er', n'') = runState (correctCaseE er) n'
                in  (EAdd el' er', n'')
              ESub el er  -> 
                let (el', n')  = runState (correctCaseE el) n
                    (er', n'') = runState (correctCaseE er) n'
                in  (ESub el' er', n'')
              EMul el er  -> 
                let (el', n')  = runState (correctCaseE el) n
                    (er', n'') = runState (correctCaseE er) n'
                in  (EMul el' er', n'')
              EDiv el er  -> 
                let (el', n')  = runState (correctCaseE el) n
                    (er', n'') = runState (correctCaseE er) n'
                in  (EDiv el' er', n'')
              EMod el er  -> 
                let (el', n')  = runState (correctCaseE el) n
                    (er', n'') = runState (correctCaseE er) n'
                in  (EMod el' er', n'')
              EUnPlus el  -> 
                let (el', n'')  = runState (correctCaseE el) n
                in  (EUnPlus el', n'')
              EUnMinus el -> 
                let (el', n'')  = runState (correctCaseE el) n
                in  (EUnMinus el', n'')
              Eif c el er -> 
                let (el', n')  = runState (correctCaseE el) n
                    (er', n'') = runState (correctCaseE er) n'
                in  (Eif c el' er', n'')
              _           -> (e, n)
  put s
  return v


correctCaseEs :: [Expr] -> [Expr] -> Integer -> ([Expr], Integer)
correctCaseEs [] acc n = (reverse acc, n)
correctCaseEs (e : es) acc n =
  let (e', n') = runState (correctCaseE e) n 
  in  correctCaseEs es (e' : acc) n' 

correctCaseP :: Program -> Integer -> Program
correctCaseP []             _ = [] 
correctCaseP (fdef : fdefs) n =
  let Fun x y e = fdef 
      (e', s')  = ST.runState (correctCaseE e) n
      fdef'     = Fun x y e'
  in  fdef' : correctCaseP fdefs (n + 1)

-- A variable might belong to:
--    a. formals --> stack frame --> Eval: 
--      depends if formal is cbv, cbn, lazy
--    b. bound to pattern --> Susp [expression list] as el --> el !! cpos 
scopingP :: Program -> Program
scopingP []             = []
scopingP (fdef : fdefs) = scopingF fdef : scopingP fdefs
  where
    scopingF :: FDef -> FDef
    scopingF (Fun fn frmls expr) = Fun fn frmls (scopingE [] expr)
    
    scopingE :: [(CaseID, [VN])] -> Expr -> Expr
    -- scopingE scopes expr | trace ("Trace scopingE: expr = " ++ show expr ++ ", scopes = " ++ show scopes) False = undefined
    scopingE scopes expr =
      case expr of
        EInt n -> EInt n
        EUnPlus e  -> EUnPlus $ scopingE scopes e
        EUnMinus e -> EUnMinus $ scopingE scopes e
        EAdd el er -> EAdd (scopingE scopes el) (scopingE scopes er)
        ESub el er -> ESub (scopingE scopes el) (scopingE scopes er)
        EMul el er -> EMul (scopingE scopes el) (scopingE scopes er)
        EDiv el er -> EDiv (scopingE scopes el) (scopingE scopes er)
        EMod el er -> EMod (scopingE scopes el) (scopingE scopes er)
        Eif c thenE elseE -> Eif (scopingE scopes c) (scopingE scopes thenE) (scopingE scopes elseE)
        ConstrF tag exprs -> ConstrF tag (L.map (scopingE scopes) exprs)
        Call fn actuals -> Call fn (L.map (scopingE scopes) actuals)
        EVar v -> 
          -- Lookup in symbol table
          --  Should return:
          --    EVar v, if var is bound with a formal parameter
          --    CProj CaseID CPos - CPos is constructor position in case patterns - otherwise
          let 
            searchST :: VN -> [(CaseID, [VN])] -> Maybe (CaseID, CPos)
            searchST _  [] = Nothing
            searchST vn st@((id, [x, y]) : t)
              | vn == x   = Just (id, 0) 
              | vn == y   = Just (id, 1)
              | otherwise = searchST vn t
          in  case searchST v scopes of
                Nothing         -> EVar v
                Just (id, cpos) -> CProj id cpos
          -- error "Var: Not yet implemented "
        CaseF id e cases -> 
          -- Case clause opens: Scope opens if e : Cons 
          --  Also assume pattern matching is exhaustive.
          --  ==> cases = [(patt1, e1), (patt2, e2)], 
          --          if e is an expression that returns a Constructor
            let cases' =  case cases of
                            [case1@(patt1, e1), case2@(patt2, e2)] ->
                              case [patt1, patt2] of                                
                                [ConstrF "Nil" [], ConstrF "Cons" [EVar x, EVar y]] ->
                                  let scope' = (id, [x, y])
                                  in  [(patt1, scopingE scopes e1), (patt2, scopingE (scope' : scopes) e2)]
                                [ConstrF "Cons" [EVar x, EVar y], ConstrF "Nil" []] ->
                                  let scope' = (id, [x, y])
                                  in  [(patt1, scopingE (scope' : scopes) e1), (patt2, scopingE scopes e2)]
                                _ -> cases
                                  -- error "Pattern matching is complex or non-exhaustive"
                            _ -> cases
            in  CaseF id (scopingE scopes e) cases'
              -- CaseF id e (L.map (fst &&& (scopingE scopes . snd)) cases) 
        CProj _ _ -> error "CProj: This should be unreached"

