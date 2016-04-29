module Typecheck (tycheckFile,tycheck) where

import Syntax
import Parse
import Data.Map as Map

type ENV_ty = Map String TY

tycheckFile = \s -> tycheck.parseStr <$> readFile s

tycheck = \s -> case s of
  Left err -> BOTTOM "syntax error"
  Right e  -> tycheck_ e (Map.empty)

tycheck_ = \e -> \env -> case e of
  NAT n -> INT
  B   b -> BOOL
  VAR x -> envLook (VAR x) env
  IF e1 e2 e3 -> let t1 = tycheck_ e1 env in
                 let t2 = tycheck_ e2 env in
                 let t3 = tycheck_ e3 env in
                 if t1==BOOL && t2==t3 then t2 else BOTTOM "type error in if statement"
  BIND x t e1 e2 -> let t1 = tycheck_ e1 env in tycheck_ e2 (envAdd x t1 env)
  ABS x t1 e -> let t2 = tycheck_ e (envAdd x t1 env) in FUN t1 t2
  REC f t x e1 e2 -> case t of
                      FUN dom cod -> let t1 = tycheck_ e1 (envAdd f t . envAdd x cod $ env) in
                                     let t2 = tycheck_ e2 (envAdd f t env) in
                                         if t1==cod then t2 else BOTTOM "REC : codomain"
  APP e1 e2  -> let t = tycheck_ e1 env in
                  case t of
                    FUN t1 t2 -> let t3 = tycheck_ e2 env in
                                  if t1 == t3 then t2 else BOTTOM "apply e1 e2 : types not match"
                    _ -> BOTTOM "apply e1 e2 : types not match"
  PRIM e1 op e2 -> let (t1,t2) = (tycheck_ e1 env,tycheck_ e2 env) in
    case op of
      PLUS  -> if (t1,t2) == (INT,INT) then INT else BOTTOM "(+)::INT->INT->INT"
      MINUS -> if (t1,t2) == (INT,INT) then INT else BOTTOM "(-)::INT->INT->INT"
      EQU   -> if t1 == t2 then BOOL else BOTTOM "(==)::a->a->BOOL"
      AND   -> if t1 == t2 then BOOL else BOTTOM "(&&)::INT->INT->INT"
  _ -> BOTTOM ""

envLook :: EXP -> ENV_ty -> TY
envLook (VAL str) env =
  case Map.lookup str env of
    Just a -> a
    _      -> BOTTOM "not in scope"

envAdd :: String -> TY -> ENV_ty -> ENV_ty
envAdd x e env = Map.insert x e env

main :: IO()
main = do
  -- print $ tycheck $ parseStr "fun (f:INT->INT) x = if x==0 then 1 else if x==1 then 1 else f(x-1) + f(x-2) in f 6"
  print 0
