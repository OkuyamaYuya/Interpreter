module MyInterpreter (evalFile,eval) where

import MyParser
import MyDataTypes
import Data.Map as Map
import Debug.Trace
-- for debug : trace str -> a -> a
data VALUE = NN Int | BB Bool | CLOSURE {env::ENV, val::String, e::EXP}
                              | RECCLOS {env::ENV, f::String, val::String, e::EXP}
                              | ERR String deriving (Show)
type ENV   = Map String VALUE

evalFile = \s -> eval.parseStr <$> readFile s

eval = \s -> case s of
  Left err -> ERR "syntax error"
  Right e  -> eval_ e (Map.empty)

eval_ = \e -> \env -> case e of
  NAT n   -> NN n
  B   b   -> BB b
  VAL x   -> envLook (VAL x) env
  IF c t f-> case eval_ c env of
      BB True  -> eval_ t env
      BB False -> eval_ f env
  ABS x _ e -> CLOSURE env x e
  APP e1 e2 -> let v1 = eval_ e1 env in case v1 of
      CLOSURE env_ x e3 -> let v2 = eval_ e2 env 
                           in eval_ e3 (envAdd x v2 env_)
      RECCLOS env_ f x e3 -> let v2 = eval_ e2 env 
                             in eval_ e3 (envAdd f v1 . envAdd x v2 $ env_)
  REC f _ x e1 e2 -> eval_ e2 (envAdd f (RECCLOS env f x e1) env)
  BIND x _ e1 e2 -> let v1 = eval_ e1 env 
                    in eval_ e2 (envAdd x v1 env)
  PRIM e1 op e2 -> let (v1,v2) = (eval_ e1 env, eval_ e2 env) 
                   in
                    case op of
                      PLUS  -> v1`plus`v2
                      MINUS -> v1`minus`v2
                      EQU   -> v1`equ`v2
                      AND   -> v1`andd`v2

plus = \v1 -> \v2 -> let (NN a,NN b) = (v1,v2) in NN $ a+b
minus = \v1 -> \v2 -> let (NN a,NN b) = (v1,v2) in NN $ a-b
equ = \v1 -> \v2 -> let (NN a,NN b) = (v1,v2) in BB $ a==b
andd = \v1 -> \v2 -> let (BB a,BB b) = (v1,v2) in BB $ a&&b

envLook :: EXP -> ENV -> VALUE
envLook (VAL str) env =
  case Map.lookup str env of
    Just a -> a
    _      -> ERR "not in scope"

envAdd :: String -> VALUE -> ENV -> ENV
envAdd x e env = Map.insert x e env

main :: IO()
main = do
  -- 静的スコープにしたい
  -- フィボナッチ数列
  -- print $ eval $ parseStr "fun (f:INT->INT) x = if x==0 then 1 else if x==1 then 1 else f(x-1) + f(x-2) in f 6"
  print 0
