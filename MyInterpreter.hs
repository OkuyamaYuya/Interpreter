import MyParser
import MyDataTypes
import Data.Map as Map

data VALUE = NN Int | BB Bool | ERR deriving (Show)

eval = \s -> case s of
  Left err -> ERR
  Right e  -> eval_ e

eval_ = \e -> case e of
  NAT n   -> NN n
  B   b   -> BB b
  IF c t f-> case eval_ c of
    BB True  -> eval_ t
    BB False -> eval_ f
  PRIM e1 op e2 -> let (v1,v2) = (eval_ e1, eval_ e2) in
    case op of
      PLUS  -> v1`plus`v2
      MINUS -> v1`minus`v2
      EQU   -> v1`equ`v2
      AND   -> v1`andd`v2
  _     -> ERR

plus = \v1 -> \v2 -> let (NN a,NN b) = (v1,v2) in NN $ a+b
minus = \v1 -> \v2 -> let (NN a,NN b) = (v1,v2) in NN $ a-b
equ = \v1 -> \v2 -> let (NN a,NN b) = (v1,v2) in BB $ a==b
andd = \v1 -> \v2 -> let (BB a,BB b) = (v1,v2) in BB $ a&&b

main :: IO()
main = do
  print $ eval $ parseStr "10+1"
  print $ eval $ parseStr "True && True"
  print $ eval $ parseStr "if True then 1 else 2"
  print $ eval $ parseStr "if 1==2 then 1000 else 0"
  print $ eval $ parseStr "val x:INT = 1 in x"
  
  print_parser $ parseStr "val x:INT = 1 in x"
