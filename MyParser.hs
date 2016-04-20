module MyParser(parseFile,parseStr) where

import Text.Parsec
import System.Environment (getArgs)
import Data.Char
import Control.Applicative ((<$>), (<*>), (*>), (<*))

data TY  = INT | BOOL | FUN TY TY deriving (Show,Read)
data OP  = PLUS | MINUS | EQU | AND deriving (Show,Read)
data EXP = NAT Int | B Bool | VAL String 
         | PRIM EXP OP EXP | APP EXP EXP
         | IF {cond::EXP, tru::EXP, fal::EXP} 
         | ABS {val::String, ty::TY, e::EXP} 
         | BIND {val::String, ty::TY, e::EXP, in_::EXP}
         | REC  {f::String, ty::TY, val::String, e::EXP, in_::EXP}
         | EOF
         deriving (Show,Read)

-- 予約語
reversedList = ["&&","==","in","else","then"]
-- ファイル読み込み
parseFile = \s -> readFile s >>= parseStr
-- 構文解析
parseStr = parseTest expr
-- 全体
expr = exprAp
-- 関数適用
exprAp = do
  e1 <- exprOp
  e2 <- exprOp
  case e2 of
    EOF -> return e1
    _   -> return $ APP e1 e2
-- 二項演算子
-- 演算子の優先順位についてはまだ
-- 結合がおかしい ( +,-が右結合になっている
exprOp = do
  e <- expr_
  (PRIM e <$>
     (string "+"  *> pure PLUS  <|>
      string "-"  *> pure MINUS <|>
      string "&&" *> pure AND   <|>
      string "==" *> pure EQU)  <*> exprOp)
      <|> pure e
-- もろもろ
expr_ = do
  get_tkn >>= parseExp
  where
    parseExp = \cur -> case cur of
      "("       -> expr <* string ")" <* spaces
      "if"      -> IF <$> expr <* string "then" <*> expr <* string "else" <*> expr
      "lambda"  -> ABS <$> get_tkn <* string ":" <*> tyty <* string "." <*> expr
      "val"     -> BIND <$> get_tkn <* string ":" <*> tyty <* string "=" 
                        <*> expr <* string "in" <*> expr
      "fun"     -> REC <$> (string "(" *> get_tkn) <* string ":" <*> tyty <* string ")" 
                       <*> get_tkn <* string "=" <*> expr <* string "in" <*> expr
      ""        -> return EOF
      _
        | isNum  cur -> return $ NAT (read cur)
        | isBool cur -> return $ B (read cur)
        | otherwise  -> return $ VAL cur
-- 文字列を型に変換 ex. INT -> INT ==> FUN INT INT
tyty = do
  t <- get_ty
  (FUN (read t) <$> (string "->" *> tyty)) <|> return (read t)
-- 文字列を読み込む
get_tkn  = 
  let read_tkn = many (newline <|> space)*> (string "(" <|> many (digit <|> letter)) <* many (newline <|> space) in
    do
      e <- lookAhead read_tkn
      if elem e reversedList then pure "" else read_tkn
-- 型を表す文字列のみ読み込む
get_ty   = spaces *> (string "INT" <|> string "BOOL") <* spaces

isNum :: String -> Bool
isNum = all isDigit
isBool :: String -> Bool
isBool = \s -> if s == "True" || s == "False" then True else False
-- main ::IO()
-- main = do
  -- (head <$> getArgs) >>= parseFile
  -- 型は大文字で書く ex. INT,BOOL,INT->BOOL
  -- parseStr "if T then f 1 else f 0"
  -- parseStr "f (1+2) + 3"
  -- parseStr "((lambda x:INT . lambda y:INT . x + y) 3) 4"
  -- parseStr "(lambda x:INT . lambda y:INT . x + y) 3 4"
  -- parseStr "fun (f:INT->INT) n = f n in f 3"
