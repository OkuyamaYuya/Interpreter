module MyParser(parseFile,parseStr,parseIO,print_parser) where

import MyDataTypes
import Text.Parsec
import System.Environment (getArgs)
import Data.Char
import Control.Applicative ((<$>), (<*>), (*>), (<*))

-- 予約語
reversedList = ["&&","==","in","else","then"]
-- ファイル読み込み
parseFile = \s -> parseStr <$> readFile s
-- 構文解析
parseStr = parse expr "syntax error"
parseIO = parseTest expr
-- 全体
expr = exprOp
-- 二項演算子
-- 結合がおかしい +,-が右結合になっている
exprOp = do
  e <- exprAp
  (PRIM e <$>
     (string "+"  *> pure PLUS  <|>
      string "-"  *> pure MINUS <|>
      string "&&" *> pure AND   <|>
      string "==" *> pure EQU)  <*> exprOp)
      <|> pure e
-- 関数適用
exprAp = do
  e1 <- expr_
  e2 <- expr_
  case e2 of
    EOF -> return e1
    _   -> return $ APP e1 e2
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

print_parser = \s -> case s of
  Right a -> print a
  Left  a -> print a

main ::IO()
main = do
  -- 型は大文字で書く ex. INT,BOOL,INT->BOOL
  -- (head <$> getArgs) >>= parseFile >>= print_parser
  print_parser $ parseStr "1+2-3"
  print_parser $ parseStr "f 1 2"
  print_parser $ parseStr "((lambda x:INT . lambda y:INT . x + y) 3) 4"
  print_parser $ parseStr "(lambda x:INT . lambda y:INT . x + y) 3 4"
