{

module Lex where

import qualified Token as T

}
%wrapper "basic"

$digit = [0-9]
$lower = [a-z]
$alpha = [a-zA-Z]

tokens :-
    $white+ ;
    "(" { \s -> T.Lparen }
    ")" { \s -> T.Rparen }
    "true" { \s -> T.Bool True }
    "false" { \s -> T.Bool False }
    $digit+ { \s -> T.Int (read s) }
    "-" { \s -> T.Minus }
    "+" { \s -> T.Plus }
    "*" { \s -> T.Times }
    "/" { \s -> T.Div }
    "==" { \s -> T.Eq }
    "||" { \s -> T.Or }
    "&&" { \s -> T.And }
    "if" { \s -> T.If }
    "then" { \s -> T.Then }
    "else" { \s -> T.Else }
    "let" { \s -> T.Let }
    "rec" { \s -> T.Rec }
    "in" { \s -> T.In }
    "\" { \s -> T.Lambda }
    "Int" { \s -> T.TyInt }
    "Bool" { \s -> T.TyBool }
    "->" { \s -> T.Arrow }
    "." { \s -> T.Dot }
    ":" { \s -> T.Colon }
    "=" { \s -> T.Bind }
    $lower [$alpha $digit \_ \’]* { \s -> T.Var s }
{

scanTokens = alexScanTokens

}
