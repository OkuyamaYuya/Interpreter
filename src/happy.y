{
module Happy (parse) where

import qualified Syntax as S
import qualified Token as T
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  Bool { T.Bool $$ }
  Int { T.Int $$ }
  Var { T.Var $$ }
  '(' { T.LPar }
  ')' { T.RPar }
  '-' { T.Minus }
  '+' { T.Plus }
  '*' { T.Times }
  '/' { T.Div }
  '=='  { T.Eq }
  '&&'  { T.And }
  '||'  { T.Or }
  if  { T.If }
  then { T.Then }
  else { T.Else }
  let { T.Let }
  in { T.In }
  rec { T.Rec }
  '.' { T.Dot }
  ':' { T.Colon }
  '=' { T.Bind }
  '->' { T.Arrow }
  lambda { T.Lambda }
  TyInt { T.TyInt }
  TyBool { T.TyBool }

%right in
%left '+' '-'
%left '*' '/'

%%

Exp : let var '=' Exp in Exp { S.Let $2 $4 $6 }
    | Exp '+' Exp            { Plus $1 $3 }
    | Exp '-' Exp            { Minus $1 $3 }
    | Exp '*' Exp            { Times $1 $3 }
    | Exp '/' Exp            { Div $1 $3 }
    | '(' Exp ')'            { $2 }
    | Int                    { S.Int $1 }
    | Var                    { S.Var $1 }
    | Bool                   { S.Bool $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
