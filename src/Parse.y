{
module Parse (parse) where

import Base
import qualified Syntax as S
import qualified Token as T
}

%name parse
%tokentype { T.Token }
%error  { parseError }
%monad { Result } { thenR } { returnR }

%token
  bool { T.Bool $$ }
  int { T.Int $$ }
  var { T.Var $$ }
  '(' { T.Lparen }
  ')' { T.Rparen }
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
  '=' { T.Assign }
  '->' { T.Arrow }
  lambda { T.Lambda }
  tyInt { T.TyInt }
  tyBool { T.TyBool }

%right in '->'
%left '+' '-'
%left '*' '/'

%%

Exp : Exp '+' Exp             { S.PRIM $1 S.PLUS $3 }
    | Exp '-' Exp             { S.PRIM $1 S.MINUS $3 }
    | Exp '*' Exp             { S.PRIM $1 S.TIMES $3 }
    | Exp '/' Exp             { S.PRIM $1 S.DIV $3 }
    | Exp '&&' Exp            { S.PRIM $1 S.AND $3 }
    | Exp '||' Exp            { S.PRIM $1 S.OR  $3 }
    | Exp '==' Exp            { S.PRIM $1 S.EQU $3 }
    | '(' Exp ')'             { $2 }
    | int                     { S.NAT $1 }
    | var                     { S.VAR $1 }
    | bool                    { S.B $1 }
    | lambda var ':' Type '.' Exp           { S.ABS $2 $4 $6 }
    | if Exp then Exp else Exp              { S.IF $2 $4 $6 }
    | let var ':' Type '=' Exp in Exp       { S.BIND $2 $4 $6 $8 }
    | let rec '(' var ':' Type ')' var '=' Exp in Exp { S.REC $4 $6 $8 $10 $12 }

Type : tyInt  { S.INT }
     | tyBool { S.BOOL }
     | Type '->' Type { S.FUN $1 $3 }
     | '(' Type ')'   { $2 }


{

thenR = (>>=)
returnR = return

parseError tokens = Reject "parse error"

}
