module Syntax where

data TY  = INT 
         | BOOL 
         | FUN TY TY 
         | BOTTOM String 
           deriving (Show,Read,Eq)

data OP  = PLUS 
         | MINUS
         | TIMES
         | DIV
         | EQU 
         | AND 
         | OR
           deriving (Show,Read)

data EXP = NAT Int 
         | B Bool
         | VAR String 
         | PRIM EXP OP EXP
         | APP EXP EXP
         | IF {cond::EXP, tru::EXP, fal::EXP} 
         | ABS {var::String, ty::TY, e::EXP} 
         | BIND {var::String, ty::TY, e::EXP, in_::EXP}
         | REC  {f::String, ty::TY, var::String, e::EXP, in_::EXP}
         | EOF
         deriving (Show,Read)

