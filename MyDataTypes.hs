module MyDataTypes where

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
