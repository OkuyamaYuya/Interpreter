module Token where

data Token = Let
           | Rec
           | In
           | Int Int
           | Bool Bool
           | Var String
           | Eq
           | And
           | Or
           | Plus
           | Minus
           | Times
           | Div
           | Lparen
           | Rparen
           | If
           | Then
           | Else
           | TyInt
           | TyBool
           | Arrow
           | Lambda
           | Comma
           | Colon
           | Bind
           deriving (Show,Eq)
