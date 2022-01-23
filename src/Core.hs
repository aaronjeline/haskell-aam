module Core where

data Exp
    = Num Int
    | Bool Bool
    | String :-> Exp
    | Rec String Exp
    | Var String
    | Op2 Op Exp Exp 
    | If Exp Exp Exp
    | Exp :@ Exp
    deriving (Show, Eq, Ord)


data Op = Add | Sub | Div | Mult | Eq
    deriving (Show, Eq, Ord)
