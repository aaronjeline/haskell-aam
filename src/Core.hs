module Core where

newtype Label = L String
    deriving (Show, Eq, Ord)

data Exp
    = Num Label Int
    | Bool Label Bool
    | Abs Label String Exp
    | Rec Label String Exp
    | Var Label String 
    | Op2 Label Op Exp Exp 
    | If Label Exp Exp Exp
    | App Label Exp Exp 
    deriving (Show, Eq, Ord)


data Op = Add | Sub | Div | Mult | Eq
    deriving (Show, Eq, Ord)
