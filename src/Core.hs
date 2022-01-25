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

label :: Exp -> Label
label (Num l _) = l
label (Bool l _) = l
label (Abs l _ _) = l
label (Rec l _ _) = l
label (Var l _) = l
label (Op2 l _ _ _) = l
label (If l _ _ _) = l
label (App l _ _) = l

data Op = Add | Sub | Div | Mult | Eq
    deriving (Show, Eq, Ord)
