module Lang where

data Exp
    = Num Int
    | Bool Bool
    | Var String
    | Op2 Op Exp Exp 
    | If Exp Exp Exp
    | [String] :-> Exp
    | Letrec Definition Exp
    | Exp :@ [Exp]
    deriving (Show, Eq)


data Op = Add | Sub | Div | Mult | Eq
    deriving (Show, Eq)

data Definition = Def 
    { name :: String
    , args :: [String]
    , body :: Exp }
    deriving (Show, Eq)


