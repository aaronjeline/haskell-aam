module Toplevel where

import Data.Bifunctor
import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

import Lang
import Desugar
import Analysis
import qualified Core as C
import qualified Eval as E


run :: Exp -> (E.Config, Graph)
run e = 
    let e' = desugar e in 
    let c = E.inject e' in 
    let m = E.run c in
    (E.C c, makeExternal m)

value :: [E.Config] -> E.Value
value cs = case last cs of
    E.C (Left v,_,_,_,_) -> v

ex0 :: Exp
ex0 = Num 1

ex1 = Op2 Add (Num 1) (Num 2)

ex2 :: Exp 
ex2 = ["x"] :-> Op2 Add (Var "x") (Num 1) :@ [Num 2]

add = ["x","y"] :-> Op2 Add (Var "x") (Var "y") :@ [Num 1, Num 2]

recurconst = Letrec Def { name = "const", args = ["x"], body = Var "x" } (Var "const" :@ [Num 3])

fact :: Definition
fact = Def { name = "fact", args = ["x"], body = 
    If (Op2 Eq (Var "x") (Num 0)) 
        (Num 1) 
        (Op2 Mult (Var "x") (Var "fact" :@ [Op2 Sub (Var "x") (Num 1)]))
        }


fact5 :: Exp
fact5 = Letrec fact (Var "fact" :@ [Num 5])

target = fact5

go :: IO ()
go = do
    print "Running..."
    let (c,g) = run target
    putStrLn "Done"
    let vs = allReachableHalts g c
    print vs



    
