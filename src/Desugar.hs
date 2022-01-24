{-# Language NamedFieldPuns, ScopedTypeVariables #-}
module Desugar 
    ( desugarLang )
where

import Control.Monad
import Control.Monad.State
import qualified Lang as L
import qualified Core as C

type Generator a = State Int a

fresh :: Generator C.Label
fresh = do
    x <- get
    let label = C.L $ show x
    put $ x + 1
    return label


desugarLang :: L.Exp -> C.Exp
desugarLang e = evalState (desugar e) initGen

initGen = 0

desugar :: L.Exp -> Generator C.Exp
desugar e = fresh >>= (`desugarL`  e)

desugarL :: C.Label -> L.Exp -> Generator C.Exp
desugarL l (L.Num i) = return $ C.Num l i
desugarL l (L.Bool b) = return $ C.Bool l b
desugarL l (L.Var x) = return $ C.Var l x 
desugarL l (L.Op2 o e1 e2) = do 
    d1 <- desugar e1
    d2 <- desugar e2
    return $ C.Op2 l (desugarOp o) d1 d2
desugarL l (L.If e0 e1 e2) = do
    d0 <- desugar e0
    d1 <- desugar e1
    d2 <- desugar e2
    return $ C.If l d0 d1 d2
desugarL l (xs L.:-> e) = do
    d <- desugar e 
    ls <- forM xs (const fresh)
    return $ foldr (\(x,l) e -> C.Abs l x e) d $ zip xs ls
desugarL l (L.Letrec def e) = do
    l' <- fresh
    def' <- desugarDef def
    d <- desugar e
    return $ C.App l' (C.Abs l (L.name def) d) def'
desugarL l (f L.:@ args) = do
    f' <- desugar f
    args' <- forM args desugar
    ls <- forM args (const fresh)
    return $ foldr (\(arg, l) f -> C.App l f arg) f' (zip args' ls)

desugarDef :: L.Definition -> Generator C.Exp
desugarDef L.Def { L.name, L.args, L.body } = do 
    l <- fresh
    body <- desugar $ args L.:-> body
    return $ C.Rec l name body 


desugarOp L.Add = C.Add
desugarOp L.Sub = C.Sub
desugarOp L.Mult = C.Mult
desugarOp L.Div = C.Div
desugarOp L.Eq = C.Eq

