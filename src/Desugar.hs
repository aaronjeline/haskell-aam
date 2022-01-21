{-# Language NamedFieldPuns, ScopedTypeVariables #-}
module Desugar where

import qualified Lang as L
import qualified Core as C


desugar :: L.Exp -> C.Exp
desugar (L.Num i) = C.Num i
desugar (L.Bool b) = C.Bool b
desugar (L.Var x) = C.Var x
desugar (L.Op2 o e1 e2) = C.Op2 (desugarOp o) (desugar e1) (desugar e2)
desugar (L.If e0 e1 e2) = C.If (desugar e0) (desugar e1) (desugar e2)
desugar (xs L.:-> e) = foldr (C.:->) (desugar e) xs
desugar (L.Letrec def e) = (L.name def C.:-> desugar e) C.:@ def'
    where def' = desugarDef def
desugar (f L.:@ args) = foldr (flip (C.:@)) (desugar f) (desugar <$> args)

desugarDef :: L.Definition -> C.Exp
desugarDef L.Def { L.name, L.args, L.body } = C.Rec name (desugar $ args L.:-> body)


desugarOp L.Add = C.Add
desugarOp L.Sub = C.Sub
desugarOp L.Mult = C.Mult
desugarOp L.Div = C.Div
desugarOp L.Eq = C.Eq

