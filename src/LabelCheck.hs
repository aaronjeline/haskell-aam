{-# Language ScopedTypeVariables #-}
module LabelCheck where
import Core
import Util
import qualified Data.Set as S

labels :: Exp -> [Label]
labels (Num l _) = [l]
labels (Bool l _) = [l]
labels (Abs l _ b) = l : labels b
labels (Rec l _ b) = l : labels b
labels (Var l _) = [l]
labels (If l e0 e1 e2) = l : labels e0 ++ labels e1 ++ labels e2
labels (App l e0 e1) = l : labels e0 ++ labels e1
labels (Op2 l _ e0 e1) = l : labels e0 ++ labels e1

noRepeats :: forall a. (Eq a) => (Ord a) => [a] -> Bool
noRepeats xs = (S.fromList xs |> S.size) == length xs

expHasNoRepeatLabels :: Exp -> Bool
expHasNoRepeatLabels = noRepeats . labels

