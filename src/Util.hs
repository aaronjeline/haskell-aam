{-# Language ScopedTypeVariables #-}
module Util where

import qualified Data.Set as S

x |> f = f x

(<%>) :: forall a b. (Ord b) => (a -> b) -> S.Set a -> S.Set b
f <%> s = S.toList s |> fmap f |> S.fromList

showEither :: Show a => Show b => Either a b -> String
showEither (Left a) = show a
showEither (Right a) = show a


contains :: forall a. (Ord a) => S.Set a -> (a -> Bool) -> Bool
contains s f = S.filter f s |> S.null |> not

    
gatherRights :: forall a b. (Ord a) => S.Set (Either b a) -> S.Set a
gatherRights s = gatherRightsAcc S.empty (S.toList s)
    where
        gatherRightsAcc :: S.Set a -> [Either b a] -> S.Set a
        gatherRightsAcc s [] = s
        gatherRightsAcc s (x:xs) =
            case x of
                Left e -> gatherRightsAcc s xs
                Right v -> gatherRightsAcc (S.insert v s) xs


filterMap :: forall a b. (Ord b) => (a -> Maybe b) -> S.Set a -> S.Set b
filterMap f s = filterMapAcc (S.toList s) S.empty
    where
        filterMapAcc :: [a] -> S.Set b -> S.Set b
        filterMapAcc [] acc = acc
        filterMapAcc (x:xs) acc = filterMapAcc xs acc'
            where 
                acc' = case f x of
                    Just x' -> S.insert x' acc
                    Nothing -> acc
