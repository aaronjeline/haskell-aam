{-# Language ScopedTypeVariables #-}
module Analysis where
import Core
import Eval
import Util
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor 
--import Data.Graph

type Graph = M.Map Config (S.Set (Either Error Config))

{-
buildGraph :: M.Map C (S.Set (Either Error C)) -> Graph 
buildGraph m = undefined
    where
        m' = makeExternal m -}

makeExternal :: M.Map C (S.Set (Either Error C)) -> M.Map Config (S.Set (Either Error Config))
makeExternal m = listOperate m (bimap C (\s -> second C <%> s) <$>) 

listOperate :: forall k j v w. (Ord j) => M.Map k v -> ([(k,v)] -> [(j,w)]) -> M.Map j w
listOperate m f = M.toList m |> f |> M.fromList

allReachableHalts :: Graph -> Config -> S.Set Value
allReachableHalts g c = filterMap isHalted $ allReachable g c

allReachable :: Graph -> Config -> S.Set Config
allReachable g c = allReachableS g (S.singleton c) c


allReachableS :: Graph -> S.Set Config -> Config -> S.Set Config
allReachableS g seen c = case M.lookup c g of
    Nothing -> S.empty
    Just cs -> S.unions $ S.insert cs' (allReachableS g seen' <%> cs')
        where 
            seen' = seen `S.union` cs'
            cs' :: S.Set Config
            cs' = gatherRights cs `S.difference` seen



isHalted :: Config -> Maybe Value
isHalted (C (Left v,p,s,a,t)) = 
    case derefK a s of
        Left _ -> Nothing
        Right s -> if any isMt s then Just v else Nothing
isHalted (C (Right _,p,s,a,t)) = Nothing

isMt :: Either Error Kont -> Bool
isMt (Right Mt) = True
isMt _ = False


