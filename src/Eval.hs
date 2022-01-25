{-# Language ScopedTypeVariables #-}
module Eval where

import Core
import Util
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf
import Data.List
import Data.Functor
import Data.Bifunctor

type Contour = [String]

newtype Config = C C
    deriving (Eq, Ord)

instance Show Config where
    show (C (vea, env, store, a, t)) = "(" ++ internal ++ ")"
        where
            internal = intercalate ", " parts
            parts = [sOfVea, show env, show store, k, show t]
            k = "K @ " ++ show a
            sOfVea = showEither vea


type C = (Either Value Exp, Env, Store, Addr, Time)

newtype Addr = Addr Integer -- (Either Label String, Contour)
    deriving (Eq, Ord)

newtype Time = T Integer -- (Maybe Label, Contour)
    deriving (Show, Eq, Ord)

instance Show Addr where
    show (Addr i) = show i

newtype Env = Env (M.Map String Addr)
    deriving (Eq, Ord)

instance Show Env where
    show (Env e) = show $ M.toList e

newtype Store = Store (M.Map Addr (S.Set (Either Value Kont)))
    deriving (Eq, Ord)

instance Show Store where
    show (Store s) = "[" ++ internal ++ "]"
        where
            internal = intercalate "," $ showEntry <$> lst
            showEntry (a,s) = printf "(%s -> { %s })" (show a) (showSet s)
            showSet s = intercalate "," $ showEither <$> S.toList s
            lst = M.toList s

derefV :: String -> Env -> Store -> Either Error [Either Error Value]
derefV x (Env p) (Store s) = 
    case (`M.lookup` s)  <$> M.lookup x p of
    Nothing -> Left $ UnboundVar x
    Just Nothing -> Left IllegalAddress
    Just (Just eOrVs) -> Right $ checkV <$> S.toList eOrVs
    where
        checkV (Left v) = Right v
        checkV (Right _) = Left NonValue


derefK :: Addr -> Store -> Either Error [Either Error Kont]
derefK a (Store s) = 
    case M.lookup a s of
        Nothing -> Left IllegalAddress
        Just eOrVs -> Right $ checkK <$> S.toList eOrVs
        where
            checkK (Left v) = Left $ NonContinuation a
            checkK (Right k) = Right k
            
tick :: C -> [Time]
tick (_,_,_,_,T i) = [T $ i + 1]

alloc :: C -> [Addr]
alloc (_,_,_,_,T t) = [Addr t]

update :: Addr -> Either Value Kont -> Store -> Store
update k v (Store s) = Store s'
    where
        s' = M.alter f k s
        set = S.singleton v
        f Nothing = Just set
        f (Just s) = Just $ s `S.union` set


allocK :: Kont -> C -> [(Addr, Store)]
allocK k c@(_,_,store,_,_) = (\a -> (a, update a (Right k) store)) <$> as
    where
        as = alloc c

allocV :: String -> Value -> Env -> C -> [(Env, Store)]
allocV x v (Env p) c@(_,_,store,_,_) = (\(s', p') -> (Env p', s')) <$> zip ss' ps'
    where
        ps' = (\a -> M.insert x a p) <$> as
        ss' = (\a -> update a (Left v) store) <$> as
        as = alloc c




data Value 
    = VI Int
    | VB Bool
    | Closure String Exp 
    deriving (Show, Eq, Ord)

typeof :: Value -> Type
typeof VI {} = Number
typeof VB {} = Boolean
typeof Closure {} = Function

typeError :: Type -> Value -> Either Error a
typeError t v = Left $ TypeError (t, typeof v)

lTypeError :: forall a. Type -> Value -> [Either Error a]
lTypeError t v = [typeError t v]

data Type
    = Number
    | Boolean
    | Function
    deriving (Show, Eq, Ord)

data Kont
    = Ar Exp Env Addr
    | Mt
    | Fn (String, Exp) Env Addr
    | IfK Exp Exp Env Addr
    | Op2_a Op Exp Env Addr
    | Op2_b Op Int Env Addr
    | RecBind String Addr
    deriving (Show, Eq, Ord)

data Error
    = UnboundVar String
    | NonValue
    | IllegalAddress 
    | NonContinuation Addr
    | DivByZero
    | TypeError (Type, Type)
    deriving (Show, Eq, Ord)

inject :: Exp -> C
inject e = (Right e, Env M.empty, s, Addr 0, tInit)
    where
        s = update (Addr 0) (Right Mt) (Store M.empty)

tInit :: Time
tInit = T 1

run :: C -> M.Map C (S.Set (Either Error C))
run c = runner (S.singleton c) M.empty

    
runner :: S.Set C -> M.Map C (S.Set (Either Error C)) -> M.Map C (S.Set (Either Error C))
runner worklist graph
    | S.null worklist = graph
    | otherwise = 
        let (c, worklist') = S.deleteFindMin worklist in
        let next = S.fromList $ eval c in
        let graph' = M.insert c next graph in
        let worklist'' = S.delete c (gatherRights next `S.union` worklist') in
        runner worklist'' graph'




pullValid :: forall e a b. (Ord e) => (Ord b) => S.Set (Either e a) -> (a -> S.Set (Either e b)) -> S.Set (Either e b)
pullValid rs f = S.unions (extract <%> rs)
    where
        extract :: Either e a -> S.Set (Either e b)
        extract (Left e) = S.singleton (Left e)
        extract (Right v) = f v

eval :: C -> [Either Error C]
eval c@(Left v, env, s, a, t) = do
    case derefK a s of
        Left e -> return $ Left e
        Right ks -> ks >>= \mk -> do
            case mk of
                Left e -> return $ Left e
                Right k -> evalValue c v k
eval c@(Right e, env, s, a, t) = evalExpr c e 

lreturn :: forall a b. a -> [Either b a]
lreturn a = [return a]

-- derefV :: String -> Env -> Store -> Either Error (S.Set (Either Error Value))

-- TODO: rename lol
handle :: forall a b. Either Error [Either Error a] -> (a -> [Either Error b]) -> [Either Error b]
handle (Left e) f  = [Left e]
handle (Right as) f = do
    ma <- as
    case ma of
        Left e -> [Left e]
        Right a -> f a



        

-- TODO: Some refactoring can happen here
evalExpr :: C -> Exp -> [Either Error C]
evalExpr c@(_, p, s, a, t) (Num l i) = putV c $ VI i
evalExpr c@(_, p, s, a, t) (Bool l b) = putV c $ VB b
evalExpr c@(_, p, s, a, t) (Abs l x v) = putV c $ Closure x v
evalExpr c@(_, p, s, a, t) (Var l x) = handle (derefV x p s) (putV c)
evalExpr c@(_, p, s, a, t) (Op2 l o e1 e2) = exprHandler c (Op2_a o e2 p a) e1
evalExpr c@(_, p, s, a, t) (If l e0 e1 e2) = exprHandler c (IfK e1 e2 p a) e0
evalExpr c@(_, p, s, a, t) (App l e0 e1) = exprHandler c (Ar e1 p a) e0
evalExpr c@(_, p, s, a, t) (Rec l name body) = exprHandler c (RecBind name a) body
    
exprHandler c@(_,p,_,_,_) k e = do
    (a', s') <- allocK k c
    t' <- tick c
    lreturn (Right e, p, s', a', t')


putV :: C -> Value -> [Either Error C]
putV c@(_, p, s, a, t) v = do
    t' <- tick c
    return $ Right (Left v, p, s, a, t')

evalValue :: C -> Value -> Kont -> [Either Error C]
evalValue c v Mt  = lreturn c
evalValue c v (Ar e p' a')  = evalAr c v e p' a'
evalValue c v (Fn cls p' a) = evalFn c v cls p' a
evalValue c v (IfK e0 e1 p a) = evalIf c v e0 e1 p a
evalValue c v (Op2_a o e p a) = evalOp2A c v o e p a
evalValue c v (Op2_b o n p a) = evalOp2B c v o n p a
evalValue c v (RecBind n a) = evalRecBind c v n a


evalAr :: C -> Value -> Exp -> Env -> Addr -> [Either Error C]
evalAr c@(_,p,s,_,t) (Closure x body) e p' a = do
    let k = Fn (x, body) p a
    (a',s') <- allocK k c
    t' <- tick c
    lreturn (Right e, p', s', a', t')
evalAr _ v _ _ _ = lTypeError Function v

evalFn :: C -> Value -> (String,Exp) -> Env -> Addr -> [Either Error C]
evalFn c@(_,_,s,_,_) v (x,body) p a = do
    (p',s') <- allocV x v p c
    t' <- tick c
    lreturn (Right body, p', s', a, t')

evalIf :: C -> Value -> Exp -> Exp -> Env -> Addr -> [Either Error C]
evalIf c@(_,_,s,_,_) (VB b) e_true e_false p a = do 
    t' <- tick c
    lreturn (Right $ if b then e_true else e_false, p, s, a, t')
evalIf c v e_true e_false p a = lTypeError Boolean v

evalOp2A :: C -> Value -> Op -> Exp -> Env -> Addr -> [Either Error C]
evalOp2A c@(_,_,s,_,_) (VI n) o e p a = do 
    let k = Op2_b o n p a
    (a',s') <- allocK k c
    t' <- tick c
    lreturn (Right e, p, s', a', t')
evalOp2A c@(_,_,s,_,_) v o e p a = lTypeError Number v

evalOp2B :: C -> Value -> Op -> Int -> Env -> Addr -> [Either Error C]
evalOp2B c@(_,_,s,_,_) (VI n') o n p a = do
    r <- evalOp o n n'
    case r of
        Left e -> [Left e]
        Right r' -> do
            let v = Left r'
            t' <- tick c
            lreturn (v, p, s, a, t')
evalOp2B c v o n p a = lTypeError Number v


evalOp :: Op -> Int -> Int -> [Either Error Value]
evalOp Add = injectF (+) VI
evalOp Sub = injectF (-) VI
evalOp Mult = injectF (*) VI
evalOp Eq = injectF (==) VB
evalOp Div = \x y -> return $ if y == 0 then Left DivByZero else Right $ VI $ x `div` y

evalRecBind :: C -> Value -> String -> Addr -> [Either Error C]
evalRecBind c@(_,p,s,_,t) v name a = do
    (p',s') <- allocV name v p c 
    t' <- tick c
    lreturn (Left v,p',s',a, t')

injectF :: forall a. (Int -> Int -> a) -> (a -> Value) -> Int -> Int -> [Either Error Value]
injectF f pack x y = [Right $ pack  $ f x y]
