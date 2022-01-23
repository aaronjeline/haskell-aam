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

newtype Addr = Addr Integer
    deriving (Eq, Ord)

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

derefV :: String -> Env -> Store -> Either Error (S.Set (Either Error Value))
derefV x (Env p) (Store s) = 
    case (`M.lookup` s)  <$> M.lookup x p of
    Nothing -> Left $ UnboundVar x
    Just Nothing -> Left IllegalAddress
    Just (Just eOrVs) -> Right $ checkV <%> eOrVs
    where
        checkV (Left v) = Right v
        checkV (Right _) = Left NonValue


derefK :: Addr -> Store -> Either Error (S.Set (Either Error Kont))
derefK a (Store s) = 
    case M.lookup a s of
        Nothing -> Left IllegalAddress
        Just eOrVs -> Right $ checkK <%> eOrVs
        where
            checkK (Left v) = Left $ NonContinuation a
            checkK (Right k) = Right k

alloc :: C -> Addr
alloc (_,_,_,_,T t) = Addr t

update :: Addr -> Either Value Kont -> Store -> Store
update k v (Store s) = Store s'
    where
        s' = M.alter f k s
        set = S.singleton v
        f Nothing = Just set
        f (Just s) = Just $ s `S.union` set


allocK :: Kont -> C -> (Addr, Store)
allocK k c@(_,_,store,_,_) = (a, update a (Right k) store)
    where
        a = alloc c

allocV :: String -> Value -> Env -> C -> (Env, Store)
allocV x v (Env p) c@(_,_,store,_,_) = (Env p', s')
    where
        p' = M.insert x a p
        s' = update a (Left v) store
        a = alloc c

newtype Time = T Integer
    deriving (Show, Eq, Ord)

tick :: C -> Time
tick (_,_,_,_,T i) = T $ i + 1


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

lTypeError :: forall a. Type -> Value -> S.Set (Either Error a)
lTypeError t v = S.singleton $ typeError t v

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

tInit = T 1

run :: C -> M.Map C (S.Set (Either Error C))
run c = runner (S.singleton c) M.empty

    
runner :: S.Set C -> M.Map C (S.Set (Either Error C)) -> M.Map C (S.Set (Either Error C))
runner worklist graph
    | S.null worklist = graph
    | otherwise = 
        let (c, worklist') = S.deleteFindMin worklist in
        let next = eval c in
        let graph' = M.insert c next graph in
        let worklist'' = S.delete c (gatherRights next `S.union` worklist') in
        runner worklist'' graph'




pullValid :: forall e a b. (Ord e) => (Ord b) => S.Set (Either e a) -> (a -> S.Set (Either e b)) -> S.Set (Either e b)
pullValid rs f = S.unions (extract <%> rs)
    where
        extract :: Either e a -> S.Set (Either e b)
        extract (Left e) = S.singleton (Left e)
        extract (Right v) = f v

eval :: C -> S.Set (Either Error C)
eval c@(Left v, env, s, a, t) =
    case derefK a s of
        Left e -> S.singleton (Left e)
        Right rs -> pullValid rs (S.singleton . evalValue c v)
eval c@(Right e, env, s, a, t) = evalExpr c e 

lreturn :: forall a b. a -> S.Set (Either b a)
lreturn a = S.singleton $ return a

-- derefV :: String -> Env -> Store -> Either Error (S.Set (Either Error Value))

-- TODO: rename lol
handle :: forall a b. (Ord b) => Either Error (S.Set (Either Error a)) -> (a -> b) -> S.Set (Either Error b)
handle (Left e) f  = S.singleton $ Left e
handle (Right es) f = g <%> es
    where
        g (Left e) = Left e
        g (Right v) = Right $ f v
        

evalExpr :: C -> Exp -> S.Set (Either Error C)
evalExpr c@(_, p, s, a, t) (Num i) = lreturn $ putV c $ VI i
evalExpr c@(_, p, s, a, t) (Bool b) = lreturn $ putV c $ VB b
evalExpr c@(_, p, s, a, t) (x :-> v) = lreturn $ putV c $ Closure x v
evalExpr c@(_, p, s, a, t) (Var x) = handle (derefV x p s) (putV c)
evalExpr c@(_, p, s, a, t) (Op2 o e1 e2) = do
    let k = Op2_a o e2 p a
    let (a',s') = allocK k c
    lreturn (Right e1, p, s', a', tick c)
evalExpr c@(_, p, s, a, t) (If e0 e1 e2) = do
    let k = IfK e1 e2 p a
    let (a', s') = allocK k c
    lreturn (Right e0, p, s', a', tick c)
evalExpr c@(_, p, s, a, t) (e0 :@ e1) = do
    let k = Ar e1 p a
    let (a',s') = allocK k c
    lreturn (Right e0, p, s', a', tick c)
evalExpr c@(_, p, s, a, t) (Rec name body) = do
    let k = RecBind name a
    let (a',s') = allocK k c
    lreturn (Right body, p, s', a', tick c)
    

putV :: C -> Value -> C
putV c@(_, p, s, a, t) v = (Left v, p, s, a, tick c)

evalValue :: C -> Value -> Kont -> Either Error C
evalValue c v Mt  = return c
evalValue c v (Ar e p' a')  = evalAr c v e p' a'
evalValue c v (Fn cls p' a) = evalFn c v cls p' a
evalValue c v (IfK e0 e1 p a) = evalIf c v e0 e1 p a
evalValue c v (Op2_a o e p a) = evalOp2A c v o e p a
evalValue c v (Op2_b o n p a) = evalOp2B c v o n p a
evalValue c v (RecBind n a) = evalRecBind c v n a

evalAr :: C -> Value -> Exp -> Env -> Addr -> Either Error C 
evalAr c@(_,p,s,_,t) (Closure x body) e p' a = do
    let k = Fn (x, body) p a
    let (a',s') = allocK k c
    return (Right e, p', s', a', tick c)
evalAr _ v _ _ _ = typeError Function v

evalFn :: C -> Value -> (String,Exp) -> Env -> Addr -> Either Error C
evalFn c@(_,_,s,_,t) v (x,body) p a = do
    let (p',s') = allocV x v p c
    return (Right body, p', s', a, tick c)

evalIf :: C -> Value -> Exp -> Exp -> Env -> Addr -> Either Error C
evalIf c@(_,_,s,_,_) (VB b) e_true e_false p a = do 
    return (Right $ if b then e_true else e_false, p, s, a, tick c)
evalIf c v e_true e_false p a = typeError Boolean v

evalOp2A :: C -> Value -> Op -> Exp -> Env -> Addr -> Either Error C
evalOp2A c@(_,_,s,_,_) (VI n) o e p a = do 
    let k = Op2_b o n p a
    let (a',s') = allocK k c
    return (Right e, p, s', a', tick c)
evalOp2A c@(_,_,s,_,_) v o e p a = typeError Number v

evalOp2B :: C -> Value -> Op -> Int -> Env -> Addr -> Either Error C
evalOp2B c@(_,_,s,_,_) (VI n') o n p a = do
    r <- evalOp o n n'
    let v = Left r
    return (v, p, s, a, tick c)
evalOp2B c v o n p a = typeError Number v


evalOp :: Op -> Int -> Int -> Either Error Value 
evalOp Add = injectF (+) VI
evalOp Sub = injectF (-) VI
evalOp Mult = injectF (*) VI
evalOp Eq = injectF (==) VB
evalOp Div = \x y -> if y == 0 then Left DivByZero else Right $ VI $ x `div` y

evalRecBind :: C -> Value -> String -> Addr -> Either Error C
evalRecBind c@(_,p,s,_,t) v name a = do
    let (p',s') = allocV name v p c 
    return (Left v,p',s',a, tick c)

injectF :: forall a. (Int -> Int -> a) -> (a -> Value) -> Int -> Int -> Either Error Value
injectF f pack x y = Right $ pack  $ f x y
