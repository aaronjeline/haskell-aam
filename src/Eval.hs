{-# Language ScopedTypeVariables #-}
module Eval where

import Core
import Util
import qualified Data.Map as M
import Data.List
import Data.Functor
import Data.Bifunctor

newtype Config = C C

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

instance Show Env where
    show (Env e) = show $ M.toList e

newtype Store = Store (M.Map Addr (Either Value Kont))

instance Show Store where
    show (Store s) = "[" ++ internal ++ "]"
        where
            internal = intercalate "," $ showEntry <$> lst
            showEntry (a, e) = show a ++ " -> " ++ showEither e
            lst = M.toList s

derefV :: String -> Env -> Store -> Either Error Value
derefV x (Env p) (Store s) = case (`M.lookup` s)  <$> M.lookup x p of
    Nothing -> Left $ UnboundVar x
    Just Nothing -> Left IllegalAddress
    Just (Just (Right _)) -> Left NonValue
    Just (Just (Left v)) -> return v
    

derefK :: Addr -> Store -> Either Error Kont
derefK a (Store s) = 
    case M.lookup a s of
        Nothing -> Left IllegalAddress
        Just (Left _) -> Left $ NonContinuation a
        Just (Right k) -> Right k

alloc :: C -> Addr
alloc (_,_,_,_,T t) = Addr t

allocK :: Kont -> C -> (Addr, Store)
allocK k c@(_,_,Store s,_,_) = (a, Store $ M.insert a (Right k) s)
    where
        a = alloc c

allocV :: String -> Value -> Env -> C -> (Env, Store)
allocV x v (Env p) c@(_,_,Store s,_,_) = (Env p', Store s')
    where
        p' = M.insert x a p
        s' = M.insert a (Left v) s
        a = alloc c

newtype Time = T Integer
    deriving Show

tick :: C -> Time
tick (_,_,_,_,T i) = T $ i + 1


data Value 
    = VI Int
    | VB Bool
    | Closure String Exp 
    deriving Show

typeof :: Value -> Type
typeof VI {} = Number
typeof VB {} = Boolean
typeof Closure {} = Function

typeError :: Type -> Value -> Either Error a
typeError t v = Left $ TypeError (t, typeof v)

data Type
    = Number
    | Boolean
    | Function
    deriving Show

data Kont
    = Ar Exp Env Addr
    | Mt
    | Fn (String, Exp) Env Addr
    | IfK Exp Exp Env Addr
    | Op2_a Op Exp Env Addr
    | Op2_b Op Int Env Addr
    | RecBind String Addr
    deriving Show

data Error
    = UnboundVar String
    | NonValue
    | IllegalAddress 
    | NonContinuation Addr
    | DivByZero
    | TypeError (Type, Type)
    deriving Show

inject :: Exp -> C
inject e = (Right e, Env M.empty, s, Addr 0, tInit)
    where
        s = Store $ M.insert (Addr 0) (Right Mt) M.empty

tInit = T 1

run :: C -> Maybe Time -> ([C], Maybe Error)
run c t = do
    case eval c of
        Left e -> ([c], Just e)
        Right c' -> case done c' t of
            Left e -> ([c,c'], Just e)
            Right True -> ([c,c'], Nothing)
            Right False -> 
                let (cs, me) = run c' t in 
                (c:cs, me)
    
    

done :: C -> Maybe Time -> Either Error Bool
done (Left v, _, s, a, t) m = do 
    k <- derefK a s
    return $ isMt k || hitBound t m
done (_,_,_,_,t) m = return $ hitBound t m

hitBound :: Time -> Maybe Time -> Bool
hitBound t Nothing = False
hitBound (T t) (Just (T t')) = t == t'

isMt :: Kont -> Bool
isMt Mt = True
isMt _ = False

eval :: C -> Either Error C
eval c@(Left v, env, s, a, t) = derefK a s >>= evalValue c v 
eval c@(Right e, env, s, a, t) = evalExpr c e 

evalExpr :: C -> Exp -> Either Error C
evalExpr c@(_, p, s, a, t) (Num i) = return $ putV c $ VI i
evalExpr c@(_, p, s, a, t) (Bool b) = return $ putV c $ VB b
evalExpr c@(_, p, s, a, t) (x :-> v) = return $ putV c $ Closure x v
evalExpr c@(_, p, s, a, t) (Var x) = putV c <$> derefV x p s 
evalExpr c@(_, p, s, a, t) (Op2 o e1 e2) = do
    let k = Op2_a o e2 p a
    let (a',s') = allocK k c
    return (Right e1, p, s', a', tick c)
evalExpr c@(_, p, s, a, t) (If e0 e1 e2) = do
    let k = IfK e1 e2 p a
    let (a', s') = allocK k c
    return (Right e0, p, s', a', tick c)
evalExpr c@(_, p, s, a, t) (e0 :@ e1) = do
    let k = Ar e1 p a
    let (a',s') = allocK k c
    return (Right e0, p, s', a', tick c)
evalExpr c@(_, p, s, a, t) (Rec name body) = do
    let k = RecBind name a
    let (a',s') = allocK k c
    return (Right body, p, s', a', tick c)
    

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
