 {-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

import Data.Generics

--
data E = Base Int
       | Plus E E
       | Minus E E
       | Times E E
       | Div E E
       | Foldl E E
       | Gt E E
       | Lt E E
       | Eqq E E
       | If E E E
       | Const Int
       | Var Int
       deriving (Typeable,Data,Eq,Show)

eval :: E -> (Int -> Int -> Int)
eval (Base c) 
    | c == 1 = (+)
    | c == 2 = (-)
    | c == 3 = (*)
eval (Plus n e)  = apply (+) (evalc n) (eval e)
eval (Minus n e) = apply (-) (evalc n) (eval e)
eval (Times n e) = apply (*) (evalc n) (eval e)

evalc :: E -> Int
evalc (Const n) = n
evalc _ = maxBound

apply :: (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> (Int -> Int -> Int)
apply p n q = (\x y -> p n (q x y))

-- literal eval
-- I pass the variable-values through the function,
-- And we call a new chain of evals with new variables if necessary, as in fold.
evalL :: Int -> Int -> E -> Int
evalL v1 v2 x
    | (Const n) <- x     = n
    | (Var 1) <- x       = v1
    | (Var 2) <- x       = v2
    | (Plus e e') <-   x = (+) (ev e) (ev e')
    | (Times e e') <-  x = (*) (ev e) (ev e')
    | (Minus e e') <-  x = (-) (ev e) (ev e')
    | (Div e e') <- x    = if (ev e') == 0 then maxBound else div (ev e) (ev e')
    | (Foldl e e') <- x  = foldl (\a b -> evalL a b e) (ev e') [1..10] -- not sure how to put the list in well... x_x
    | (Gt e e') <- x     = if (ev e) > (ev e') then 1 else 0
    | (Lt e e') <- x     = if (ev e) < (ev e') then 1 else 0
    | (Eqq e e') <- x    = if (ev e) == (ev e') then 1 else 0
    | (If e e' e'') <- x = if (ev e) == 0 then (ev e'') else (ev e')
    where ev = evalL v1 v2

t1 = (Plus (Const 1) (Times (Const 2) (Const 2)))
t2 = (Plus (Const 1) (Times (Const 2) (Var 1)))
t3 = (Foldl (Plus (Var 1) (Var 2)) (Const 0)) 
t4 = (If (Gt (Times (Const 2) (Var 2)) (Var 1)) (Plus (Var 1) (Var 2)) (Var 1) )
t5 = (Foldl t4 (Const 0)) -- foldl (\a b -> if 2 * b > a then a + b else a) 0 [1..10]
