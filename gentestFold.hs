 {-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

-- Getting fold to work in any super-duper crude way
-- Got it working with f(a,b), and then adding, multiplying, or subtracting a random constant to f(a,b).
-- Not much flexibility, but it evolves the functions to run fold on.
-- It seems to work okay for some simple functions, and not at all for others.
-- Not sure how good the error function is either.
-- For example, it got foldl (\a b -> (a + b) + 5) 1 [1..10] = 5 == 106
-- And it got foldl (\a b -> (a * b) + 5) 1 [1..10] == 34805305
-- Anyway, hope it's food for thought. 

import GenProg
import GenProg.GenExpr
import GenProg.GenExpr.Data
import Data.Generics
import Control.Monad
import Control.Monad.Random


data E = Base Int
       | Plus E E
       | Minus E E
       | Times E E
       | Greater E E
       | Less E E
       | GreaterOrEqual E E
       | LessOrEqual E E
	   | Foldl E E E --here it is
       | Const Int
	   | Cons [Int]
       deriving (Typeable,Data,Eq,Show)
	   
	   
evala :: E -> [Int]
evala (Cons l) = l
evala _ = []
	   
--foldl included
evalb :: E -> Int
evalb (Foldl e n l) = foldl (eval e) (evalc n) (evala l)
evalb _ = maxBound

--I know evalb and evalc are same. Once it works, will merge them.
evalc :: E -> Int
evalc (Const n) = n
evalc _ = maxBound

eval :: E -> (Int -> Int -> Int)
eval (Base c) 
    | c == 1 = (+)
    | c == 2 = (-)
    | c == 3 = (*)
eval (Plus n e)  = apply (+) (evalc n) (eval e)
eval (Minus n e) = apply (-) (evalc n) (eval e)
eval (Times n e) = apply (*) (evalc n) (eval e)
eval (Greater n e) = apply1 (>) (evalc n) (eval e)
eval (Less n e) = apply1 (<) (evalc n) (eval e)
eval (GreaterOrEqual n e) = apply1 (>=) (evalc n) (eval e)
eval (LessOrEqual n e) = apply1 (<=) (evalc n) (eval e)


apply :: (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> (Int -> Int -> Int)
apply p n q = (\x y -> p n (q x y))

--Using greater in if else. Example (if 2 '+/-/*' x > y then x '+/-/*' y else x)
apply1 :: (Int -> Int -> Bool) -> Int -> (Int -> Int -> Int) -> (Int -> Int -> Int)
apply1 p n q = (\x y -> if (p (q n x) y) then (q x y) else x)

-- foldl (\a b -> if 2*b > a then a + b else a) 1 [1..10]
-- foldl (\a b -> (a * b) + 9) 1 [1..10]
-- Plus(Const 9) (Base 3)

instance GenProg (Rand StdGen) E where
  terminal    = liftM Base $ getRandomR (1,3)
  nonterminal = do
    let num = liftM Const $ getRandomR (1,9)
    let options = [liftM2 Plus num terminal,
                   liftM2 Minus num terminal,
                   liftM2 Times num terminal]
    r <- getRandomR (0,length options - 1)
    options !! r
	 
myFitness :: (Int,Double) -> E -> Double
myFitness (n,n') e = error + size
  where error = realToFrac $ (abs . (n-)) (if fval < -(10^6) then maxBound else fval) 
        size  = n' * (realToFrac $ nodes e) / 100
        fval = foldeval e

foldeval :: E -> Int
foldeval e = foldl fn 5 [1..3]
    where fn = eval e
	
--Sample run  foldevol (Foldl (Plus (Const 9)(Base 1)) (Const 2) (Cons [1,2,3])).Works!!	
foldevol :: E -> Int
foldevol e = evalb e

-- Runs genprog. (Goal, size-weight) -> (n,n'); StdGen -> r; mutation rate -> m
run :: (Int,Double) -> Int -> Double ->  IO ()
run n r m = do
    let params = defaultEvolParams { fitness = myFitness n }
    let g = mkStdGen r
    let trace = evalRand (evolveTrace params {elitists = 1, mProb = m, terminate = tGeneration 5}) g
    print $ map (sFitness . best . pop) trace
    let i = cachedBest $ last trace
    print $ foldeval $ unInd i
    print $ unInd i
    print $ nodes $ unInd i
