{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
-- List permutations

import GenProg
import GenProg.GenExpr
import GenProg.GenExpr.Data
import Data.Generics
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Random.List

-------------------------------------------

data G = Arith Char G G
       | List Char G
	   | Cons [Int]
	   deriving (Typeable,Data,Eq,Show)
	   
data E = Comp Char E E
	   | Logic Char E E
	   | Const [Int]
	   deriving (Typeable,Data,Eq,Show)
	   
---------------------------------------------
	   
evala :: G -> Maybe [Int]
evala (Cons c)    = Just c
evala (Arith c e1 e2)
 |c == 'p' = liftM2 (++) (evala e1) (evala e2)
 |c == 'u' = liftM2 (union) (evala e1) (evala e2)
 |otherwise = Nothing
evala (List c e)
 |c == 'r' = liftM (reverse) (evala e)
 |c == 's' = liftM (sort) (evala e)
 |otherwise = Nothing
 
-----

evalb :: E -> Maybe [Int]
evalb (Const c) = Just c

evalc :: E -> Maybe Bool
evalc (Comp c e1 e2)
 |c == 'l' = liftM2 (<) (evalb e1) (evalb e2)
 |c == 'g' = liftM2 (>) (evalb e1) (evalb e2)
 |c == 'k' = liftM2 (<=) (evalb e1) (evalb e2)
 |c == 'f' = liftM2 (>=) (evalb e1) (evalb e2)
 |otherwise = Nothing
evalc (Logic c e1 e2)
 |c == 'e' = liftM2 (==) (evalb e1) (evalb e2)
 |c == 'n' = liftM2 (/=) (evalb e1) (evalb e2)
 |otherwise = Nothing
 
--------------------------------------------------------------
 
instance GenProg (Rand StdGen) G where
  terminal    = liftM (Cons . (:[])) $ getRandomR (0,9)
  nonterminal = do
   r <- getRandomR (0, 1)
   [liftM3 Arith (fromList [('p',1.2),('u',1.3)]) terminal terminal,
    liftM2 List (fromList [('r',1.2),('s',1.3)]) terminal] !! r
	
----
	
instance GenProg (Rand StdGen) E where
  terminal    = liftM (Const . (:[])) $ getRandomR (0,9)
  nonterminal = do
   r <- getRandomR (0, 1)
   [liftM3 Comp (getRandomR ('a','z')) terminal terminal,
    liftM3 Logic (getRandomR ('a','z')) terminal terminal] !! r
	
-----------------------------------------------------------------
	
myFitness :: [Int] -> G -> Double
myFitness n e 
 |isArith e || isList e = error + size --how do I define isInt in where such that it would pattern match with (Arith c e1 e2) and (List c e) I am unable to figure out the syntax
-- |isLogic e || isComp e = err + siz --For Comp and Logic
 |otherwise = infinity
   where infinity = 1/0
         error = realToFrac $ maybe maxBound (diff2 n) (evala e)
         size  = (realToFrac $ nodes e) / 100
         diff1 a b = sum $ map abs [(length a - length b), sum $ zipWith (-) a b]
         diff2 a b = if length a > length b then d a b else d b a
             where d x1 x2 = fst $ foldl (\(acc,i) x -> (acc + (maybe l (abs . (i-)) (elemIndex x x2)), i+1) ) (0,0) x1
                     where l = length x1

--Fitness function needed for Logical operators
					 
--helper functions
isLogic (Logic _ _ _) = True
isLogic _ = False

isComp (Comp _ _ _) = True
isComp _ = False

isArith (Arith _ _ _) = True
isArith _ = False

isList (List _ _) = True
isList _ = False

boolToInt :: Bool -> Double --converting bool To Int and then to [Int] just in case
boolToInt b
 | b == True = 1
 | b == False = 0

-----------------------
		
run :: [Int] -> Int -> Double -> IO ()
run n r m = do
    let params = defaultEvolParams { fitness = myFitness n }
    let g = mkStdGen r
    let trace = evalRand (evolveTrace params {elitists = 1, mProb = m}) g
    print $ map (sFitness . best . pop) trace
    let i = cachedBest $ last trace
    print $ evala $ unInd i
    print $ unInd i
    print $ nodes $ unInd i
