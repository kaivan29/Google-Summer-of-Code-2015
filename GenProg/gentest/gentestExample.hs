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

data G = Arith Char G G
       | List Char G
	   | Comp Char G G
	   | Logic Char G G
	   | Cons [Int]
	   deriving (Typeable,Data,Eq,Show)
	   
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

evalc :: G -> Maybe Bool
evalc (Comp c e1 e2)
 |c == 'l' = liftM2 (<) (evala e1) (evala e2)
 |c == 'g' = liftM2 (>) (evala e1) (evala e2)
 |c == 'k' = liftM2 (<=) (evala e1) (evala e2)
 |c == 'f' = liftM2 (>=) (evala e1) (evala e2)
 |otherwise = Nothing
evalc (Logic c e1 e2)
 |c == 'e' = liftM2 (==) (evala e1) (evala e2)
 |c == 'n' = liftM2 (/=) (evala e1) (evala e2)
 |otherwise = Nothing
 
instance GenProg (Rand StdGen) G where
  terminal    = liftM (Cons . (:[])) $ getRandomR (0,9)
  nonterminal = do
   r <- getRandomR (0, 1)
   [liftM3 Arith (fromList [('p',1.2),('u',1.3)]) terminal terminal,
    liftM2 List (fromList [('r',1.2),('s',1.3)]) terminal] !! r
	
myFitness :: [Int] -> G -> Double
myFitness n e 
 |isArith e || isList e = error + size --how do I define isInt in where such that it would pattern match with (Arith c e1 e2) and (List c e) I am unable to figure out the syntax
 |isLogic e || isComp e = err + siz --For Comp and Logic
 |otherwise = infinity
   where infinity = 1/0
         err = 0
         siz = 0 --I dont think we are going to need Comp and Logic for List. But if we do I'll create some fitness function
         error = realToFrac $ maybe maxBound (diff2 n) (evala e)
         size  = (realToFrac $ nodes e) / 100
         diff1 a b = sum $ map abs [(length a - length b), sum $ zipWith (-) a b]
         diff2 a b = if length a > length b then d a b else d b a
             where d x1 x2 = fst $ foldl (\(acc,i) x -> (acc + (maybe l (abs . (i-)) (elemIndex x x2)), i+1) ) (0,0) x1
                     where l = length x1

--helper functions
isLogic (Logic c _ _)
 | c == 'l' || c == 'g' || c == 'k' || c == 'f' = True
isLogic _ = False

isComp (Comp c _ _)
 | c == 'e' || c == 'n' = True
isComp _ = False

isArith (Arith c _ _)
 | c == 'p' || c == 'u' = True
isArith _ = False

isList (List c _)
 | c == 'r' || c == 's' = True
isList _ = False

boolToInt :: Bool -> Double --converting bool To Int and then to [Int] just in case
boolToInt b
 | b == True = 1
 | b == False = 0

-----------------------
		
run :: [Int] -> Int -> Double -> IO ()
run n l m = do
    let params = defaultEvolParams { fitness = myFitness n }
    let g = mkStdGen l
    let trace = evalRand (evolveTrace params {elitists = 1, mProb = m}) g
    print $ map (sFitness . best . pop) trace
    let i = cachedBest $ last trace
    print $ evala $ unInd i
    print $ unInd i
    print $ nodes $ unInd i
