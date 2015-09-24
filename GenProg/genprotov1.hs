{-Experimentation with GenProg Library to generate the type of expressions we need-}

{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}


import GenProg
import GenProg.GenExpr
import GenProg.GenExpr.Data
import Data.Generics
import Control.Monad
import Control.Monad.Random
import Test.HUnit
import Data.List
import Data.Char
import Data.Fixed

--functions using fold; and functions manipulating string;

triads :: Int -> [(Int, Int, Int)]
triads n = [(x,y,z)|x<-[1..n],y<-[x..n],z<-[y..n],x^2+y^2==z^2]

----------------------------------------------------------------
factors :: Int -> Int
factors n = foldr (+) 0 lists
			where	
					lists = [x|x<-[1..(n-1)], n `mod` x == 0]

----------------------------------------------------------------

multiply :: [Int] -> Int
multiply (x:xs) = foldr (*) x xs

---------------------------------------------------------------

concatenate :: [String] -> String     
concatenate (x:xs) = foldl(++) x xs

----------------------------------------------------------------

concatenateAndUpcaseOddLengthStrings :: [String] -> String
concatenateAndUpcaseOddLengthStrings (x:xs)= map toUpper (concatenate ([x|x<-xs,odd(length x)]))

------------------------------------------------------------------------------------------------

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

----------------------------------------------------------------------------

maxElementOfAList :: Ord a => [a] -> a 
maxElementOfAList xs = foldl1 max xs

--------------------------------------------------------------------

keepInBetween ::Int -> Int -> [Int] -> [Int]
keepInBetween a b xs =filter (>=a) $ filter (<b) xs

--------------------------------------------------------------------

data E = Plus E E
       | Minus E E
       | Times E E
       | Div E E
	   | Sin E
	   | Cos E
	   | Exp E
       | Const Double
       deriving (Typeable,Data,Eq,Show)

eval :: E -> Maybe Double
eval (Const c)     = Just c
eval (Plus e1 e2)  = liftM2 (+) (eval e1) (eval e2)
eval (Minus e1 e2) = liftM2 (-) (eval e1) (eval e2)
eval (Times e1 e2) = liftM2 (*) (eval e1) (eval e2)
eval (Div e1 e2) | ok        = liftM2 (/) x1 x2
                 | otherwise = Nothing
  where (x1,x2) = (eval e1,eval e2)
        ok = x2 /= Just 0 && liftM2 (mod') x1 x2 == Just 0
eval (Sin e1) = liftM (sin) (eval e1)
eval (Cos e1) = liftM (cos) (eval e1)
eval (Exp e1) = liftM (exp) (eval e1)

instance GenProg (Rand StdGen) E where
  terminal    = Const `liftM` getRandomR (1,9)
  nonterminal = do
    r <- getRandomR (0,6)
    [liftM2 Plus terminal terminal,
     liftM2 Minus terminal terminal,
     liftM2 Times terminal terminal,
     liftM2 Div terminal terminal,
	 liftM Sin terminal,
	 liftM Cos terminal,
	 liftM Exp terminal] !! r
	 
myFitness :: Double -> E -> Double
myFitness n e = error + size
  where error = realToFrac $ maybe infinity (abs . (n-)) (eval e)
        size  = (realToFrac $ nodes e) / 100
        infinity = 1/0 --yes

run :: Double
run = sFitness i
      where i = cachedBest $ evalRand (evolve params) g
                where g = mkStdGen 0
                      params = defaultEvolParams {fitness = myFitness 1234}
