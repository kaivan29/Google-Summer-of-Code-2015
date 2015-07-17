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

{- List Operation Ideas
Plus e1 e2 = e1 ++ e2
Reverse e1 = reverse e1
ShiftL (x:xs) = xs++[x]
Union e1 e2 = union e1 e2
Sort e = sort e
Length e1 = length e1
Maximum e1 = maximum e1
Minimum e1 = minimum e1
Product e = product e
-}

r = 0 :: Int

data E = Plus E E
       | Head E
       | Reverse E
       | Union E E
       | Sort E
       | Length E
       | Maximum E
       | Minimum E
       | Product E
       | Less E E
       | Greater E E
       | LessOrEqual E E
       | GreaterOrEqual E E
       | Const [Int]
       deriving (Typeable,Data,Eq,Show)

eval :: E -> Maybe [Int]
eval (Const c)     = Just c
eval (Plus e1 e2)  = liftM2 (++) (eval e1) (eval e2)
eval (Reverse e)   = liftM (reverse) (eval e)
eval (Union e1 e2) = liftM2 (union) (eval e1) (eval e2)
eval (Sort e) = liftM (sort) (eval e)
eval (Head e)      = do xs <- eval e
                        case xs of
                            x:_ -> return [x]
                            [ ] -> Nothing

--takes a list as input
eval2 :: E -> Maybe Int
eval2 (Length e)  = liftM (length) (eval e)
eval2 (Product e) = liftM (product) (eval e)
eval2 (Maximum e) = liftM (maximum) (eval e)
eval2 (Minimum e) = liftM (minimum) (eval e)

evalB :: E -> Maybe Bool
evalB _ = Nothing
evalB (Less e1 e2) = liftM2 (<) (eval e1) (eval e2)
evalB (Greater e1 e2) = liftM2 (>) (eval e1) (eval e2)
evalB (LessOrEqual e1 e2) = liftM2 (<=) (eval e1) (eval e2)
evalB (GreaterOrEqual e1 e2) = liftM2 (>=) (eval e1) (eval e2)

instance GenProg (Rand StdGen) E where
  terminal    = liftM (Const . (:[])) $ getRandomR (0,9)
  nonterminal = do
    r <- getRandomR (0,8)
    [liftM2 Plus terminal terminal,
     liftM Reverse terminal,
     liftM Head terminal,
     liftM2 Union terminal terminal,
     liftM Sort terminal,
     liftM2 Less terminal terminal,
     liftM2 Greater terminal terminal,
     liftM2 LessOrEqual terminal terminal,
     liftM2 GreaterOrEqual terminal terminal] !! r


myFitness :: [Int] -> E -> Double
myFitness n e = error + size
  where error = realToFrac $ maybe maxBound (diff2 n) (eval e)
        size  = (realToFrac $ nodes e) / 100
        diff1 a b = sum $ map abs [(length a - length b), sum $ zipWith (-) a b]
        diff2 a b = if length a > length b then d a b else d b a
                where d x1 x2 = fst $ foldl (\(acc,i) x -> (acc + (maybe l (abs . (i-)) (elemIndex x x2)), i+1) ) (0,0) x1
                        where l = length x1

myFitB :: Bool -> E -> Double
myFitB n e = error + size
  where error = 0
        size = 1

{- diff2:
    a,b = []
    diff = 0
    for i in a:
     if i in b:
      diff += a.index(i) - a.index(b)
     else:
      diff += max(len(a),len(b))
-}

-- Runs genprog. Goal -> n; StdGen -> r; Mutation -> m
run :: [Int] -> Int -> Double -> IO ()
run n l m = do
    let params = defaultEvolParams { fitness = myFitness n }
    let g = mkStdGen l
    let trace = evalRand (evolveTrace params {elitists = 1, mProb = m}) g
    print $ map (sFitness . best . pop) trace
    let i = cachedBest $ last trace
    print $ eval $ unInd i
    print $ unInd i
    print $ nodes $ unInd i
