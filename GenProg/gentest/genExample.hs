{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}


import GenProg
import GenProg.GenExpr
import GenProg.GenExpr.Data
import Data.Generics
import Control.Monad
import Control.Monad.Random


data E = Plus E E
       | Minus E E
       | Times E E
       | Div E E
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
        ok = x2 /= Just 0
eval (Exp e1)      = liftM (exp) (eval e1)

instance GenProg (Rand StdGen) E where
  terminal    = Const `liftM` getRandomR (1,9)
  nonterminal = do
    r <- getRandomR (0,4)
    [liftM2 Plus terminal terminal,
     liftM2 Minus terminal terminal,
     liftM2 Times terminal terminal,
     liftM2 Div terminal terminal,
     liftM Exp terminal] !! r
	 
myFitness :: Double -> E -> Double
myFitness n e = error + size
  where error = realToFrac $ maybe infinity (abs . (n-)) (eval e)
        size  = (realToFrac $ nodes e) / 100
        infinity = 1/0 -- maxBound returns the greatest int, so a big number should suffice.

-- Runs genprog. Goal -> n; StdGen -> r
run :: Double -> Int -> IO ()
run n r = do
    let params = defaultEvolParams { fitness = myFitness n }
    let g = mkStdGen r
    let trace = evalRand (evolveTrace params {elitists = 1, mProb = 0.05}) g
    print $ map (sFitness . best . pop) trace
    let i = cachedBest $ last trace
    print $ eval $ unInd i
    print $ unInd i
    print $ nodes $ unInd i
