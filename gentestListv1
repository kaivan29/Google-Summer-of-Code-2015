{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

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
Head e1 = head e1
Tail e1 = tail e1
Last e = last e
Init e = init e
Union e1 e2 = union e1 e2
Sort e = sort e
Length e1 = length e1
Maximum e1 = maximum e1
Minimum e1 = minimum e1
Product e = product e

Should we pack length, maximum, minimum, product in singleton List?
Also, I changed Head such that it takes care of an empty List. Let me know if it looks right.

Also, I did not understand what you were trying to do with shiftL? Were you trying to select a pivot and shift the elements around it? Like [1,2,3,4] shiftL around 2 so that would be [3,4,2,1]?
ShiftL seems bad... swap?
And if it is swap? How would you want it to work? If you could give an example?
-}

data E = Plus E E
       | Reverse E
       | Head E
       | Tail E
       | Last E
       | Init E
       | Union E E
       | Sort E
       | Length E
       | Maximum E
       | Minimum E
       | Product E
--     | ShiftL E
       | Const [Int]
       deriving (Typeable,Data,Eq,Show)

eval :: E -> Maybe [Int]
eval (Const c)     = Just c
eval (Plus e1 e2)  = liftM2 (++) (eval e1) (eval e2)
eval (Reverse e)   = liftM (reverse) (eval e)
--eval (Head e)      = liftM ((:[]) . head) (eval e)
eval (Head e)      = do xs <- eval e
                        case xs of
                            x:_ -> return [x]
                            [ ] -> Nothing							
eval (Tail e) | ok        = t
              | otherwise = Nothing
    where t = liftM (tail) (eval e)
          ok = t /= Just []
eval (Last e)      = liftM ((:[]) . last) (eval e)
eval (Init e)      = liftM (init) (eval e)
--eval (Length e)    = liftM ((:[]) . length) (eval e)
--eval (Product e)   = liftM ((:[]) . product) (eval e)
--eval (ShiftL e)    = liftM (shiftL) (eval e)
--    where shiftL (x:xs) = xs ++ [x]
eval (Union e1 e2) = liftM2 (union) (eval e1) (eval e2)
eval (Sort e) = liftM (sort) (eval e)

--takes a list as input
eval2 :: E -> Maybe Int
eval2 (Length e)  = liftM (length) (eval e)
eval2 (Product e) = liftM (product) (eval e)
eval2 (Maximum e) = liftM (maximum) (eval e)
eval2 (Minimum e) = liftM (minimum) (eval e)


instance GenProg (Rand StdGen) E where
  terminal    = liftM (Const . (:[])) $ getRandomR (0,9)
  nonterminal = do
    r <- getRandomR (0,7)
    [liftM2 Plus terminal terminal,
     liftM Reverse terminal,
     liftM Head terminal,
     liftM Tail terminal,
	 liftM Last terminal,
	 liftM Init terminal,
     liftM2 Union terminal terminal,
	 liftM Sort terminal] !! r

myFitness :: [Int] -> E -> Double
myFitness n e = error + size
  where error = realToFrac $ maybe maxBound (diff2 n) (eval e)
        size  = (realToFrac $ nodes e) / 100
        diff1 a b = sum $ map abs [(length a - length b), sum $ zipWith (-) a b]
        diff2 a b = if length a > length b then d a b else d b a
                where d x1 x2 = fst $ foldl (\(acc,i) x -> (acc + (maybe l (abs . (i-)) (elemIndex x x2)), i+1) ) (0,0) x1
                        where l = length x1

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
run n r m = do
    let params = defaultEvolParams { fitness = myFitness n }
    let g = mkStdGen r
    let trace = evalRand (evolveTrace params {elitists = 1, mProb = m}) g
    print $ map (sFitness . best . pop) trace
    let i = cachedBest $ last trace
    print $ eval $ unInd i
    print $ unInd i
    print $ nodes $ unInd i
