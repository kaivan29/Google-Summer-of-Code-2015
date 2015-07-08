-- The following language extensions need to be enabled:
-- DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses

{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, ExistentialQuantification , GADTs #-}
{-# LANGUAGE RankNTypes #-}

import GenProg
import GenProg.GenExpr
import GenProg.GenExpr.Data
import Data.Generics
import Control.Monad
import Control.Monad.Random
import Data.Fixed
import Data.Typeable
import Data.Data

class IsGenExp t where
      mutate :: (MonadRandom m) => t -> m t
      crossOver :: (MonadRandom m) => t -> t -> m t
      randomGen :: (MonadRandom m) => m t
	  
data GenExp = forall t . IsGenExp t => GenExp t

-- ∃ t. (t, t → t, t → String)

data Fox = forall a b. (Show b, Eq b) => Fox a a (a -> a -> b) (b -> String)

boxc :: Int -> Int -> String -> Fox
boxc n1 n2 x
  | x == "add" = Fox n1 n2 (+) show
  | x == "sub" = Fox n1 n2 (-) show
  | x == "times" = Fox n1 n2 (*) show
  | x == "eq" = Fox n1 n2 (==) show
  | x == "gt" = Fox n1 n2 (>) show
  | x == "lt" = Fox n1 n2 (<) show

fapply :: Fox -> String
fapply (Fox x y f p) = p (f x y)

t1 = fapply (boxc 2 2 "eq")
t2 = fapply (boxc 4 5 "add")
--t2 = fapply (boxc read(fapply (boxc 4 5 "times")) read (fapply(boxc 5 6 "sub")) "add")

--lets try this
data SBox = forall a. Show a => SBox a

boxes :: [SBox]
boxes = [SBox (), SBox 2, SBox "foo"]

showBox :: SBox -> String
showBox (SBox a) = show a

main :: IO ()
main = mapM_ (putStrLn . showBox) boxes

{-
instance GenProg (Rand StdGen) Fox where
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

{- data Box = forall a. Box a (a -> a) (a -> String)

boxa :: Box
boxa = Box 1 negate show

boxb :: Box
boxb = Box "foo" reverse show 
-- ∃ t. Show t => t

-- ()
-- 2
-- "foo"
 newtype SBox a = SBox a; elem :: (Eq a) => a -> SBox a -> Bool; elem x (SBox y) = x == y
-}
-}
