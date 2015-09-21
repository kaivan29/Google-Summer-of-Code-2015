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
      eval :: t -> t
      getType :: t -> String
      getApplicableTypes :: t -> [String]
	  
data GenExp = forall t . IsGenExp t => GenExp t

data IntExp = IntExp Int deriving (Typeable, Data, Show, Eq)
data FloatExp = FloatExp Float
data ListExp = ListExp [GenExp]
data ArithExp = Add GenExp GenExp | Sub GenExp GenExp | Mul GenExp GenExp
data CompExp = Eq GenExp GenExp | Lt GenExp GenExp | Gt GenExp GenExp

instance Random IntExp
instance Num IntExp

instance Random FloatExp
instance Num FloatExp

instance Random ListExp
instance Num ListExp

--I have just generated some random numbers. Do we want something specific?

instance IsGenExp IntExp where
   randomGen = getRandomR (0,50)
   eval (IntExp num) = IntExp num --this seems about right?

instance IsGenExp FloatExp where
   randomGen = getRandomR (0,50)
   eval (FloatExp num) = FloatExp num
   
instance IsGenExp ListExp where
   randomGen = getRandomR (0,50)
   --eval what should ListExp evaluate to?


{-
data SBox = forall a. Show a => SBox a

boxes :: [SBox]
boxes = [SBox (), SBox 2, SBox "foo"]

showBox :: SBox -> String
showBox (SBox a) = show a

main :: IO ()
main = mapM_ (putStrLn . showBox) boxes

data Box = forall a. Box a (a -> a) (a -> String)

boxa :: Box
boxa = Box 1 negate show

boxb :: Box
boxb = Box "foo" reverse show 
-- ∃ t. Show t => t

 newtype SBox a = SBox a; elem :: (Eq a) => a -> SBox a -> Bool; elem x (SBox y) = x == y

Random instance of Int 
 https://hackage.haskell.org/package/random-1.0.0.2/docs/src/System-Random.html

Minimal complete definition: randomR and random." -- So you'll have to define a Random instance for your type, and provide definitions for random and randomR in the definition.
Your IntExp is just a wrapper around Int, so all you have to do is use the Random instance for Int and wrap the results in your IntExp constructor. 
-}
