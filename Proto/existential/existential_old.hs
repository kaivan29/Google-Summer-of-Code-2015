{- Trying to understand how existential works-}

-- The following language extensions need to be enabled:
-- DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses

{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, ExistentialQuantification , GADTs #-}

import GenProg
import GenProg.GenExpr
import GenProg.GenExpr.Data
import Data.Generics
import Control.Monad
import Control.Monad.Random
import Data.Fixed

class IsGenExp t where
      mutate :: (MonadRandom m) => t -> m t
      crossOver :: (MonadRandom m) => t -> t -> m t
      randomGen :: (MonadRandom m) => m t
	  
data GenExp = forall t . IsGenExp t => GenExp t

data Expr a where
    I :: Int -> Expr Int    --Our integer constants and 'a' is 
    B :: Bool -> Expr Bool   --Boolean constants
    Plus :: Expr Int -> Expr Int -> Expr Int
    Times :: Expr Int -> Expr Int -> Expr Int
    Eq :: Expr Int -> Expr Int -> Expr Bool

deriving instance Typeable (Expr a)
--Expr = forall a . Data a => Expr a
--deriving instance Data a => Data (Expr a)
deriving instance Eq (Expr a)
deriving instance Show (Expr a)
	   
eval :: Expr a -> a --Maybe (Either Int Bool)
eval (I n) = n
eval (B b) = b
eval (Plus e1 e2) = eval e1 + eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Eq e1 e2) = eval e1 == eval e2

instance GenProg (Rand StdGen) (Expr a) where
  terminal    = I `liftM` getRandomR (1,9)
  nonterminal = do
    r <- getRandomR (0,2)
    [liftM2 Plus terminal terminal,
     liftM2 Times terminal terminal,
	 liftM2 Eq terminal terminal] !! r


{-
Conclusion: GADT's are a better approach than Existential.
Also, I learned that while working with existential, it is 
not the best option and will not be really useful many times because once we generalize and wrap a data type in class, we can not get this type back.
-}