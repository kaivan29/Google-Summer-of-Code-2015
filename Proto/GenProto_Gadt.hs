{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Random
import Control.Monad
import Data.List

data Expr :: * -> * where
    I :: Int -> Expr Int
    B :: Bool -> Expr Bool
    L :: [Expr a] -> Expr [a]
    Plus :: Expr Int -> Expr Int -> Expr Int
    Minus :: Expr Int -> Expr Int -> Expr Int
    Times :: Expr Int -> Expr Int -> Expr Int
    Eq :: Expr Int -> Expr Int -> Expr Bool
    Gt :: Expr Int -> Expr Int -> Expr Bool
    Lt :: Expr Int -> Expr Int -> Expr Bool
    If :: Expr Bool -> Expr Int -> Expr Int -> Expr Int
    Sum :: Expr [Int] -> Expr [Int] -> Expr Int

deriving instance Show (Expr a)

data ExprW = EI (Expr Int) | EB (Expr Bool) | EL (Expr [Int]) deriving Show

class Rando a where
    rando :: MonadRandom m => Int -> m a

instance Rando (Expr Bool) where
    rando 1 = B <$> getRandom
    rando d = do
      c <- getRandomR (0 , 3 :: Int)
      case c of
        0 -> B  <$> getRandom
        1 -> Eq <$> rando (d - 1) <*> rando (d - 1)
        2 -> Gt <$> rando (d - 1) <*> rando (d - 1)
        3 -> Lt <$> rando (d - 1) <*> rando (d - 1)

instance Rando (Expr Int) where
    rando 1 = I <$> getRandom
    rando d = do
      c <- getRandomR (0, 5 :: Int)
      case c of
        0 -> I     <$> getRandom
        1 -> Plus  <$> rando (d - 1) <*> rando (d - 1)
        2 -> Times <$> rando (d - 1) <*> rando (d - 1)
        3 -> Minus <$> rando (d - 1) <*> rando (d - 1)
        4 -> If <$> rando (d - 1) <*> rando (d - 1) <*> rando (d - 1)
        5 -> Sum <$> rando (d - 1) <*> rando (d-1)

instance Rando (Expr a) => Rando (Expr [a]) where
    rando d = do 
      n <- getRandomR (0,2)
      L <$> replicateM n (rando (d - 1))

instance Rando ExprW where
    rando d = do
      c <- getRandomR (0,2 :: Int)
      case c of
        0 -> EI <$> rando d
        1 -> EB <$> rando d
        2 -> EL <$> rando d

initialize :: MonadRandom m => m ExprW
initialize = rando 2

nonterminal :: MonadRandom m => m ExprW
nonterminal = rando 5

main :: IO ()
main = print =<< evalRandIO nonterminal
