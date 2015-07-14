{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TemplateHaskell #-}

import Control.Monad.Random
import Control.Monad
import Data.List
import Control.Monad.State
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class

--I have infinite length tree forming up because of the List. Can I make
--it more constraint as to what will the nodes of the List? Also, keeping
--a check on the number of nodes using runStateT

--runStateT (do { put "100 nodes"; n <- lift $ getRandomR (0, 10); return n }) 

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

--Can I have eval defined in such a way that it would also take List
-- and perform the operations on it?
eval :: Expr a -> a
eval (I n) = n
eval (B n) = n
eval (Plus e1 e2) = (eval e1) + (eval e2)
eval (Minus e1 e2) = (eval e1) - (eval e2)
eval (Times e1 e2) = (eval e1) * (eval e2)
eval (Eq e1 e2) = (eval e1) == (eval e2)
eval (Gt e1 e2) = (eval e1) > (eval e2)
eval (Lt e1 e2) = (eval e1) < (eval e2)
eval (If s e1 e2) = if (eval s) then (eval e1) else (eval e2)

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
