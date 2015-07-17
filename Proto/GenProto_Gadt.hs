{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TemplateHaskell, ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, UndecidableInstances, AllowAmbiguousTypes #-}

import Data.Generics
import Test.HUnit
import Control.Monad.Random
import Control.Monad
import Control.Monad.State

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
    Sum :: Expr [Int] -> Expr Int
    Product :: Expr [Int] -> Expr Int
    P :: (Expr Int -> Expr Int -> Expr Int) -> Expr (Int -> Int -> Int)
    FoldL :: Expr (Int -> Int -> Int) -> Expr Int -> Expr [Int] -> Expr Int

deriving instance Typeable(Expr) 
deriving instance Show (Expr a)
--deriving instance Eq (Expr a)

--Can I have eval defined in such a way that it would also take List and perform the operations on it? Works for List too. magic of GADTs
{-in case of eval (L n), eval :: Expr [a] -> [a] ; and n :: [Expr a] ; and map eval n :: [a]-}
eval :: Expr a -> a
eval (I n) = n
eval (B n) = n
eval (L n) = map eval n
eval (Plus e1 e2) = (eval e1) + (eval e2)
eval (Minus e1 e2) = (eval e1) - (eval e2)
eval (Times e1 e2) = (eval e1) * (eval e2)
eval (Eq e1 e2) = (eval e1) == (eval e2)
eval (Gt e1 e2) = (eval e1) > (eval e2)
eval (Lt e1 e2) = (eval e1) < (eval e2)
eval (If s e1 e2) = if (eval s) then (eval e1) else (eval e2)
eval (Sum n) = foldl (+) 0 (eval n)
eval (Product n) = foldl (*) 1 (eval n)
--eval (P (Plus e1 e2)) = eval
--eval (FoldL op n ls) = foldl (\ x y -> eval op x y) (eval n) (eval ls)

data ExprW = EI (Expr Int) | EB (Expr Bool) | EL (Expr [Int]) deriving Show

--Rando Class
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
      c <- getRandomR (0, 6 :: Int)
      case c of
        0 -> I       <$> getRandom
        1 -> Plus    <$> rando (d - 1) <*> rando (d - 1)
        2 -> Times   <$> rando (d - 1) <*> rando (d - 1)
        3 -> Minus   <$> rando (d - 1) <*> rando (d - 1)
        4 -> If      <$> rando (d - 1) <*> rando (d - 1) <*> rando (d - 1)
        5 -> Sum     <$> rando (d - 1)
        6 -> Product <$> rando (d - 1)
        7 -> FoldL   <$> rando (d-1) <*> rando (d-1) <*> rando (d-1)

instance Rando (Expr a) => Rando (Expr [a]) where
    rando 1 = do
      n <- getRandomR (0,2)
      L <$> replicateM n (rando 1)
    rando d = do 
      n <- getRandomR (0,2)
      L <$> replicateM n (rando (d - 1))

instance Rando (Expr (Int -> Int -> Int)) where
    rando d = do
      c <- getRandomR (0, 2 :: Int)
      case c of
        0 -> Plus    <$> rando (d - 1) <*> rando (d - 1)
        1 -> Times   <$> rando (d - 1) <*> rando (d - 1)
        2 -> Minus   <$> rando (d - 1) <*> rando (d - 1)

instance Rando ExprW where
    rando d = do
      c <- getRandomR (0,2 :: Int)
      case c of
        0 -> EI <$> rando d
        1 -> EB <$> rando d
        2 -> EL <$> rando d

--Class Mutable
class Mutable a where
   mutate :: MonadRandom m => a -> m a
   
--instance Mutable (Expr Int) where

--Class Crossable
class Crossable a where
   crossOver :: MonadRandom m => a -> a -> m a

--randomGen :: MonadRandom m => m ExprW

--instance GenProg (Rand StdGen) (Expr a) where
-- terminal    = I `liftM` getRandomR (1,9)

initialize :: MonadRandom m => m ExprW
initialize = rando 2

nontermina :: MonadRandom m => m ExprW
nontermina = rando 5

main :: IO ()
main = print =<< evalRandIO nontermina
