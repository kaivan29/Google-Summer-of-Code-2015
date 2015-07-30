{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

data Indexor :: [k] -> k -> * where
    IZ :: Indexor (k ': ks) k
    IS :: Indexor (k ': ks) k -> Indexor (j ': k ': ks) k

data Expr :: [*] -> * -> * where
    I      :: Int                           -> Expr vs Int
    B      :: Bool                          -> Expr vs Bool
    V      :: Indexor vs a                  -> Expr vs a
    O1     :: Op1 a -> Expr vs a -> Expr vs a
    O2     :: Op2 a b -> Expr vs a -> Expr vs a -> Expr vs b
    (:$)   :: Expr (a ': vs) b -> Expr vs a -> Expr vs b
    List   :: [Expr vs a] -> Expr vs [a]
    Const  :: Expr vs a -> Expr (b ': vs) a
    Foldr  :: Expr (a ': b ': vs) b -> Expr vs b -> Expr vs [a] -> Expr vs b

data Op1 :: * -> * where
    Abs :: Op1 Int
    Signum :: Op1 Int
    Not :: Op1 Bool

data Op2 :: * -> * -> * where
    Plus :: Op2 Int Int
    Times :: Op2 Int Int
    Minus :: Op2 Int Int
    LEquals :: Op2 Int Bool
    And    :: Op2 Bool Bool
    Or    :: Op2 Bool Bool

deriving instance Show (Indexor ks k)
-- deriving instance Show a => Show (Expr vs a)

main :: IO ()
main = print . eval $ Foldr (V IZ + V (IS IZ)) (I 0) (List [I 1, I 2, I 3])

eval :: Expr '[] a -> a
eval (I i)       = i
eval (B b)       = b
eval (V v)       = error $ "Unexpected unbound variable " ++ show v ++ "...should be impossible really."
eval (O1 o e1) = op1 o (eval e1)
eval (O2 o e1 e2) = op2 o (eval e1) (eval e2)
eval (e1 :$ e2)  = eval $ reduceWith e2 e1
eval (List es)   = map eval es
eval (Foldr ef ez el) = case el of
                          List es -> eval $ foldr (\x z -> (ef :$ Const x) :$ z) ez es
                          _       -> error "Unexpected list type not formed by List constructor"


op1 :: Op1 a -> a -> a
op1 Abs = abs
op1 Signum = signum
op1 Not = not

op2 :: Op2 a b -> a -> a -> b
op2 Plus = (+)
op2 Times = (*)
op2 Minus = (-)
op2 LEquals = (<=)
op2 And = (&&)
op2 Or = (||)

reduceWith :: Expr vs v -> Expr (v ': vs) a -> Expr vs a
reduceWith ev el = case el of
                     I i        -> I i
                     B b        -> B b
                     V IZ       -> ev
                     V (IS v)   -> V v
                     Const e1   -> e1
                     O1 o e1    -> O1 o $ reduceWith ev e1
                     O2 o e1 e2 -> O2 o (reduceWith ev e1) (reduceWith ev e2)
                     e1 :$ e2   -> reduceWith ev (reduceWith e2 e1)
                     List es    -> List $ map (reduceWith ev) es
                     -- Foldr ef ez el -> Foldr (reduceWith ev ef) (reduceWith ev ez) (reduceWith ez el)

instance Num (Expr vs Int) where
    (+) = O2 Plus
    (*) = O2 Times
    (-) = O2 Minus
    abs = O1 Abs
    signum = O1 Abs
    fromInteger = I . fromInteger
