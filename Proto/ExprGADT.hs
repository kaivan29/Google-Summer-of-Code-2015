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
