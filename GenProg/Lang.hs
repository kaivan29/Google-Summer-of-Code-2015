 {-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Main where

import Prelude hiding (lookup)
import GenProg
import GenProg.GenExpr
import GenProg.GenExpr.Data
import Data.Generics
import Control.Monad
import Control.Monad.Random

import Test.HUnit
import System.Exit

data WValue = VInt Int 
            | VBool Bool 
              deriving (Eq, Show)

data WExp = Val WValue
            
          | Var String
            
          | Plus WExp WExp
          | Minus WExp WExp
          | Multiplies WExp WExp            
          | Equals WExp WExp
          | Less WExp WExp
          | Greater WExp WExp
          | Foldl WExp WExp WExp

data WStmt = Empty
           | VarDecl String WExp
           | Assign String WExp
           | If WExp WStmt WStmt
           | Block [WStmt]

type Memory = [(String, WValue)]

eval :: WExp -> Memory -> WValue
eval (Val v) _ = v
eval (Var s) m =
  case lookup s m of
    Nothing -> error $ "Unknown variable " ++ s ++ "\nMEMORY IS: " ++ show (map fst m)
    Just v -> v
eval (Plus e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VInt $ asInt e1' + asInt e2'
eval (Minus e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VInt $ asInt e1' - asInt e2'
eval (Multiplies e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VInt $ asInt e1' * asInt e2'
eval (Equals e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VBool $ e1' == e2'
eval (Less e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VBool $ asInt e1' < asInt e2'
eval (Greater e1 e2) m =
  let e1' = eval e1 m
      e2' = eval e2 m
  in
   VBool $ asInt e1' > asInt e2'

--helper functions
--lookup would be used to find the variable names that have been created
lookup s [] = Nothing
lookup s ((k,v):xs) | s == k = Just v
                    | otherwise = lookup s xs
					
asInt (VInt v) = v
asInt x = error $ "Expected a number, got " ++ show x

asBool (VBool v) = v
asBool x = error $ "Expected a boolean, got " ++ show x
