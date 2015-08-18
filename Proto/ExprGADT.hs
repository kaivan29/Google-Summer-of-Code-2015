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

import Control.Arrow
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.State
import Data.Foldable
import Data.IntMap.Strict           (IntMap)
import Data.List
import Data.Maybe
import Data.Proxy
import GHC.TypeLits
import Text.Show
import qualified Data.IntMap.Strict as IM

data Indexor :: [k] -> k -> * where
    IZ :: Indexor (k ': ks) k
    IS :: Indexor ks k -> Indexor (j ': ks) k


data Expr :: [*] -> * -> * where
    I      :: Int                           -> Expr vs Int
    B      :: Bool                          -> Expr vs Bool
    V      :: Indexor vs a                  -> Expr vs a
    O1     :: Op1 a -> Expr vs a -> Expr vs a
    O2     :: Show a => Op2 a b -> Expr vs a -> Expr vs a -> Expr vs b
    (:$)   :: Show a => Expr (a ': vs) b -> Expr vs a -> Expr vs b
    List   :: Show a => [Expr vs a] -> Expr vs [a]
    Const  :: Expr vs a -> Expr (b ': vs) a
    Foldr  :: Show a => Expr (a ': b ': vs) b -> Expr vs b -> Expr vs [a] -> Expr vs b

infixr 1 :$

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
deriving instance Show (Op1 a)
deriving instance Show (Op2 a b)
deriving instance Show a => Show (Expr vs a)

-- tester :: Rand StdGen (Expr '[Int, Int, Int] Int)
tester :: MonadRandom m => m (Expr '[] Int)
tester = rando 5

tester2 :: MonadRandom m => m(Expr '[] Bool)
tester2 = rando 5

tester3 :: MonadRandom m => m(Expr '[] [Int])
tester3 = rando 5

tester4 :: MonadRandom m => m(Expr '[] [Bool])
tester4 = rando 5

main :: IO ()
-- main = print . eval $ ((V IZ - V (IS IZ)) :$ I 5) :$ I 2
-- main = print . eval $ List [I 1, I 2]
-- main = print . eval $ (V IZ + V (IS IZ)) :$ I 4 :$ I 5
-- main = print . eval $ Foldr (V IZ + V (IS IZ)) (I 0) (List [I 1, I 2, I 3])
-- main = print =<< evalRandIO tester
main = do
    e <- tester3
    print e
    putStrLn . showPretty $ e
    print $ eval e

eval :: Show a => Expr '[] a -> a
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

class Rando a where
    rando :: MonadRandom m => Int -> m a

instance Rando (Expr '[] Int) where
    rando 1 = I <$> getRandomR (-10,10)
    rando d = do
      c <- getRandomR (0, 8 :: Int)
      case c of
        0 -> I <$> getRandomR (-10, 10)
        1 -> O1 Abs <$> rando (d - 1)
        2 -> O2 Plus <$> rando (d - 1) <*> rando (d - 1)
        3 -> O2 Times <$> rando (d - 1) <*> rando (d - 1)
        4 -> O2 Times <$> rando (d - 1) <*> rando (d - 1)
        _ -> do
          f <- rando (d - 1)
          x <- rando (d - 1)
          return $ f :$ (x :: Expr '[] Int)

instance (Rando (Expr vs Int), Rando (Indexor (Int ': vs) Int)) => Rando (Expr (Int ': vs) Int) where
    rando 1 = V <$> rando 1
    rando d = do
      c <- getRandomR (0, 8 :: Int)
      case c of
        0 -> V <$> rando d
        1 -> O1 Abs <$> rando (d - 1)
        2 -> O2 Plus <$> rando (d - 1) <*> rando (d - 1)
        3 -> O2 Times <$> rando (d - 1) <*> rando (d - 1)
        4 -> O2 Times <$> rando (d - 1) <*> rando (d - 1)
        _ -> do
          f <- rando (d - 1)
          x <- rando (d - 1)
          return $ f :$ (x :: Expr (Int ': vs) Int)

instance Rando (Expr '[] Bool) where
    rando 1 = B <$> getRandom
    rando d = do
      c <- getRandomR (0, 8 :: Int)
      case c of
        0 -> B <$> getRandom
        1 -> O2 And <$> rando (d - 1) <*> rando (d - 1)
        2 -> O2 Or <$> rando (d - 1) <*> rando (d - 1)
        3 -> O2 LEquals <$> rando (d - 1) <*> rando (d - 1)
        _ -> do
          f <- rando (d - 1)
          x <- rando (d - 1)
          return $ f :$ (x :: Expr '[] Bool)

instance (Rando (Expr vs Bool), Rando (Indexor (Bool ': vs) Bool)) => Rando (Expr (Bool ': vs) Bool) where
    rando 1 = V <$> rando 1
    rando d = do
      c <- getRandomR (0, 8 :: Int)
      case c of
        0 -> V <$> rando d
        1 -> O2 And <$> rando (d - 1) <*> rando (d - 1)
        2 -> O2 Or <$> rando (d - 1) <*> rando (d - 1)
        _ -> do
          f <- rando (d - 1)
          x <- rando (d - 1)
          return $ f :$ (x :: Expr (Bool ': vs) Bool)

instance (Show a, Rando (Expr vs a)) => Rando (Expr vs [a]) where
    rando 1 = do
      n <- getRandomR (0,2)
      List <$> replicateM n (rando 1)
    rando d = do
      n <- getRandomR (0,2)
      List <$> replicateM n (rando (d - 1))

instance {-# OVERLAPPING #-} Rando (Indexor (v ': '[]) v) where
    rando _ = return IZ

instance {-# OVERLAPPING #-} Rando (Indexor vs u) => Rando (Indexor (v ': vs) u) where
    rando d = IS <$> rando d

instance {-# OVERLAPPING #-} (Rando (Indexor (u ': vs) v)) => Rando (Indexor (v ': u ': vs) v) where
    rando d = do
      c <- getRandomR (0, 2 :: Int)     -- can be made a bit smarter
      case c of
        0 -> return IZ
        _ -> IS <$> rando d

data ExprW :: * where
    EI :: (Expr '[] Int) -> ExprW
    EB :: (Expr '[] Bool) -> ExprW
    IL :: (Expr '[] [Int]) -> ExprW
    BL :: (Expr '[] [Bool]) -> ExprW

instance Rando ExprW where
    rando d = do
      c <- getRandomR (0,3 :: Int)
      case c of
        0 -> EI <$> rando d
        1 -> EB <$> rando d
        2 -> IL <$> rando d
        3 -> BL <$> rando d

-- | Pretty printing

indexor :: Indexor ks k -> Int
indexor IZ = 0
indexor (IS x) = 1 + indexor x

showPretty :: Expr vs x -> String
showPretty e = let (res, (os, _)) = runState (showExpr e [] <* fillOs) (IM.empty, varNames)
               in  if IM.null os
                     then res 0 ""
                     else showString ("\\" ++ unwords (IM.elems os) ++ " -> ")
                        . res 0
                        $ ""
  where
    showExpr :: Expr vs x -> [String] -> State (IntMap String, [String]) (Int -> ShowS)
    showExpr e vs = case e of
        -- I i -> return $ \p -> showParen (i < 0) $ showsPrec p i
        I i -> return $ flip showsPrec i
        B b -> return $ flip showsPrec b
        V v -> let i = indexor v
                   o = i - length vs
               in  if  o >= 0
                     then do
                       os <- lookupOs o
                       case os of
                         Just x  -> return $ \_ -> showString x
                         Nothing -> do
                           x <- pop
                           addO o x
                           return $ \_ -> showString x
                     else return $ \_ -> showString (vs !! i)
        O1 op e1 -> do
            e1' <- showExpr e1 vs
            return $ showAp (const $ showString (op1 op)) e1'
        O2 op e1 e2 -> do
            e1' <- showExpr e1 vs
            e2' <- showExpr e2 vs
            let (opFix, opPrec, opName) = op2 op
            return $ showOp opFix opPrec opName e1' e2'
        ef :$ ex -> do
            v <- pop
            ef' <- showExpr ef (v : vs)
            ex' <- showExpr ex vs
            let ef'' = showParen True
                         $ showString ("\\" ++ v ++ " -> ")
                         . ef' 0
            return $ showOp InfixL 10 " " (const ef'')
                                          ex'

        List exs -> do
            exs' <- mapM (`showExpr` vs) exs
            return . const $ showListWith ($ 0) exs'
        Const e1 -> do
            e1' <- showExpr e1 vs
            return $ showAp (const $ showString "const") e1'
        Foldr ef ez exs -> do
            x <- pop
            y <- pop
            ef' <- showExpr ef (x : y : vs)
            ez' <- showExpr ez vs
            exs' <- showExpr exs vs
            let ef'' = showParen True
                         $ showString ("\\" ++ x ++ " " ++ y ++ " -> ")
                         . ef' 0
            return $ flip showAp exs'
                   . flip showAp ez'
                   . flip showAp (const ef'')
                   $ (\_ -> showString "foldr")
    showOp :: Fixity -> Int -> String -> (Int -> ShowS) -> (Int -> ShowS) -> Int -> ShowS
    showOp fixity prec op e1 e2 p = showParen (p > prec)
                                      $ e1 (if fixity == InfixL then prec else prec + 1)
                                      . showString op
                                      . e2 (if fixity == InfixR then prec else prec + 1)
    showAp :: (Int -> ShowS) -> (Int -> ShowS) -> Int -> ShowS
    showAp = showOp InfixL 10 " "
    pop = state $ \(os, vn:vns) -> (vn, (os, vns))
    lookupOs o = gets  $ IM.lookup o . fst
    addO o vn  = modify $ first (IM.insert o vn)
    fillOs :: State (IntMap String, [String]) ()
    fillOs = do
        os <- gets fst
        unless (IM.null os) $ do
          let maxKey = fst . IM.findMin $ os
          forM_ [0..maxKey] $ \k -> do
            hasK <- gets $ IM.member k . fst
            unless hasK $ do
              vn <- pop
              addO k vn
    varNames :: [String]
    varNames = [ v : if n == 0 then "" else show n
               | n <- [0..]
               , v <- "xyzhijklmnpqrstuvw"]
    op1 :: Op1 a -> String
    op1 Abs    = "abs"
    op1 Signum = "signum"
    op1 Not    = "not"
    op2 :: Op2 a b -> (Fixity, Int, String)
    op2 Plus    = (InfixL, 6, " + ")
    op2 Times   = (InfixL, 7, " * ")
    op2 Minus   = (InfixL, 6, " - ")
    op2 LEquals = (Infix, 4, " <= ")
    op2 And     = (InfixR, 3, " && ")
    op2 Or      = (InfixR, 2, " || ")

data Fixity = InfixL | InfixR | Infix deriving Eq
