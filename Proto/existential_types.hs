--{-# LANGUAGE ExistentialQuantification #-}

--data ShowBox = forall s. Show s => SB s

--heteroList::[ShowBox]
--heteroList = [SB (), SB 5, SB True]

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- ∃ t. (t, t → t, t → String)
data Box = forall a. Box a (a -> a) (a -> String)
		  
data Tox = forall a. Tox a a (a -> a -> a) (a -> String)

boxa :: Box
boxa = Box 1 negate show

boxb :: Box
boxb = Box "foo" reverse show

boxc :: Tox
boxc = Tox 2 3 add show

add :: Num a => a -> a -> a
add a b = a + b

apply :: Box -> String
apply (Box x f p) = p (f x)

fapply :: Tox -> String
fapply (Tox x y f p) = p (f x y)

-- ∃ t. Show t => t
data SBox = forall a. Show a => SBox a

boxes :: [SBox]
boxes = [SBox (), SBox 2, SBox "foo"]

showBox :: SBox -> String
showBox (SBox a) = show a

main :: IO ()
main = mapM_ (putStrLn . showBox) boxes
-- ()
-- 2
-- "foo"
