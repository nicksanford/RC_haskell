{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Lib where

import Control.Applicative
import Data.Char
import Data.Maybe

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

boopDoop :: Num a => a -> a
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

bip :: Num a => a -> a
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupledA :: [Char] -> ([Char], [Char])
tupledA = liftA2 (,) rev cap

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  r <- rev
  c <- cap
  return (r, c)

tupledMB :: [Char] -> ([Char], [Char])
tupledMB = rev >>= (\r -> cap >>= (\c -> return (r, c)))

newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a 
ask = Reader id

-- pure :: a -> f a
-- pure :: a  -> (r -> a)

-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName
                     , dogName :: DogName
                     , address :: Address
                     } deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName
               , dogsAddress :: Address
               } deriving (Eq, Show)

pers :: Person
pers = Person ( HumanName "Big Bird")
              ( DogName "Barkly" )
              ( Address "Sessame Street" )
chris :: Person
chris = Person ( HumanName "Chris" )
               ( DogName "Papu" )
               ( Address "Austin" )

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = myLiftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rab) (Reader ra) = Reader (rab <*> ra)

instance Monad (Reader r) where
   return = pure
   (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
   (>>=) (Reader ra) aRb = Reader $ \r -> (runReader $ aRb (ra r)) r

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

lup :: forall a b. Eq a => a -> [(a, b)] -> Maybe b
lup q = foldr f Nothing
  where f :: (a, b) -> Maybe b -> Maybe b
        f _ (Just b) = Just b
        f (a, b) _ = if a == q then Just b else Nothing

xs :: Maybe Integer
xs = lup 3 $ zip x y

ys :: Maybe Integer
ys = lup 6 $ zip y z

zs :: Maybe Integer
zs = lup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' a = lup a $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  -- print $ sequenceA [Just 3, Just 2, Just 1]
  -- print $ sequenceA [x, y]
  -- print $ sequenceA [xs, ys]
  -- print $ summed <$> ((,) <$> xs <*> ys)
  -- print $ fmap summed ((,) <$> xs <*> zs)
  -- print $ bolt 7
  -- print $ fmap bolt z
  -- print $ sequenceA [(>3), (<8), even] 7
  print $ foldr (&&) True (sequA 6)
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 xs
