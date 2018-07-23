module Exercises where

import Control.Applicative
import Data.List (elemIndex)

f' :: Integer -> Maybe String
f' x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")
               ]

g :: Integer -> Maybe String
g x = lookup x [ (7, "sup?")
               , (8, "chris")
               , (9, "aloha")
               ]

h :: Integer -> Maybe Integer
h x = lookup x [ (2, 3)
               , (5, 6)
               , (7, 8)
               ]

m :: Integer -> Maybe Integer
m x = lookup x [ (4, 10)
               , (8, 13)
               , (1, 9001)
               ]

added :: Maybe Integer
added = pure (+3) <*> lookup 3 (zip [1,2,3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x1 :: Maybe Int
x1 = elemIndex 3 [1, 2, 3, 4, 5]

y1 :: Maybe Int
y1 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x1 <*> y1

--
xs = [1, 2, 3]
ys = [4, 5, 6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y2 :: Maybe Integer
y2 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x2 <*> y2

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ =  Constant mempty
  (<*>) a b = Constant $ getConstant a `mappend` getConstant b

x3 = const <$> Just "Hello" <*> pure "World"

y3 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
