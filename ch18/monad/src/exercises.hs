module Exercises where

import Control.Monad (join, (>=>))
import Data.List (foldl')

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma 

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = xs >>= \x ->
    if even x
      then [x*x, x*x]
      else []

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a) _ = First a
  (<*>) _ (First b) = First b
  (<*>) (Second a) (Second b) = Second $ a b

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second a) f = f a

data CountMe a = CountMe Integer a
               deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  (<*>) (CountMe n f) (CountMe n' a) = CountMe (n+n') (f a)

instance Monad CountMe where
  return = pure
  (>>=) (CountMe n a) f = let CountMe n' b = f a
                          in CountMe (n + n') b

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

data PhtEither a b = PLeft b
                   | PRight a
                   deriving (Eq, Show)


instance Functor (PhtEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft $ f a

instance Applicative (PhtEither b) where
  pure = PLeft
  (<*>) (PRight a) _ = PRight a
  (<*>) _ (PRight b) = PRight b
  (<*>) (PLeft a) (PLeft b) = PLeft $ a b

instance Monad (PhtEither b) where
  return = pure
  (>>=) (PRight a) _ = PRight a
  (>>=) (PLeft a) f = f a

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a)  = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x ) (fmap f xs)

instance Semigroup (List a) where
  (<>) Nil a = a
  (<>) a Nil = a
  (<>) (Cons a as) bs = a `Cons` (as <> bs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil y = Nil
  (<*>) x Nil = Nil
  (<*>) (Cons f fs) ys = (f <$> ys) <> (fs <*> ys)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) xs f = go (<>) Nil (f <$> xs)

go :: (a -> b -> b) -> b -> List a -> b
go f acc Nil = acc
go f acc (Cons x xs) = f x $ go f acc xs

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = f <$> a

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b =  f <$> a <*> b

a :: Monad m => m a -> m (a -> b) -> m b
a ma mab = mab <*> ma

meh :: Monad m => [a] -> (a -> m b) ->  m [b]
meh xs f = foldr foldIntoList (return []) $ f <$> xs
  where foldIntoList :: Monad m => m b -> m [b] -> m [b]
        foldIntoList mb mlb = do
          b <- mb
          l <- mlb
          return $ b:l

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
