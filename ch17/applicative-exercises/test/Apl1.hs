module Apl1 where

import Control.Applicative
import Data.Monoid
import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Semigroup a => Semigroup (ZipList a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = f x `Cons` fmap f xs

instance Semigroup (List a) where
  (<>) a Nil = a
  (<>) Nil b = b
  (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Monoid (List a) where
  mempty = Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = resize 3 $ fmap (foldr Cons Nil) (listOf arbitrary)

instance CoArbitrary a => CoArbitrary (List a) where
  coarbitrary Nil     = variant 0
  coarbitrary (Cons x xs) = variant 1 . coarbitrary (x,xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    x <- arbitrary
    return $ ZipList' x

instance (Arbitrary e, Arbitrary a, CoArbitrary e, CoArbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return $ Failure' x)
              , (1, return $ Success' y)
              ]


instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)

take' :: Int -> List a -> List a
take' int list = if int >= 0
               then go int list Nil
               else Nil
  where go 0 _ acc = acc
        go _ Nil acc = acc
        go i (Cons x xs) acc = go (i-1) xs (Cons x acc)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 1000 l
          ys' = let (ZipList' l) = ys
                in take' 1000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ Cons x Nil
  (<*>) (ZipList' a) (ZipList' b) = ZipList' $ zipApp a b
    where zipApp Nil _ = Nil
          zipApp _ Nil = Nil
          zipApp (Cons f fs) (Cons x xs) =  f x `Cons` (fs `zipApp`  xs)

data Validation e a = Failure' e
                    | Success' a
                    deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Success' a) (Success' b) = Success' $ a b
  (<*>) (Failure' a) (Success' _) = Failure' a
  (<*>) (Success' _) (Failure' b) = Failure' b
  (<*>) (Failure' a) (Failure' b) = Failure' (a <> b)

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

listPure :: a -> [a]
listPure = pure

ioPure :: a -> IO a
ioPure = pure

tupPure :: (Monoid a) => b -> (a,b)
tupPure = pure

funcPure :: a -> (->) e a
funcPure = pure

data Pair a = Pair a a deriving (Show, Eq)
data Two a b = Two a b deriving (Show, Eq)
data Three a b c = Three a b c deriving (Show, Eq)
data Three' a b = Three' a b b deriving (Show, Eq)
data Four a b c d = Four a b c d deriving (Show, Eq)
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Pair) where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c $ f d

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)


instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f1 f2) (Pair a b) = Pair (f1 a) (f2 b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two f1 f2) (Two a b) = Two (f1 <> a) (f2 b)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three f1 f2 f3) (Three a b c) = Three (f1 <> a) (f2 <> b) (f3 c)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' f1 f2 f3) (Three' a b c) = Three' (f1 <> a) (f2  b) (f3 c)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four f1 f2 f3 f4) (Four a b c d) = Four (f1 <> a) (f2 <> b) (f3 <> c) (f4 d)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' f1 f2 f3 f4) (Four' a b c d) = Four' (f1 <> a) (f2 <> b) (f3 <> c) (f4 d)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 (,,)
