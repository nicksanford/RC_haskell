{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Network.Wreq
import Data.ByteString.Lazy hiding (map, foldr)
-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f = Examples.sequenceA . fmap f

--   sequenceA :: Applicative f => t (f a) -> f (t a)
--   sequenceA = Examples.traverse id

--catMaybes [Just 1, Just 2, Just 3] => [1,2,3]
-- sequenceA  [Just 1, Just 2, Just 3] => Just [1,2,3]

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

data OEither a b = OLeft a | ORight b deriving (Eq, Ord, Show)

instance Functor (OEither a) where
  fmap _ (OLeft x) = OLeft x
  fmap f (ORight x) = ORight $ f x

instance Applicative (OEither e) where
  pure = ORight
  OLeft e <*> _ = OLeft e
  ORight f <*> r = fmap f r

instance Foldable (OEither a) where
  foldMap _ (OLeft _) = mempty
  foldMap f (ORight y) = f y

  foldr _ z (OLeft _) = z
  foldr f z (ORight y) = f y z

instance Traversable (OEither a) where
  traverse _ (OLeft x) = pure (OLeft x)
  traverse f (ORight y) = ORight <$> f y

-- instance Functor ((,) a) where
--   fmap f (x,y) = (x, f y)

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u,f) <*> (v,x) = (u `mappend` v, f x)

-- instance Foldable ((,) a) where
--   foldMap f (_, y) = f y
--   foldr f z (_, y) = f y z

-- instance Traversable ((,) a) where
--   traverse f (x, y) = (,) x <$> f y

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

newtype Constant a b = Constant { getConstant :: a} deriving (Show, Eq)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ z _ = z

instance Traversable (Constant a) where
  traverse _ (Constant a) = Constant <$> pure a

data Optional a = Nada | Yep a

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep a) = f a z
instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = f x `Cons` fmap f xs

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x $ foldr f z xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldr f z (Pair _ b) = f b z

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance (Semigroup a, Semigroup b) => Semigroup (Big a b) where
  (<>) (Big a b c) (Big d e f) = Big (a <> d) (b <> e) (c <> f)

instance (Monoid a, Monoid b) => Monoid (Big a b) where
  mempty = Big mempty mempty mempty

instance Monoid a => Applicative (Big a) where
  pure x = Big mempty x x
  (<*>) (Big a b c) (Big d e f) = Big (a <> d) (b e) (c f)

instance Foldable (Big a) where
  foldMap f (Big _ b c) = f b <> f c

instance Traversable (Big a) where
  sequenceA (Big a fb fc) = (Big a) <$> fb <*> fc

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance (Semigroup a, Semigroup b) => Semigroup (Bigger a b) where
  (<>) (Bigger a b c d) (Bigger e f g h) = Bigger (a<>e) (b<>f) (c<>g) (d<>h)

instance (Monoid a, Monoid b) => Monoid (Bigger a b) where
  mempty = Bigger mempty mempty mempty mempty

instance (Monoid a) => Applicative (Bigger a) where
  pure x = Bigger mempty x x x
  (<*>) (Bigger a b c d) (Bigger e f g h) = Bigger (a <> e) (b f) (c g) (d h)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
  sequenceA (Bigger a b c d) = Bigger a <$> b <*> c <*> d

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance (Applicative n, Semigroup a) => Semigroup (S n a) where
  (<>) (S n a) (S m b) = S n' a'
    where n' = fmap (<>) n <*> m
          a' = a <> b

instance (Applicative n, Monoid a) => Monoid (S n a) where
  mempty = S (pure mempty) mempty

instance (Foldable n) => Foldable (S n) where
  foldMap f (S n a) = (foldMap f n) <> (f a)

instance (Traversable n, Applicative n) => Traversable (S n) where
  traverse f (S n a) =  S <$> traverse f n <*> f a

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node a b c) = Node (f <$> a) (f b) (f <$> c)

instance Foldable Tree where
  foldMap f (Leaf a) = f a
  foldMap f (Node a b c) = foldMap f a <> f b <> foldMap f c

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node a b c) = Node <$> (traverse f a) <*>  (f b) <*> (traverse f c)
