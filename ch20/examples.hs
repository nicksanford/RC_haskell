{-# LANGUAGE ScopedTypeVariables #-}
module Examples where

newtype Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a = NaDa
                | Yep a

instance Foldable Optional  where
  foldr _ z NaDa = z
  foldr f z (Yep a) = f a z

  foldl _ z NaDa = z
  foldl f z (Yep a) = f z a

  foldMap _ NaDa = mempty
  foldMap f (Yep a) = f a


sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: forall a t. (Foldable t, Eq a) =>  a -> t a -> Bool
elem x = foldr f False
  where f :: (Eq a) => a -> Bool -> Bool
        f a False = a == x
        f _ True = True

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr f Nothing
  where f :: Ord a => a -> Maybe a -> Maybe a
        f x Nothing = Just x
        f x (Just y) = if x < y then Just x else Just y

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing
  where f :: Ord a => a -> Maybe a -> Maybe a
        f x Nothing = Just x
        f x (Just y) = if x > y then Just x else Just y

null :: (Foldable t) => t a -> Bool
null x = Examples.length x == 0

length :: (Foldable t) => t a -> Int
length = foldr (\_ counter -> counter + 1) 0

--8. Some say this is all Foldable amounts to.
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- 9. Hint: use foldMap. Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = Examples.foldMap id

--10. Define foldMap in terms of foldr.
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap g = foldr f mempty
  where f x acc = g x `mappend` acc

data Conatant a b = Conatant b

instance Monoid b => Monoid (Conatant a b) where
  mempty = Conatant mempty
  mappend (Conatant x) (Conatant y) = Conatant $ mappend x y

instance Foldable (Conatant a) where
  foldr f z (Conatant a) = f a z

data Two a b = Two a b

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a b) (Two c d) = Two (mappend a c) (mappend b d)

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z

data Three a b c = Three a b c

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend (Three a b c) (Three d e f) = Three (mappend a d) (mappend b e) (mappend c f)

instance Foldable (Three a b) where
  foldr f z (Three _ _ a) = f a z

data Three' a b = Three' a b b

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
  mempty = Three' mempty mempty mempty
  mappend (Three' a b c) (Three' d e f) = Three' (mappend a d) (mappend b e) (mappend c f)

data Four' a b = Four' a b b b

instance (Monoid a, Monoid b) => Monoid (Four' a b) where
  mempty = Four' mempty mempty mempty mempty
  mappend (Four' a b c d) (Four' e f g h) =
    Four' (mappend a e)
          (mappend b f)
          (mappend c g)
          (mappend d h)

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF g = foldr f mempty
  where f x acc = if g x then mappend (pure x) acc else acc
