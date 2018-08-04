{-# LANGUAGE FlexibleContexts #-}
module Spec where
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Lib

type TI = []

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap (foldr (Cons) Nil) (listOf arbitrary)

instance Eq a => EqProp (List a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where (=-=) = eq


instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

-- Eq prop from the book appears to be wrong, the below works
-- instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
--   (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)
instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

-- instance Arbitrary a => Arbitrary (Tree a) where
--    arbitrary = do
--      a <- arbitrary
--      b <- arbitrary
--      c <- arbitrary
--      frequency [ (1, return $ Leaf a)
--                , (1, return $ Node a b c)
--                , (1, return Empty)
--                ]

instance (Eq a) => EqProp (Tree a) where (=-=) = eq

main :: IO ()
main = hspec $ do
  -- describe "lists" $ do
  --   it "is traversable" $ do
  --     let trigger :: TI (Int, Int, [Int])
  --         trigger = undefined
  --     quickBatch (traversable trigger)
  -- describe "Identity" $ do
  --   it "is traversable" $ do
  --     let trigger :: Identity (Int, Double, String)
  --         trigger = undefined --     quickBatch (functor trigger)
  --     quickBatch (traversable trigger)
  -- describe "Constant" $ do
  --   it "is traversable" $ do
  --     let trigger :: Constant Int (Int, Double, String)
  --         trigger = undefined
  --     quickBatch (functor trigger)
  --     quickBatch (traversable trigger)
  -- describe "List" $ do
  --   it "is traversable" $ do
  --     let trigger :: List (Int, Double, String)
  --         trigger = undefined
  --     quickBatch (functor trigger)
  --     quickBatch (monoid trigger)
  --     quickBatch (traversable trigger)
  -- describe "Three" $ do
  --   it "is traversable" $ do
  --     let trigger :: Three Int String (Int, Double, String)
  --         trigger = undefined
  --     quickBatch (functor trigger)
  --     quickBatch (traversable trigger)
  -- describe "Pair" $ do
  --   it "is traversable" $ do
  --     let trigger :: Pair Int (Int, Double, String)
  --         trigger = undefined
  --     quickBatch (functor trigger)
  --     quickBatch (traversable trigger)
  -- describe "Pair" $ do
  --   it "is traversable" $ do
  --     let trigger :: Pair Int (Int, Double, String)
  --         trigger = undefined
  --     quickBatch (functor trigger)
  --     quickBatch (traversable trigger)
  -- describe "Big" $ do
  --   it "is traversable" $ do
  --     let trigger :: Big [Int] ([Int], [Double], String)
  --         trigger = undefined
  --     quickBatch (functor trigger)
  --     quickBatch (semigroup trigger)
  --     quickBatch (monoid trigger)
  --     quickBatch (applicative trigger)
  --     quickBatch (traversable trigger)
  -- describe "Bigger" $ do
  --   it "is traversable" $ do
  --     let trigger :: Bigger [Int] ([Int], [Double], String)
  --         trigger = undefined
  --     verboseBatch (functor trigger)
  --     verboseBatch (semigroup trigger)
  --     verboseBatch (monoid trigger)
  --     verboseBatch (applicative trigger)
  --     verboseBatch (traversable trigger) -- describe "S" $ do --   it "is traversable" $ do
  -- describe "S" $ do
  --   it "is traversable" $ do
  --     let trigger :: S [] ([Int], [Int], [Int])
  --         trigger = undefined
  --     verboseBatch (functor trigger)
  --     verboseBatch (semigroup trigger)
  --     verboseBatch (monoid trigger)
  --     verboseBatch (traversable trigger)

  describe "Tree" $ do
    it "is traversable" $ do
      let trigger :: Tree (Int, Double, String)
          trigger = undefined
      verboseBatch (functor trigger)
      verboseBatch (semigroup trigger)
      verboseBatch (monoid trigger)
      verboseBatch (traversable trigger)
