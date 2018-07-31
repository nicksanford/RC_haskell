import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec

import Exercises

sumBatch :: Sum Int (String, Double, Int)
sumBatch = undefined

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ First a)
              , (1, return $ Second b)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhtEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return $ PLeft x)
              , (1, return $ PRight y)
              ]

instance (Eq a, Eq b) => EqProp (PhtEither b a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = list `suchThat` (\x -> (go (\_ sum -> sum + 1) 0 x) <= 10)
    where list = (foldr Cons Nil) <$> listOf arbitrary
          go f acc Nil = acc
          go f acc (Cons x xs) = f x $ go f acc xs

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = hspec $ do
  describe "nope" $ do
    it "follows the functor, applicative, and monad laws" $ do
      quickBatch $ functor (NopeDotJpg :: Nope (Int, String, Double))
      quickBatch $ applicative (NopeDotJpg :: Nope (Int, String, Double))
      quickBatch $ monad (NopeDotJpg :: Nope (Int, String, Double))
  describe "PhtEither" $ do
    it "follows the functor, applicative, and monad laws" $ do
      quickBatch $ functor (undefined :: PhtEither Int (Int, String, Double))
      quickBatch $ applicative (undefined :: PhtEither Int (Int, String, Double))
      quickBatch $ monad (undefined :: PhtEither Int (Int, String, Double))
  describe "Identity" $ do
    it "follows the functor, applicative, and monad laws" $ do
      quickBatch $ functor (undefined :: Identity (Int, String, Double))
      quickBatch $ applicative (undefined :: Identity (Int, String, Double))
      quickBatch $ monad (undefined :: Identity (Int, String, Double))
  describe "List" $ do
    it "follows the functor, applicative, and semigroup monad laws" $ do
      quickBatch $ semigroup (undefined :: List (Int, String, Double))
      quickBatch $ functor (undefined :: List (Int, String, Double))
      quickBatch $ applicative (undefined :: List (Int, String, Double))
      quickBatch $ monad (undefined :: List (Int, String, Double))
  describe "j" $ do
    it "behaves like join" $ do
      j [[1,2], [], [3]] `shouldBe` [1,2,3]
    it "behaves like join" $ do
      j (Just (Just 1)) `shouldBe` Just 1
    it "behaves like join" $ do
      (j $ Just Nothing) `shouldBe` (Nothing::Maybe Int)
    it "behaves like join" $ do
      j  Nothing `shouldBe` (Nothing::Maybe Int)
  describe "sumbatch" $ do
    it "follows the monad laws" $ do
      quickBatch (functor sumBatch)
      quickBatch (applicative sumBatch)
      quickBatch (monad sumBatch)
  describe "CountMe" $ do
    it "follows the monad laws" $ do
      let trigger :: CountMe (Int, String, Int)
          trigger = undefined
      quickBatch $ functor trigger
      quickBatch $ applicative trigger
      quickBatch $ monad trigger

    
