module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

multBy :: (Eq a, Num a) => a -> a -> a
multBy = go 0 0
          where go count s a b
                    | count == b = s
                    | otherwise = go (count + 1) (s + a) a b

-- :t arbitrary
-- arbitrary :: Arbitrary a => Gen a

  -- :t sample
-- sample :: Show a => Gen a -> IO ()

  -- :t sample'
-- sample' :: Show a => Gen a -> IO ()
-- sample' :: Gen a ->  IO [a]

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: (Arbitrary a) => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))
            ]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

  
main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1::Integer) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $
      (2 + 2::Integer) == 4 `shouldBe` True
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x::Integer)
    it "15 divided by 3 is 5" $ do
      dividedBy 15 (3::Integer)  `shouldBe` (5,0)
    it "22 divided by 5 is 4 Remainder 2" $ do
      dividedBy 22 (5::Integer) `shouldBe` (4,2)
    it "1 multiplied by 0 is 0" $ do
      multBy 1 (0::Integer) `shouldBe` 0
    it "0 multiplied by 1 is 0" $ do
      multBy 0 (1::Integer) `shouldBe` 0
    it "1 multiplied by 1 is 1" $ do
      multBy 1 (1::Integer) `shouldBe` 1
    it "1 multiplied by 2 is 2" $ do
      multBy 1 (2::Integer) `shouldBe` 2
    it "20 multiplied by 10 is 200" $ do
      multBy 20 (10::Integer) `shouldBe` 200
