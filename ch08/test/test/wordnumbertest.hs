-- NOTE: Today I learned that if your test suite entry point is not Main (I think) then `stack test` won't work.
-- However if your test script does not have a module name, but does have a main function, that will work.
import Test.Hspec
import Test.QuickCheck
import WordNumber (digitToWord, digits, wordNumber, half)
import Data.List (sort)
import Data.Char (toUpper)

data Fool = Fulse
          | Frue deriving (Eq, Show)

equalFool :: Gen Fool
equalFool = arbitrary

instance Arbitrary Fool where
  arbitrary = elements [Fulse, Frue]

unEqualFool :: Gen Fool
unEqualFool = frequency [ (3, return Fulse)
                        , (1, return Frue)
                        ]

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

genDouble :: Gen Double
genDouble = arbitrary

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_halfIdentity :: Property
prop_halfIdentity = forAll genDouble (\c -> halfIdentity c == c)

-- TODO I'm not sure why I need this `Arbitrary a` typeclass constraint here, the examples from the book didn't require it...
-- Maybe Lucy will know why this is
genList :: (Arbitrary a, Ord a) => Gen [a]
genList = listOf arbitrary

prop_charListOrdered :: Property
prop_charListOrdered = forAll  (genList::(Gen [Char])) (listOrdered . sort)

prop_intListOrdered :: Property
prop_intListOrdered = forAll  (genList::(Gen [Int])) (listOrdered . sort)

prop_integerListOrdered :: Property
prop_integerListOrdered = forAll  (genList::(Gen [Integer])) (listOrdered . sort)

prop_stringListOrdered :: Property
prop_stringListOrdered = forAll  (genList::(Gen [String])) (listOrdered . sort)

prop_plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_plusAssociative  a b c = (a + b) + c == a + (b + c)

prop_plusCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_plusCommutative a b = a + b == b + a

prop_multiAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_multiAssociative  a b c = (a * b) * c == a * (b * c)

prop_multiCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_multiCommutative a b = a * b == b * a

  
-- NOTE I defined these convenience functions b/c I didn't understand that `arbitrary` was already able to deal with tuples, non zeros, positives, etc. So these types of functions are really not necessary :)
-- numThreeple :: (Arbitrary a, Num a, Eq a) => Gen (a,a,a)
-- numThreeple = do
--   a <- arbitrary
--   b <- arbitrary
--   c <- arbitrary
--   return (a, b, c)

-- numTuple :: (Arbitrary a, Num a, Eq a) => Gen (a,a)
-- numTuple = do
--   a <- arbitrary
--   b <- arbitrary
--   return (a, b)

-- nonZeroTuple :: (Arbitrary a, Num a, Eq a) => Gen (NonZero a, NonZero a)
-- nonZeroTuple = do
--   a <- arbitrary
--   b <- arbitrary
--   return (a, b)


-- prop_dollar :: Fun Bool Bool -> Bool
-- prop_dollar (Fun _ f) = f $ True == f True

prop_dollar :: Eq b => (Blind (a -> b), a) -> Bool
prop_dollar (Blind f, x) = (f $ x) == (f x)

prop_dot :: Eq c => (Blind (b -> c), Blind (a -> b), a) -> Bool
prop_dot (Blind f, Blind g, a) = ((f . g) a) == (f (g a))

square x = x * x

twice :: (a -> a) -> (a -> a)
twice f = f . f

threeTimes :: (a -> a) -> (a -> a)
threeTimes f = f . f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = (toUpper x) : xs

prop_capitalizeIdempotence :: String  -> Bool
prop_capitalizeIdempotence x = and [ capitalizeWord x == twice capitalizeWord x
                                   , capitalizeWord x == threeTimes capitalizeWord x
                                   , capitalizeWord x == fourTimes capitalizeWord  x
                                   ]
prop_sortIdempotence :: String -> Bool
prop_sortIdempotence x = and [ sort x == twice sort x
                                   , sort x == threeTimes sort x
                                   , sort x == fourTimes sort  x
                                   ]

main :: IO ()
main = hspec $ do
  describe "idempotence" $ do
    it "holds for capitalization" $
      property prop_capitalizeIdempotence
    it "holds for sorting" $
      property prop_sortIdempotence
-- Doesn't work b/c of floating point math is not arbitrary precision
--  describe "square" $ do
--    it "has the following identity" $ do
--      property (\x -> ((square . sqrt) x) == x)
  describe "read . show" $ do
    it "has the following identity" $ do
      property ((\x -> (read (show x)) == x)::[Integer] -> Bool)
--  describe "length" $ do
--    it "has the property `f n xs = length (take n xs) == n`" $ do
--      property (forAll
--                (arbitrary::Gen ([Integer], Positive Int))
--                (\(xs, Positive n) -> length (take n xs) == n))
      
  describe "flip $ foldr (:)" $ do
    it "is the same as (++)" $ do
      property (forAll
                (arbitrary::Gen (String, String))
                (\(xs,ys) -> (foldr (:) ys xs) == (xs ++ ys)))
  describe "foldr (++) []" $ do
    it "is the same as concat" $ do
      property (forAll
                (arbitrary::Gen [String])
                (\xs -> (foldr (++) [] xs) == concat xs))
-- NOTE: You will need to use coarbitrary for this
-- This was very helpful: https://www.reddit.com/r/haskell/comments/8234pu/new_to_haskell_need_help_with_coarbitrary/
  describe "$" $ do
    it "has identity `f $ a == f a`" $ do
      property (forAll
                (arbitrary::Gen(Blind (String -> String), String))
                prop_dollar)

  describe "." $ do
    it "has identity `f . g == f (g x)`" $ do
      property (forAll
                (arbitrary::Gen(Blind (String -> String), Blind (String -> String), String))
                prop_dot)
      

  describe "revese" $ do
    it "reverse . reverse == id" $ do
      property $ forAll (arbitrary::Gen [Integer]) (\xs -> ((reverse . reverse) xs) == id xs)

  describe "quot rem" $ do
    it "have identity (quot x y)*y + (rem x y) == x" $ do
      property (forAll (arbitrary::Gen (NonZero Integer, NonZero Integer))
                (\((NonZero x), (NonZero y)) -> (quot x y)*y + (rem x y) == x))

  describe "div mod" $ do
     it "have identity (div x y)*y + (mod x y) == x" $ do
       property (forAll (arbitrary::Gen (NonZero Integer, NonZero Integer))
                 (\((NonZero x), (NonZero y)) -> (div x y)*y + (mod x y) == x))
  -- describe "^" $ do
  --   it " is associative" $ do
  --     property (forAll (arbitrary::Gen (Integer, Integer,Integer)) (\(x,y,z) -> (x^y)^z == x^(y^z)))
  --   it " is commutitive" $ do
  --     property (forAll (arbitrary::Gen (Positive Integer, Positive Integer)) (\(Positive x, Positive y) -> (x^y) == (y^x)))
  describe "ordered" $ do
    it "applies to [Char]" $ do
      property prop_charListOrdered
    it "applies to [Int]" $ do
      property prop_intListOrdered
    it "applies to [Integer]" $ do
      property prop_integerListOrdered
    it "applies to [String]" $ do
      property prop_stringListOrdered

  describe "plus" $ do
    it "is associative on Integers" $ do
      property $ forAll (arbitrary::Gen(Integer, Integer, Integer)) (\(a,b,c) -> (prop_plusAssociative a b c) == True)
    it "is commutative" $ do
      property $ forAll (arbitrary::Gen(Integer, Integer)) (\(a,b) -> (prop_plusCommutative a b) == True)
  describe "multi" $ do
    it "is associative on Integers" $ do
      property $ forAll (arbitrary::Gen(Integer, Integer, Integer)) (\(a,b,c) -> (prop_multiAssociative a b c) == True)
    it "is commutative" $ do
      property $ forAll (arbitrary::Gen(Integer, Integer)) (\(a,b) -> (prop_multiCommutative a b) == True)
  
  describe "half" $ do
    it "halfIdentity x == x" $ do
      property prop_halfIdentity

  describe "digitToword" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
    it "returns two for 2" $ do
      digitToWord 2 `shouldBe` "two"
    it "returns three for 3" $ do
      digitToWord 3 `shouldBe` "three"
    it "returns four for 4" $ do
      digitToWord 4 `shouldBe` "four"
    it "returns five for 5" $ do
      digitToWord 5 `shouldBe` "five"
    it "returns six for 6" $ do
      digitToWord 6 `shouldBe` "six"
    it "returns seven for 7" $ do
      digitToWord 7 `shouldBe` "seven"
    it "returns four for 4" $ do
      digitToWord 8 `shouldBe` "eight"
    it "returns nine for 9" $ do
      digitToWord 9 `shouldBe` "nine"
    it "returns invalid for anything else" $ do
      digitToWord (-1) `shouldBe` "invalid"
    it "returns invalid for anything else" $ do
      digitToWord 10 `shouldBe` "invalid"
    it "returns invalid for anything else" $ do
      digitToWord 1020 `shouldBe` "invalid"

  describe "digits" $ do
    it "returns [0] for 0" $ do
      digits 0 `shouldBe` [0]
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1,0] for 10" $ do
      digits 10 `shouldBe` [1,0]
    it "returns [1,0,3,1,2] for 10312" $ do
      digits 10312 `shouldBe` [1,0,3,1,2]
    it "returns [9,0,3,1,2] for 90312" $ do
      digits 90312 `shouldBe` [9,0,3,1,2]

  describe "wordNumber" $ do
    it "returns one-zero-one for 101" $ do
      wordNumber 101 `shouldBe` "one-zero-one"
    it "returns nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
