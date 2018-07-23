import Data.Monoid 
import Control.Applicative

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import BadMonoid
import Apl1


type SSI = (String, String, Int)

ziplistTrigger :: [SSI]
ziplistTrigger = undefined

listTrigger :: List SSI
listTrigger = undefined

myZipListTrigger :: ZipList' (List String, List String, List Int)
myZipListTrigger = undefined

myValidationTrigger :: Validation [String] (String, String, String)
myValidationTrigger = undefined

myPairTrigger :: Pair ((Int, Int), (String, String), (Bool, Bool))
myPairTrigger = undefined

myTwoTrigger :: Two String (Int, Double, String)
myTwoTrigger = undefined

myThreeTrigger :: Three String (Sum Int) (Int, Double, String)
myThreeTrigger = undefined

myThree'Trigger :: Three' String (Int, Double, String)
myThree'Trigger = undefined

myFourTrigger :: Four String (Sum Integer) (Product Integer) (Int, Double, String)
myFourTrigger = undefined

myFour'Trigger :: Four' String (Int, Double, String)
myFour'Trigger = undefined

toL :: [a] -> List a
toL = foldr Cons Nil 

main :: IO ()
main = hspec $ do
  -- NOTE These are commented out in order to speed up tests
  --quickBatch (monoid Twoo)
  --quickBatch (applicative ziplistTrigger)
--  describe "my ZipList helloworld example" $ do
--    it "follows the monoid laws" $ do
--      quickBatch $ monoid (ZipList [1 :: Sum Int])
  -- describe "my List applicative instance" $ do
  --   it "passes basic app check" $ do
  --      Cons (+1) (Cons (*2) Nil) <*> Cons 1 (Cons 2 Nil) `shouldBe` Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
  --   it "follows applicative laws" $ do
  --     quickBatch $ applicative listTrigger
  --   it "follows applicative laws" $ do
  --     quickBatch $ applicative ziplistTrigger
  -- describe "myZiplist applicative instance" $ do
  --   it "passes basic app checks" $ do
  --     (ZipList' $ toL [(+9), (*2), (+8)]) <*> (ZipList' $ toL [1..3]) `shouldBe` ( ZipList' $ toL [10,4,11])
  --   it "passes basic app checks" $ do
  --     (ZipList' $ toL [(+9), (*2), (+8)]) <*> (ZipList' $ toL $ repeat 1) `shouldBe` ( ZipList' $ toL [10,2,9])
  --   it "follows applicative laws" $ do
  --     quickBatch $ applicative myZipListTrigger
  -- describe "myValidation applicative instance" $ do
  --   it "follows applicative laws" $ do
  --     quickBatch $ applicative myValidationTrigger
  --   it "passes basic checks" $ do
  --     ((Success' (+1)) <*> (Success' 1)) `shouldBe` ((Success' 2):: Validation String Int)
  --   it "passes basic checks" $ do
  --     ((Success' (+1)) <*> (Failure' ["StackOverflow"])) `shouldBe` ((Failure' ["StackOverflow"]):: Validation [String] Int)
  --   it "passes basic checks" $ do
  --     ((Failure' ["StackOverflow"]) <*> (Success' (+1))) `shouldBe`  ((Failure' ["StackOverflow"]):: Validation [String] Int)
  --   it "passes basic checks" $ do
  --     ((Failure' ["StackOverflow"]) <*> (Failure' ["OutOfMemory"])) `shouldBe`  ((Failure' ["StackOverflow", "OutOfMemory"]):: Validation [String] Int)
  describe "pair applicative instance" $ do
    it "follows applicative laws" $ do
      quickBatch $ applicative myPairTrigger
  describe "two applicative instance" $ do
    it "follows applicative laws" $ do
      quickBatch $ applicative myTwoTrigger
  describe "three applicative instance" $ do
    it "follows applicative laws" $ do
      quickBatch $ applicative myThreeTrigger
  describe "three' applicative instance" $ do
    it "follows applicative laws" $ do
      quickBatch $ applicative myThree'Trigger
  describe "four applicative instance" $ do
    it "follows applicative laws" $ do
      quickBatch $ applicative myFourTrigger
  describe "four' applicative instance" $ do
    it "follows applicative laws" $ do
      quickBatch $ applicative myFour'Trigger
