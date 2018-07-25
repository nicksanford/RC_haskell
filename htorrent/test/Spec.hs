import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import BEncode
import Data.Map as M

charsToMaybeInt_prop :: Positive Int -> Bool
charsToMaybeInt_prop (Positive xs) = (charsToMaybeInt stringifiedXs) == (Just xs)
  where stringifiedXs = show xs

main :: IO ()
main = hspec $ do
  describe "charsToMaybeInt" $ do
    it "follows the property that any number, when stringified is able to be reparsed" $ do
      property charsToMaybeInt_prop
  describe "decode" $ do
    describe "strings" $ do
      it "returns a BString with no unparsed data if given a well formed string" $ do
        decode "5:hello" `shouldBe` Run  "" (Just (BString "hello"))
      it "returns a BString with no parsed data if given a malformed string" $ do
        decode "5:helloasdf" `shouldBe` Run "asdf" (Just (BString "hello"))
    describe "integers" $ do
      it "returns a BInteger with no unparsed data if given a well formed integer" $ do
        decode "i9001e" `shouldBe` Run "" (Just (BInteger 9001))
      it "returns a BInteger with parsed data if given a malformed integer" $ do
        decode "i9001easdf" `shouldBe` Run "asdf" (Just (BInteger 9001))
      it "returns a BInteger with no unparsed data if given a well formed negative integer" $ do
        decode "i-9001e" `shouldBe` Run "" (Just (BInteger (-9001)))
    describe "lists" $ do
      it "parses l4:spam4:eggse into BList [BString \"spam\", BString \"eggs\"]" $ do
        decode "l4:spam4:eggse" `shouldBe` Run "" (Just (BList [BString "spam", BString "eggs"]))
      it "parses invalid lists to nothing with the rest maintained" $ do
        decode "l4:spam4:eggsasdf" `shouldBe` Run "l4:spam4:eggsasdf" Nothing
      it "parses d3:cow3:moo4:spam4:eggse into BDict M.map [(\"cow\",\"moo\"), (\"spam\", \"eggs\")]" $ do
        decode "d3:cow3:moo4:spam4:eggse" `shouldBe` Run "" (Just $ BDict $ M.fromList [(BString "cow", BString "moo"), (BString "spam", BString "eggs")])
