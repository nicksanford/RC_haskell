import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import BEncode
import Data.Map as M

-- data BEncode = BInteger Integer
--              | BString String
--              | BList [BEncode]
--              | BDict (M.Map BEncode BEncode)
--              deriving (Eq, Show, Ord)
instance Arbitrary BEncode where
  arbitrary = do
    r <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    frequency [ (1, return $ BInteger r)
              , (1, return $ BString x)
              , (1, return $ BList y)
              , (1, return $ BDict z)
              ]


charsToMaybeInt_prop :: Positive Int -> Bool
charsToMaybeInt_prop (Positive xs) = (charsToMaybeInt stringifiedXs) == (Just xs)
  where stringifiedXs = show xs

main :: IO ()
main = hspec $ do
  describe "charsToMaybeInt" $ do
    it "follows the property that any number, when stringified is able to be reparsed" $ do
      property charsToMaybeInt_prop

  describe "decode" $ do
    -- describe "round trip property" $ do
    --   it "should decode any valid encoded BEncode datatype" $ do
    describe "strings" $ do
      it "can parse an empty string" $ do
        decode "0:" `shouldBe` Run  "" (Just (BString ""))
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
      it "can parse an empty lists" $ do
        decode "le" `shouldBe` Run "" (Just (BList []))
      it "parses l4:spam4:eggse into BList [BString \"spam\", BString \"eggs\"]" $ do
        decode "l4:spam4:eggse" `shouldBe` Run "" (Just (BList [BString "spam", BString "eggs"]))
      it "parses invalid lists to nothing with the rest maintained" $ do
        decode "l4:spam4:eggsasdf" `shouldBe` Run "l4:spam4:eggsasdf" Nothing

    describe "dicts" $ do
      it "can parse an empty dict" $ do
        decode "de" `shouldBe` Run "" (Just (BDict $ M.fromList []))
      it "can parse a dict" $ do
        decode "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee" `shouldBe` Run "" (Just (BDict $ M.fromList $ [(BString "publisher",BString "bob"), (BString "publisher-webpage",BString "www.example.com"),(BString "publisher.location",BString "home")]))
      it "parses d3:cow3:moo4:spam4:eggse into BDict M.map [(\"cow\",\"moo\"), (\"spam\", \"eggs\")]" $ do
        decode "d3:cow3:moo4:spam4:eggse" `shouldBe` Run "" (Just $ BDict $ M.fromList [(BString "cow", BString "moo"), (BString "spam", BString "eggs")])
      it "parses lists in dicts" $ do
        decode "d4:spaml1:a1:bee" `shouldBe` Run "" (Just $ BDict $ M.fromList [(BString "spam", BList [BString "a", BString "b"])])

