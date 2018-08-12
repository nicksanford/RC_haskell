{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import BEncode
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

-- data BEncode = BInteger Integer
--              | BString String
--              | BList [BEncode]
--              | BDict (M.Map BEncode BEncode)
--              deriving (Eq, Show, Ord)
sizedBencode :: Int -> Gen BEncode
sizedBencode c
  | c <= 0 = do
    i <- arbitrary
    s <- fmap UTF8.fromString arbitrary
    elements [ BInteger i, BString s ]
  | c <= 10 = do
    ls <- vectorOf 2 $ sizedBencode (c - 1)
    ks <- vectorOf 2 (fmap UTF8.fromString arbitrary)
    elements [ BList ls
             , BDict (M.fromList $ zipWith (\k v -> (BString k, v)) ks ls) ]
  | otherwise = sizedBencode 10

instance Arbitrary BEncode where
  arbitrary = sized sizedBencode

charsToMaybeInt_prop :: Positive Int -> Bool
charsToMaybeInt_prop (Positive x) = (charsToMaybeInt stringifiedXs) == (Just x)
  where stringifiedXs = UTF8.fromString $ show x

encodeDecodeRoundTrip_prop :: BEncode -> Bool
encodeDecodeRoundTrip_prop bencode = bencode == ((\(Run "" (Just x)) -> x) . decode . encode $ bencode)

main :: IO ()
main = hspec $ do

  describe "decode" $ do
    describe "charsToMaybeInt_prop" $ do
      it "it to have the round trip property" $ do
        quickCheck charsToMaybeInt_prop

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
      it "can parse li4ee" $ do
        decode "li4ee" `shouldBe` Run "" (Just $ BList [BInteger 4])

    describe "dicts" $ do
      it "can parse an empty dict" $ do
        decode "de" `shouldBe` Run "" (Just (BDict $ M.fromList []))
      it "can parse a dict" $ do
        decode "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee" `shouldBe` Run "" (Just (BDict $ M.fromList $ [(BString "publisher",BString "bob"), (BString "publisher-webpage",BString "www.example.com"),(BString "publisher.location",BString "home")]))
      it "parses d3:cow3:moo4:spam4:eggse into BDict M.map [(\"cow\",\"moo\"), (\"spam\", \"eggs\")]" $ do
        decode "d3:cow3:moo4:spam4:eggse" `shouldBe` Run "" (Just $ BDict $ M.fromList [(BString "cow", BString "moo"), (BString "spam", BString "eggs")])
      it "parses lists in dicts" $ do
        decode "d4:spaml1:a1:bee" `shouldBe` Run "" (Just $ BDict $ M.fromList [(BString "spam", BList [BString "a", BString "b"])])

  describe "encode" $ do
    it "has the round-trip property with encode" $ do
        quickCheck encodeDecodeRoundTrip_prop
