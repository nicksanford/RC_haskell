{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import BEncode
import qualified Tracker as T
import qualified FileManager as FM
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Set as S
import qualified Data.List as L

--(fromIntegral $ length $ BS.unpack pieces ) / 20 => 1146.0

--BitField "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\192"

-- drop if not correct length
-- drop if spare bits are set
-- determine if it has all pieces
-- Return list of tuples / ordered dict with keys as piece hashes and values as booleans as to whether or not the client has them
-- Maintain the same datastructure for yourself (regarding whether or not you have the pieces)

exampleBitfield = [0,0,0,145,5,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,192]
-- This was received in response to an initiate handshake. It contains both the handshake response, the bitfield, a have message, 
p = [19,66,105,116,84,111,114,114,101,110,116,32,112,114,111,116,111,99,111,108,0,0,0,0,0,24,0,5,86,150,229,87,40,196,12,146,156,208,144,115,165,194,99,129,132,252,224,24,45,113,66,52,49,50,48,45,126,98,106,73,85,57,99,74,122,49,56,99,0,0,0,145,5,255,239,255,255,191,255,255,255,255,247,255,255,255,255,255,239,253,239,255,255,255,255,255,223,255,223,255,255,255,127,239,255,251,255,255,255,255,251,223,253,255,255,191,127,255,127,255,175,255,255,255,255,127,223,255,255,255,255,255,127,255,255,255,255,255,255,255,253,255,255,251,255,255,247,253,247,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,223,255,255,255,235,255,255,255,255,255,255,159,255,223,255,123,255,255,255,191,251,255,255,247,251,255,247,255,255,255,239,255,251,253,255,255,191,191,223,255,255,255,255,223,255,255,255,191,247,255,255,251,192,0,0,0,5,4,0,0,0,11,0,0,0,5,4,0,0,0,33,0,0,0,5,4,0,0,0,76,0,0,0,5,4,0,0,0,123,0,0,0,5,4,0,0,0,134,0,0,0,5,4,0,0,0,139,0,0,0,5,4,0,0,0,186,0,0,0,5,4,0,0,0,202,0,0,0,5,4,0,0,0,232,0,0,0,5,4,0,0,0,243,0,0,0,5,4,0,0,1,5,0,0,0,5,4,0,0,1,45,0,0,0,5,4,0,0,1,50,0,0,0,5,4,0,0,1,62,0,0,0,5,4,0,0,1,81,0,0,0,5,4,0,0,1,88,0,0,0,5,4,0,0,1,104,0,0,0,5,4,0,0,1,121,0,0,0,5,4,0,0,1,123,0,0,0,5,4,0,0,1,160,0,0,0,5,4,0,0,1,170,0,0,0,5,4,0,0,1,216,0,0,0,5,4,0,0,2,30,0,0,0,5,4,0,0,2,53,0,0,0,5,4,0,0,2,76,0,0,0,5,4,0,0,2,86,0,0,0,5,4,0,0,2,92,0,0,0,5,4,0,0,2,218,0,0,0,5,4,0,0,2,251,0,0,0,5,4,0,0,2,253,0,0,0,5,4,0,0,3,49,0,0,0,5,4,0,0,3,50,0,0,0,5,4,0,0,3,66,0,0,0,5,4,0,0,3,80,0,0,0,5,4,0,0,3,85,0,0,0,5,4,0,0,3,113,0,0,0,5,4,0,0,3,125,0,0,0,5,4,0,0,3,148,0,0,0,5,4,0,0,3,157,0,0,0,5,4,0,0,3,172,0,0,0,5,4,0,0,3,203,0,0,0,5,4,0,0,3,221,0,0,0,5,4,0,0,3,230,0,0,0,5,4,0,0,3,249,0,0,0,5,4,0,0,4,1,0,0,0,5,4,0,0,4,10,0,0,0,5,4,0,0,4,50,0,0,0,5,4,0,0,4,81,0,0,0,5,4,0,0,4,92,0,0,0,5,4,0,0,4,117,0,0,0,1,1]
--p = [19,66,105,116,84,111,114,114,101,110,116,32,112,114,111,116,111,99,111,108,0,0,0,0,0,24,0,5,86,150,229,87,40,196,12,146,156,208,144,115,165,194,99,129,132,252,224,24,45,113,66,52,49,50,48,45,126,98,106,73,85,57,99,74,122,49,56,99, --- 0,0,0,145,5,255,239,255,255,191,255,255,255,255,247,255,255,255,255,255,239,253,239,255,255,255,255,255,223,255,223,255,255,255,127,239,255,251,255,255,255,255,251,223,253,255,255,191,127,255,127,255,175,255,255,255,255,127,223,255,255,255,255,255,127,255,255,255,255,255,255,255,253,255,255,251,255,255,247,253,247,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,223,255,255,255,235,255,255,255,255,255,255,159,255,223,255,123,255,255,255,191,251,255,255,247,251,255,247,255,255,255,239,255,251,253,255,255,191,191,223,255,255,255,255,223,255,255,255,191,247,255,255,251,192, --- 0,0,0,5,4,0,0,0,11, --- 0,0,0,5,4,0,0,0,33, --- 0,0,0,5,4,0,0,0,76, -- 0,0,0,5,4,0,0,0,123, -- 0,0,0,5,4,0,0,0,134, -- 0,0,0,5,4,0,0,0,139, -- 0,0,0,5,4,0,0,0,186, -- 0,0,0,5,4,0,0,0,202, -- 0,0,0,5,4,0,0,0,232, -- 0,0,0,5,4,0,0,0,243, -- 0,0,0,5,4,0,0,1,5, -- 0,0,0,5,4,0,0,1,45, -- 0,0,0,5,4,0,0,1,50, -- 0,0,0,5,4,0,0,1,62, -- 0,0,0,5,4,0,0,1,81,0,0,0,5,4,0,0,1,88,0,0,0,5,4,0,0,1,104,0,0,0,5,4,0,0,1,121,0,0,0,5,4,0,0,1,123,0,0,0,5,4,0,0,1,160,0,0,0,5,4,0,0,1,170,0,0,0,5,4,0,0,1,216,0,0,0,5,4,0,0,2,30,0,0,0,5,4,0,0,2,53,0,0,0,5,4,0,0,2,76,0,0,0,5,4,0,0,2,86,0,0,0,5,4,0,0,2,92,0,0,0,5,4,0,0,2,218,0,0,0,5,4,0,0,2,251,0,0,0,5,4,0,0,2,253,0,0,0,5,4,0,0,3,49,0,0,0,5,4,0,0,3,50,0,0,0,5,4,0,0,3,66,0,0,0,5,4,0,0,3,80,0,0,0,5,4,0,0,3,85,0,0,0,5,4,0,0,3,113,0,0,0,5,4,0,0,3,125,0,0,0,5,4,0,0,3,148,0,0,0,5,4,0,0,3,157,0,0,0,5,4,0,0,3,172,0,0,0,5,4,0,0,3,203,0,0,0,5,4,0,0,3,221,0,0,0,5,4,0,0,3,230,0,0,0,5,4,0,0,3,249,0,0,0,5,4,0,0,4,1,0,0,0,5,4,0,0,4,10,0,0,0,5,4,0,0,4,50,0,0,0,5,4,0,0,4,81,0,0,0,5,4,0,0,4,92,0,0,0,5,4,0,0,4,117,0,0,0,1,1]

poissibleResponse =  "\DC3BitTorrent protocol\NUL\NUL\NUL\NUL\NUL\CAN\NUL\ENQV\150\229W(\196\f\146\156\208\144s\165\194c\129\132\252\224\CAN-qB4120-~bjIU9cJz18c\NUL\NUL\NUL\145\ENQ\255\239\255\255\191\255\255\255\255\247\255\255\255\255\255\239\253\239\255\255\255\255\255\223\255\223\255\255\255\DEL\239\255\251\255\255\255\255\251\223\253\255\255\191\DEL\255\DEL\255\175\255\255\255\255\DEL\223\255\255\255\255\255\DEL\255\255\255\255\255\255\255\253\255\255\251\255\255\247\253\247\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\223\255\255\255\235\255\255\255\255\255\255\159\255\223\255{\255\255\255\191\251\255\255\247\251\255\247\255\255\255\239\255\251\253\255\255\191\191\223\255\255\255\255\223\255\255\255\191\247\255\255\251\192\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\NUL\v\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\NUL!\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\NULL\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\NUL{\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\NUL\134\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\NUL\139\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\NUL\186\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\NUL\202\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\NUL\232\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\NUL\243\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOH\ENQ\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOH-\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOH2\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOH>\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOHQ\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOHX\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOHh\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOHy\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOH{\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOH\160\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOH\170\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\SOH\216\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\STX\RS\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\STX5\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\STXL\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\STXV\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\STX\\\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\STX\218\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\STX\251\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\STX\253\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETX1\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETX2\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETXB\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETXP\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETXU\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETXq\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETX}\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETX\148\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETX\157\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETX\172\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETX\203\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETX\221\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETX\230\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\ETX\249\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\EOT\SOH\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\EOT\n\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\EOT2\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\EOTQ\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\EOT\\\NUL\NUL\NUL\ENQ\EOT\NUL\NUL\EOTu\NUL\NUL\NUL\SOH\SOH"



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



  -- describe "encode" $ do
  --   it "has the round-trip property with decode" $ do
  --       quickCheck encodeDecodeRoundTrip_prop

  describe "getRequestList" $ do
    it "when all lengths are summed up it should equal the length of the content" $ do
      Just tracker <- T.testTracker
      let T.SingleFileInfo (T.Name _) (T.Length totalLength) (T.MD5Sum _) = T.getTrackerSingleFileInfo tracker
      sum [len | FM.Request _ _ (FM.RequestLength len) <- FM.getRequestList tracker] `shouldBe` totalLength

      Just (T.Tracker p a pl ps ih (T.SingleFileInfo n (T.Length l) md5) mdi me) <- T.testTracker
      let newTracker = (T.Tracker p a pl ps ih (T.SingleFileInfo n (T.Length (l+3)) md5) mdi me)
      sum [len | FM.Request _ _ (FM.RequestLength len) <- FM.getRequestList newTracker] `shouldBe` l+3

      let newnewTracker = (T.Tracker p a pl ps ih (T.SingleFileInfo n (T.Length (l-3)) md5) mdi me)
      sum [len | FM.Request _ _ (FM.RequestLength len) <- FM.getRequestList newnewTracker] `shouldBe` l-3

    it "there should be no duplicate elements" $ do
      Just tracker <- T.testTracker
      (S.size $ S.fromList $ FM.getRequestList tracker) `shouldBe` (fromIntegral $ length $ FM.getRequestList tracker)

    it "the length should never exceed the blockSize" $ do
      Just tracker <- T.testTracker
      maximum [len | FM.Request _ _ (FM.RequestLength len) <- FM.getRequestList tracker] `shouldBe` FM.blockSize

    it "the length should never be smaller or equal to 0" $ do
      Just tracker <- T.testTracker
      minimum [len | FM.Request _ _ (FM.RequestLength len) <- FM.getRequestList tracker] `shouldSatisfy` (> 0)

    it "when grouped by pieceIndex, there should be the same number of pieces and the indexes should be the same as the piece indexes" $ do
      Just tracker <- T.testTracker
      let pieces = T.getTrackerPieces tracker
      let groupedRequestList = L.groupBy (\(FM.Request (FM.PieceIndex x) _ _) (FM.Request (FM.PieceIndex y) _ _) -> x == y) $ FM.getRequestList tracker
      length groupedRequestList `shouldBe` length pieces

    it "when grouped by pieceIndex, the indexes should be the same as the piece indexes" $ do
      Just tracker <- T.testTracker
      let pieces = T.getTrackerPieces tracker
      let rl = FM.getRequestList tracker
      let requestIndeciesSet = S.fromList $ fmap (\(FM.Request (FM.PieceIndex x) _ _) -> x) rl
      (S.size requestIndeciesSet) `shouldBe` (fromIntegral $ length pieces)

    it "still works if the total length is not a power of 2 above" $ do
      Just (T.Tracker p a pl ps ih (T.SingleFileInfo n (T.Length l) md5) mdi me) <- T.testTracker
      let newTracker = (T.Tracker p a pl ps ih (T.SingleFileInfo n (T.Length (l+3)) md5) mdi me)
      let pieces = T.getTrackerPieces newTracker
      let rl = FM.getRequestList newTracker
      let requestIndeciesSet = S.fromList $ fmap (\(FM.Request (FM.PieceIndex x) _ _) -> x) rl
      (S.size requestIndeciesSet) `shouldBe` (fromIntegral $ length pieces)

    it "still works if the total length is not a power of 2 below" $ do
      Just (T.Tracker p a pl ps ih (T.SingleFileInfo n (T.Length l) md5) mdi me) <- T.testTracker
      let newTracker = (T.Tracker p a pl ps ih (T.SingleFileInfo n (T.Length (l-3)) md5) mdi me)
      let pieces = T.getTrackerPieces newTracker
      let rl = FM.getRequestList newTracker
      let requestIndeciesSet = S.fromList $ fmap (\(FM.Request (FM.PieceIndex x) _ _) -> x) rl
      (S.size requestIndeciesSet) `shouldBe` (fromIntegral $ length pieces)
