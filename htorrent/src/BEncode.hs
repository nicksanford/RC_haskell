{-# LANGUAGE OverloadedStrings #-}
module BEncode where

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.List (unfoldr, sortOn)
  
-- https://en.wikipedia.org/wiki/Bencode
-- NOTE: According to the unofficial wiki, dictionaries can only have strings as their keys, however this should handle that case, and I believe the fact that this type is able to represent potentially invalid bencode binaries shouldn't be a problem in practice: https://wiki.theory.org/index.php/BitTorrentSpecification
-- Will refactor if I find that to not be the case.
data BEncode = BInteger Integer
             | BString BS.ByteString
             | BList [BEncode]
             | BDict (M.Map BEncode BEncode)
             deriving (Eq, Show, Ord)

type UnparsedContent = BS.ByteString
data Run a = Run UnparsedContent (Maybe a) deriving (Eq, Show)


digitToI = digitToI' . BS.singleton

digitToI' :: BS.ByteString -> Maybe Int
digitToI' "0" = Just 0
digitToI' "1" = Just 1
digitToI' "2" = Just 2
digitToI' "3" = Just 3
digitToI' "4" = Just 4
digitToI' "5" = Just 5
digitToI' "6" = Just 6
digitToI' "7" = Just 7
digitToI' "8" = Just 8
digitToI' "9" = Just 9
digitToI' _   = Nothing

maybeReadBencode :: String -> IO (Either BS.ByteString BEncode)
maybeReadBencode filePath = do
  xs <- BS.readFile filePath
  -- NOTE: Done this way b/c UTF8.toString ends up throwing an error, I think the solution for this is to change the module to just use byte strings
  return $ case decode xs of
    Run "" (Just x) -> Right x
    Run unparsed _ -> Left $ BS.concat ["ERROR: Hit a BEncode parse error, stopped parsing at", unparsed]

-- letterToEmpty :: Char -> Maybe BEncode
-- letterToEmpty 'd' = Just $ BDict M.empty
-- letterToEmpty 'l' = Just $ BList []
-- letterToEmpty 'i' = Just $ BInteger 0
-- letterToEmpty x = if isNum x
--                   then Just $ BString ""
--                   else Nothing
-- isNum :: Char -> Bool
-- isNum = (`BS.elem` ("0123456789"::BS.ByteString))


makeDict :: BS.ByteString -> Maybe ((Run BEncode, Run BEncode), BS.ByteString)
makeDict xs = (BS.uncons xs) >>= (\(x, rest) -> if isEnd x then Nothing else makeDict' xs)

makeDict' :: BS.ByteString -> Maybe ((Run BEncode, Run BEncode), BS.ByteString)
makeDict' string = if isNothing maybeBencode2
                    then Nothing
                    else Just ((Run rest1 maybeBencode1, Run rest2 maybeBencode2), rest2)
    where (Run rest1 maybeBencode1) = decode string
          (Run rest2 maybeBencode2) = decode rest1

unfoldList :: BS.ByteString -> Maybe (Run BEncode, BS.ByteString)-- -> [Run BEncode]
unfoldList string = if beingsWithEndChar || isNothing maybeBencode
                    then Nothing
                    else Just (Run rest maybeBencode, rest)
    where (Run rest maybeBencode) = decode string
          beingsWithEndChar = fromMaybe True (BS.uncons string >>= (\(x,_) -> return $ BS.singleton x == "e"))

encode :: BEncode -> BS.ByteString
encode (BInteger i) = BS.concat ["i", UTF8.fromString $ show i, "e"]
encode (BString s) = BS.concat [UTF8.fromString $ show $ BS.length s, ":", s]
encode (BList s) = BS.concat ["l", BS.concat [encode x | x <- s] , "e"]
encode (BDict d) = BS.concat ["d", go , "e"]
  where go = BS.concat $ fmap encodeedTuples $ sortOn fst $ M.toList d
        encodeedTuples :: (BEncode, BEncode) -> BS.ByteString
        encodeedTuples xs = BS.concat [(encode . fst $ xs), (encode . snd $ xs)]

data BencodeParseType = PDict | PList | PInt | PStr deriving (Eq, Show)
decode :: BS.ByteString ->  Run BEncode
decode xs =
  fromMaybe (Run xs Nothing) $ BS.uncons xs >>= handleUncons
  where handleUncons (word, rest) = case BS.singleton word of
          "d" -> return $ decodeType PDict rest
          "l" -> return $ decodeType PList rest
          "i" -> return $ decodeType PInt rest
          _   -> return $ decodeType PStr $ BS.cons word rest

decodeType :: BencodeParseType -> BS.ByteString ->  Run BEncode
decodeType PDict xs =
  case (unfold, isNothing maybeRest) of
    ([], _) -> Run "" dict
    (_, False) -> Run (fromJust maybeRest) dict
    _          -> Run (BS.concat ["l", xs]) Nothing
  where unfold :: [(Run BEncode, Run BEncode)]
        unfold = unfoldr makeDict xs
        dict = if any (\(x, y) -> isNothing x || isNothing y) maybeList
               then Nothing
               else Just (BDict (M.fromList $ fmap (\(x,y) -> (fromJust x, fromJust y)) maybeList))
        maybeList = fmap (\(Run _ bencode1, Run _ bencode2) -> (bencode1, bencode2)) unfold
        maybeRest =  (maybeHead (reverse unfold)) >>= restToMaybe
        restToMaybe :: (Run BEncode, Run BEncode) -> Maybe BS.ByteString
        restToMaybe (_, (Run (rs) _)) = (BS.uncons rs) >>= (\(r, rest) -> if isEnd r then Just rest else Nothing)

decodeType PList xs =
  case (unfold, isNothing maybeRest) of
    ([], _) -> Run "" list
    (_, False) -> Run (fromJust maybeRest) list
    _ -> Run (BS.concat ["l", xs]) Nothing
  where unfold = unfoldr unfoldList xs
        list = fmap BList $ if any isNothing maybeList
                            then Nothing
                            else Just (fmap fromJust maybeList)
        maybeList = fmap (\(Run _ bencode) -> bencode ) unfold
        maybeRest =  maybeHead (reverse unfold) >>= (\(Run rest _) -> BS.uncons rest >>= (\(r, rs) -> if isEnd r then Just rs else Nothing))

decodeType PInt xs = parseInt xs

decodeType PStr xs = parseString xs

parseInt :: BS.ByteString -> Run BEncode
parseInt (xs) = fromMaybe (Run xs Nothing) (BS.uncons xs >>= handleUncons)
  where
        handleUncons (first, rest) = if BS.singleton first == "-"
                                            then return $ handleNegative rest
                                            else return $ handlePositive xs
        handleNegative :: BS.ByteString -> Run BEncode
        handleNegative xs =  case handlePositive xs of
          Run rest (Just (BInteger x)) -> Run rest (Just (BInteger (negate x)))
          returnValue -> returnValue
        handlePositive :: BS.ByteString -> Run BEncode
        handlePositive xs = Run rest maybeBInteger
          where maybeBInteger = fmap (BInteger . fromIntegral) $ charsToMaybeInt $ BS.takeWhile isNotEnd xs
                rest = BS.tail $ BS.dropWhile isNotEnd xs

isNotEnd = not . isEnd

isEnd = (== "e") . BS.singleton

parseString :: BS.ByteString -> Run BEncode
parseString xs =
  case probablyInt of
    Nothing -> Run xs Nothing
    (Just i) -> Run  (restOfString i) (Just (BString $ string i))
  where afterNumber :: BS.ByteString
        afterNumber = BS.tail $ BS.dropWhile isNotSeparator xs
        probablyInt :: Maybe Int
        probablyInt = charsToMaybeInt $ BS.takeWhile isNotSeparator xs
        string i = BS.take i afterNumber
        restOfString i = BS.drop i afterNumber
        isNotSeparator = (/= ":") . BS.singleton


maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

charsToMaybeInt :: BS.ByteString -> Maybe Int
charsToMaybeInt = BS.foldl' charsToMaybeIntFold (Just 0)
  where charsToMaybeIntFold Nothing _ = Nothing
        charsToMaybeIntFold (Just sumInt) word = (+) (sumInt * 10) <$> digitToI word
