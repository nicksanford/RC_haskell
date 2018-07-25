{-# LANGUAGE OverloadedStrings #-}
module BEncode where
import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.Maybe (isNothing, fromJust)
import Data.List (foldl', unfoldr)

-- https://en.wikipedia.org/wiki/Bencode
data BEncode = BInteger Integer
             | BString String
             | BList [BEncode]
             | BDict (M.Map BEncode BEncode)
             deriving (Eq, Show, Ord)

type UnparsedContent = String
data Run a = Run UnparsedContent (Maybe a) deriving (Eq, Show)

encode :: BEncode -> String
encode = undefined

--takeDrop :: (Char -> Bool) -> (String, String)
--takeDrop f xs = (takeWhile f xs, dropWhile f xs)

digitToI :: Char -> Maybe Int
digitToI '0' = Just 0
digitToI '1' = Just 1
digitToI '2' = Just 2
digitToI '3' = Just 3
digitToI '4' = Just 4
digitToI '5' = Just 5
digitToI '6' = Just 6
digitToI '7' = Just 7
digitToI '8' = Just 8
digitToI '9' = Just 9
digitToI _   = Nothing

bsfold :: (Char -> b -> b) -> b -> String -> b
bsfold f z ([]) = z
bsfold f z (x:xs) = f x $ bsfold f z xs

-- parse :: Char -> (String, String, BEncode)
-- parse 'd' (bsBuffer, refference, BEmpty) = ('d':bsBuffer, refference, BDict (M.fromList []))
-- parse 'd' (bsBuffer, refference, bencodeAcc) = ('d':bsBuffer, bencodeAcc)
-- parse 'i' (bsBuffer, refference, bencodeAcc)
-- parse 'l' (bsBuffer, refference, bencodeAcc)
-- parse _   (bsBuffer, refference, bencodeAcc)
--("", BEmpty)

--   | BS.uncons(bytes) == Just ('d', rest) = takeDrop (/= ':') rest
--   | BS.uncons(bytes) == Just ('i', rest) =
--   | BS.uncons(bytes) == Just ('l', rest) =
--   | otherwise =

-- test :: String -> IO (String)
-- test filePath = do
--   fmap decode $ readFile filePath
test :: String -> IO (Run BEncode)
test filePath = do
  xs <- readFile filePath
  return $ decode xs
  -- let flag = letterToEmpty x
  -- if isNothing flag
  -- then return Nothing
  -- else return $ decode xs (fromJust flag)

-- data BEncode = BInteger Integer
--              | BString String
--              | BList [BEncode]
--              | BDict (M.Map BEncode BEncode)
--              deriving (Eq, Show, Ord)

letterToEmpty :: Char -> Maybe BEncode
letterToEmpty 'd' = Just $ BDict M.empty
letterToEmpty 'l' = Just $ BList []
letterToEmpty 'i' = Just $ BInteger 0
letterToEmpty x = if isNum x
                  then Just $ BString ""
                  else Nothing
isNum :: Char -> Bool
isNum = (`elem` ("0123456789"::String))

parseInt :: String -> Run BEncode
parseInt ('-':xs) =
  case parseInt xs of
    (Run rest (Just (BInteger x))) -> (Run rest (Just (BInteger (negate x))))
    returnValue -> returnValue
parseInt (xs) = Run rest maybeBInteger
  where maybeBInteger = fmap (BInteger . fromIntegral) $ charsToMaybeInt $ takeWhile (/= 'e') xs
        rest = dropWhile (== 'e') $ dropWhile (/= 'e') xs

parseString :: String -> Run BEncode
parseString xs =
  case probablyInt of
    Nothing -> Run xs Nothing
    (Just i) -> Run  (restOfString i) (Just (BString $ string i))
  where afterNumber :: String
        afterNumber = dropWhile (== ':') $ dropWhile (/= ':') xs
        probablyInt :: Maybe Int
        probablyInt = charsToMaybeInt $ takeWhile (/= ':') xs
        string i = take i afterNumber
        restOfString i = drop i afterNumber
        
makeDict :: String -> Maybe ((Run BEncode, Run BEncode), String)
makeDict ('e':_) = Nothing
makeDict string = if isNothing maybeBencode2
                    then Nothing
                    else Just (((Run rest1 maybeBencode1), (Run rest2 maybeBencode2)), rest2)
    where (Run rest1 maybeBencode1) = decode string
          (Run rest2 maybeBencode2) = decode rest1

unfoldList :: String -> Maybe (Run BEncode, String)-- -> [Run BEncode]
unfoldList ('e':_) = Nothing
unfoldList string = if isNothing maybeBencode
                    then Nothing
                    else Just ((Run rest maybeBencode), rest)
    where (Run rest maybeBencode) = decode string
-- NOTE It seems like it would be good to be able to know where the computation failed so that we can fix the file we are parsing.
decode :: String ->  Run BEncode
decode ('d':xs) = if isNothing maybeRest
                  then Run ('l':xs) Nothing
                  else Run (fromJust maybeRest) dict
  where unfold :: [(Run BEncode, Run BEncode)]
        unfold = unfoldr makeDict xs
        dict = if any (\(x, y) -> isNothing x || isNothing y) maybeList
               then Nothing
               else Just (BDict (M.fromList $ fmap (\(x,y) -> (fromJust x, fromJust y)) maybeList))
        maybeList = fmap (\((Run _ bencode1), (Run _ bencode2)) -> (bencode1, bencode2)) unfold
        maybeRest =  (maybeTail $ reverse unfold) >>= maybeHead  >>= restToMaybe
        restToMaybe :: (Run BEncode, Run BEncode) -> Maybe String
        restToMaybe (_, (Run ('e':rs) _)) = Just rs
        restToMaybe _ = Nothing
        
decode ('l':xs) = if isNothing maybeRest
                  then Run ('l':xs) Nothing
                  else Run (fromJust maybeRest) list
  where unfold = unfoldr unfoldList xs
        list = fmap BList $ if any isNothing maybeList
                            then Nothing
                            else (Just (fmap fromJust maybeList))
        maybeList = fmap (\(Run _ bencode) -> bencode ) unfold
        maybeRest =  (maybeHead $ reverse unfold) >>= (\(Run (r:rs) _) -> if r == 'e' then Just rs else Nothing)                  -- case decode xs of

decode ('i':xs) = parseInt xs
decode xs = parseString xs

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_:xs) = Just xs

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

-- decode :: String -> BEncode ->  RunBEncode
-- decode (xs) (BDict ys) = foldr makeDict (Just (BDict ys)) xs
-- -- decode ('l':xs)
-- -- decode ('i':xs)
-- decode (xs) = 
--   where (string, rest) = getStringAndRest xs

--isNum = (`elem` "0123456789")
-- decode :: String -> Maybe BEncode
-- decode xs = snd $ foldr parse ("", xs, BEmpty) xs


charsToMaybeInt :: String -> Maybe Int
charsToMaybeInt = foldl' charsToMaybeIntFold (Just 0)
  where charsToMaybeIntFold :: Maybe Int -> Char -> Maybe  Int
        charsToMaybeIntFold Nothing _ = Nothing
        charsToMaybeIntFold (Just sumInt) char = (+) (sumInt * 10) <$> digitToI char
        
