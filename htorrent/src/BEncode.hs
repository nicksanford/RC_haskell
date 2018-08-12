{-# LANGUAGE OverloadedStrings #-}
module BEncode where

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
--import qualified Data.ByteString.UTF8 as UTF8
import Crypto.Hash
import Crypto.Random
import Data.Maybe (isNothing, fromJust, isJust)
import Data.List (foldl', unfoldr, sortOn)
  
-- https://en.wikipedia.org/wiki/Bencode
-- NOTE: According to the unofficial wiki, dictionaries can only have strings as their keys, however this should handle that case, and I believe the fact that this type is able to represent potentially invalid bencode binaries shouldn't be a problem in practice: https://wiki.theory.org/index.php/BitTorrentSpecification
-- Will refactor if I find that to not be the case.
data BEncode = BInteger Integer
             | BString String
             | BList [BEncode]
             | BDict (M.Map BEncode BEncode)
             deriving (Eq, Show, Ord)

type UnparsedContent = String
data Run a = Run UnparsedContent (Maybe a) deriving (Eq, Show)


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

maybeReadBencode :: String -> IO (Maybe BEncode)
maybeReadBencode filePath = do
  xs <- BS.readFile filePath
  -- NOTE: Done this way b/c UTF8.toString ends up throwing an error, I think the solution for this is to change the module to just use bytestrings
  return $ case decode $ Char8.unpack xs of
    Run "" (Just x) -> Just x
    _ -> Nothing

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
    Run rest (Just (BInteger x)) -> Run rest (Just (BInteger (negate x)))
    returnValue -> returnValue
parseInt xs = Run rest maybeBInteger
  where maybeBInteger = fmap (BInteger . fromIntegral) $ charsToMaybeInt $ takeWhile (/= 'e') xs
        rest = tail $ dropWhile (/= 'e') xs

parseString :: String -> Run BEncode
parseString xs =
  case probablyInt of
    Nothing -> Run xs Nothing
    (Just i) -> Run  (restOfString i) (Just (BString $ string i))
  where afterNumber :: String
        afterNumber = tail $ dropWhile (/= ':') xs
        probablyInt :: Maybe Int
        probablyInt = charsToMaybeInt $ takeWhile (/= ':') xs
        string i = take i afterNumber
        restOfString i = drop i afterNumber

makeDict :: String -> Maybe ((Run BEncode, Run BEncode), String)
makeDict ('e':_) = Nothing
makeDict string = if isNothing maybeBencode2
                    then Nothing
                    else Just ((Run rest1 maybeBencode1, Run rest2 maybeBencode2), rest2)
    where (Run rest1 maybeBencode1) = decode string
          (Run rest2 maybeBencode2) = decode rest1

unfoldList :: String -> Maybe (Run BEncode, String)-- -> [Run BEncode]
unfoldList ('e':_) = Nothing
unfoldList string = if isNothing maybeBencode
                    then Nothing
                    else Just (Run rest maybeBencode, rest)
    where (Run rest maybeBencode) = decode string

encode :: BEncode -> String
encode (BInteger i) = concat ["i", show i, "e"]
encode (BString s) = concat [show $ length s, ":", s]
encode (BList s) = concat ["l", s >>= encode , "e"]
encode (BDict d) = concat ["d", go , "e"]
  where go = concatMap encodeedTuples $ sortOn fst $ M.toList d
        encodeedTuples :: (BEncode, BEncode) -> String
        encodeedTuples = (encode . fst) <> (encode . snd)

decode :: String ->  Run BEncode
decode ('d':xs) =
  case (unfold, isNothing maybeRest) of
    ([], _) -> Run "" dict
    (_, False) -> Run (fromJust maybeRest) dict
    _          -> Run ('l':xs) Nothing
  where unfold :: [(Run BEncode, Run BEncode)]
        unfold = unfoldr makeDict xs
        dict = if any (\(x, y) -> isNothing x || isNothing y) maybeList
               then Nothing
               else Just (BDict (M.fromList $ fmap (\(x,y) -> (fromJust x, fromJust y)) maybeList))
        maybeList = fmap (\(Run _ bencode1, Run _ bencode2) -> (bencode1, bencode2)) unfold
        maybeRest =  (maybeHead (reverse unfold)) >>= restToMaybe
        restToMaybe :: (Run BEncode, Run BEncode) -> Maybe String
        restToMaybe (_, (Run ('e':rs) _)) = Just rs
        restToMaybe _ = Nothing

decode ('l':xs) =
  case (unfold, isNothing maybeRest) of
    ([], _) -> Run "" list
    (_, False) -> Run (fromJust maybeRest) list
    _ -> Run ('l':xs) Nothing
  where unfold = unfoldr unfoldList xs
        list = fmap BList $ if any isNothing maybeList
                            then Nothing
                            else Just (fmap fromJust maybeList)
        maybeList = fmap (\(Run _ bencode) -> bencode ) unfold
        maybeRest =  maybeHead (reverse unfold) >>= (\(Run (r:rs) _) -> if r == 'e' then Just rs else Nothing)

decode ('i':xs) = parseInt xs
decode xs = parseString xs

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

charsToMaybeInt :: String -> Maybe Int
charsToMaybeInt = foldl' charsToMaybeIntFold (Just 0)
  where charsToMaybeIntFold :: Maybe Int -> Char -> Maybe  Int
        charsToMaybeIntFold Nothing _ = Nothing
        charsToMaybeIntFold (Just sumInt) char = (+) (sumInt * 10) <$> digitToI char
