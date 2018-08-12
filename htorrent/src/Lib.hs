{-# LANGUAGE OverloadedStrings #-}
module Lib where

import BEncode

import Network.Wreq
import qualified Network.HTTP.Simple as HTTP
import Control.Lens

import Data.List
import qualified  System.Random as R
  
--import qualified Data.Byteable as Byteable

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString.Char8 as Char8

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.Map as M

import qualified Crypto.Hash as C

import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE

import Data.Maybe (isNothing, fromJust, isJust)
  
--import Control.Monad (join)
import Crypto.Random (getRandomBytes)

--import qualified Network.URI as URI

newtype Announce e = Announce e deriving (Eq, Show)
newtype Name e = Name e deriving (Eq, Show)
newtype Length e = Length e deriving (Eq, Show)
newtype Path e = Path e deriving (Eq, Show)
newtype MD5Sum e = MD5Sum e deriving (Eq, Show)
newtype Files e = Files e deriving (Eq, Show)
newtype Encoding e = Encoding e deriving (Eq, Show)
newtype PieceLength e = PieceLength e deriving (Eq, Show)
newtype Pieces e = Pieces e deriving (Eq, Show)
newtype InfoHash e = InfoHash e deriving (Eq, Show)

data SingleFileInfo = SingleFileInfo (Name BS.ByteString) (Length Integer) (MD5Sum (Maybe BS.ByteString)) deriving (Eq, Show)
data DirectoryFile = DirectoryFile (Path BS.ByteString) (Length Integer) (MD5Sum (Maybe BS.ByteString)) deriving (Eq, Show)
data DirectoryInfo = DirectoryInfo (Name BS.ByteString) (Files [DirectoryFile]) deriving (Eq, Show)

data Tracker = Tracker (Announce BS.ByteString) (PieceLength Integer) (Pieces BS.ByteString) (InfoHash BS.ByteString) (Maybe SingleFileInfo) (Maybe DirectoryInfo) (Maybe (Encoding BS.ByteString)) deriving (Eq, Show)

getSingleFileLength :: SingleFileInfo -> Integer
getSingleFileLength (SingleFileInfo _ (Length l) _) = l

getDirectoryFileLength :: DirectoryFile -> Integer
getDirectoryFileLength (DirectoryFile _ (Length l) _) = l

getDirectoryInfoFiles :: DirectoryInfo -> [DirectoryFile]
getDirectoryInfoFiles (DirectoryInfo _ (Files xs)) = xs

bencodeToMaybeString :: BEncode -> Maybe BS.ByteString
bencodeToMaybeString (BString a) = Just a
bencodeToMaybeString _           = Nothing

bencodeToMaybeInteger :: BEncode -> Maybe Integer
bencodeToMaybeInteger (BInteger a) = Just a
bencodeToMaybeInteger _           = Nothing

bencodeToMaybeDict :: BEncode -> Maybe (M.Map BEncode BEncode)
bencodeToMaybeDict (BDict a) = Just a
bencodeToMaybeDict _         = Nothing

bencodeToMaybeDirectoryFile :: BEncode -> Maybe [DirectoryFile]
bencodeToMaybeDirectoryFile (BList xs@(BDict _:_)) = traverse dictToMaybeDirectoryFile xs
bencodeToMaybeDirectoryFile _ = Nothing


dictToMaybeDirectoryFile :: BEncode -> Maybe DirectoryFile
dictToMaybeDirectoryFile (BDict x) = DirectoryFile <$> (Path <$> (M.lookup (BString "path") x >>= bencodeToMaybeString)) <*>
                                                       (Length <$> (M.lookup (BString "length") x >>= bencodeToMaybeInteger)) <*>
                                                       Just (MD5Sum $  M.lookup (BString "md5sum") x >>= bencodeToMaybeString)
dictToMaybeDirectoryFile _ = Nothing

toTracker :: BEncode -> Maybe Tracker
toTracker (BDict d) =  buildTracker <$> maybeAnnounce <*> maybePieceLength <*> maybePieces <*> maybeInfoHash >>= validateTracker
  where buildTracker :: Announce BS.ByteString -> PieceLength Integer -> Pieces BS.ByteString -> InfoHash BS.ByteString-> Tracker
        buildTracker a pl p i = Tracker a pl p i singleFileInfo directoryInfo maybeEncoding
        validateTracker :: Tracker -> Maybe Tracker
        validateTracker t@(Tracker _ _ _ _ sfi dfi _)
          | isNothing sfi && isNothing dfi = Nothing
          | otherwise = Just t
        l :: BEncode -> Maybe BEncode
        l x = M.lookup (BString "info") d >>= bencodeToMaybeDict >>= M.lookup x
        maybeInfoHash :: Maybe (InfoHash BS.ByteString)
        maybeInfoHash = InfoHash <$> (shaHash . encode) <$> (M.lookup (BString "info") d)
        maybeAnnounce :: Maybe (Announce BS.ByteString)
        maybeAnnounce =  Announce <$> (M.lookup (BString "announce") d >>= bencodeToMaybeString)
        singleFileInfo :: Maybe SingleFileInfo
        singleFileInfo = SingleFileInfo <$> (Name <$> (l (BString "name") >>= bencodeToMaybeString)) 
                                        <*> (Length <$> (l (BString "length") >>= bencodeToMaybeInteger))
                                        <*> Just (MD5Sum (l (BString "md5sum") >>= bencodeToMaybeString))
        directoryInfo :: Maybe DirectoryInfo
        directoryInfo = DirectoryInfo <$> (Name <$> (l (BString "name") >>= bencodeToMaybeString))
                                      <*> (Files <$> (l (BString "files") >>= bencodeToMaybeDirectoryFile))
        maybeEncoding :: Maybe (Encoding BS.ByteString)
        maybeEncoding = Encoding <$> (M.lookup (BString "encoding") d >>= bencodeToMaybeString)
        maybePieceLength :: Maybe (PieceLength Integer)
        maybePieceLength = PieceLength <$> (l (BString "piece length") >>= bencodeToMaybeInteger)
        maybePieces :: Maybe (Pieces BS.ByteString)
        maybePieces = Pieces <$> (l (BString "pieces") >>= bencodeToMaybeString)
toTracker _ = Nothing




  
        -- directoryInfo = String -> Files [DirectoryFile] -> DirectoryInfo
        -- buildDirectoryInfo :: DirectoryInfo
  --       port = 6881
  -- -- TODO: this should be the total number of bytes uploaded.
  --       uploaded = 0
  -- -- TODO: this should be the total number of bytes downloaded.
  --       downloaded = 0
 --        -- left = 

-- test = do
--   getMagnetLink "https://download.documentfoundation.org/libreoffice/stable/6.0.5/mac/x86_64/LibreOffice_6.0.5_MacOS_x86-64.dmg.magnet"

-- getMagnetLink :: String  -> IO LBS.ByteString
-- getMagnetLink link = do
--   r <- get link
--   let body = r ^.responseBody
--   return body


start :: String -> IO String
start filePath = do
  peer_id <- getPeerID
  maybeBencode <- maybeReadBencode filePath
  case maybeBencode >>= toTracker of
    Just tracker -> trackerRequest peer_id tracker
    Nothing -> return "ERROR: Hit an error"

randomBytes :: Int -> IO BS.ByteString
randomBytes  = getRandomBytes

unescape :: BS.ByteString -> BS.ByteString
unescape x = case fmap (\(a,b) -> (BS.singleton a, id b)) (BS.uncons x) of
  Nothing -> x
  (Just ("%", rest)) -> BS.concat [(BS.take 2 rest), unescape $ BS.drop 2 rest]
  -- figure out how to do the ord stuff
  (Just (first, rest)) -> BS.concat [(BS.take 2 rest), unescape $ BS.drop 2 rest]

allowed :: BS.ByteString
allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZYZ0123456789.-_~"

escape :: BS.ByteString -> BS.ByteString
escape x = case fmap (\(a,b) -> (BS.singleton a, id b)) (BS.uncons x) of
  Nothing -> x
  _ -> do
    let nextByte = BS.take 2 x
    let charOfByte = fst $ B16.decode $ nextByte
    if isJust $ BS.findSubstring charOfByte allowed
      then BS.concat [charOfByte, escape $ BS.drop 2 x]
      else BS.concat ["%", nextByte, escape $ BS.drop 2 x]
  
-- def decode(x):
--     if not x:
--         return ''

--     if x[0] == '%':
--         return x[1:3] + decode(x[3:])
--     else: --         return format(ord(x[0]), '02x') + decode(x[1:])


-- def encode(x):
--     if not x:
--         return x
--     next_byte = x[0:2]
--     char_of_byte = chr(int(next_byte, 16))
--     if char_of_byte in allowed:
--         return char_of_byte + encode(x[2:])
--    else:
--        return "%" + next_byte + encode(x[2:])

--g <- R.getStdGen 

alphaNumsList :: BS.ByteString
alphaNumsList = "abcdefghijklmnopqrstuvwzyz0123456789"
  
--alphaNums :: R.StdGen -> BS.ByteString
alphaNums g = unfoldr f (randomIndexFromSeed g)
  where f (i, newG) = Just (BS.index alphaNumsList i, randomIndexFromSeed newG)
        randomIndexFromSeed = R.randomR (0, BS.length alphaNumsList - 1)

getPeerID :: IO BS.ByteString
getPeerID = do
  g <- R.getStdGen
  return $ BS.concat ["-TR2940-",  (BS.pack $ take 12 $ alphaNums g)]
-- curl http://tracker.archlinux.org:6969/announce\?info_hash\=V%96%e5W%28%c4%0c%92%9c%d0%90s%a5%c2c%81%84%fc%e0%18\&peer_id\=-TR2940-tw3hjli2yig7\&port\=51413\&uploaded\=0\&downloaded\=0\&left\=22036480\&numwant\=80\&key\=44421c03\&compact\=1\&supportcrypto\=1\&event\=started\&ipv6\=2001%3A458%3A3%3A100%3A29ad%3Add0e%3A7f4d%3A5361

trackerRequest :: BS.ByteString -> Tracker -> IO String
trackerRequest peer_id (Tracker (Announce url) _ _ (InfoHash info_hash) (maybeSingleFileInfo) (maybeDirectoryInfo) _) = do
  --request' <- HTTP.parseRequest $ concat ["GET ", UTF8.toString url]
  -- let request =
  --       HTTP.setRequestQueryString [("port", return "6882"),
  --                                   ("uploaded", return "0"),
  --                                   ("downloaded", return "0"),
  --                                   ("left", return $ UTF8.fromString $ show left),
  --                                   ("event", return "started"),
  --                                   ("peer_id", return peer_id)] $ request'
  request <- HTTP.parseRequest $ UTF8.toString $ BS.concat [url, "?peer_id=", peer_id, "&left=", UTF8.fromString $ show left, "&event=started&port=6882&uploaded=0&downloaded=0", "&info_hash=", escape info_hash]
  response <- HTTP.httpLbs request

  --HTTP.httpLbs "http://tracker.archlinux.org:6969/announce?info_hash=V%96%e5W%28%c4%0c%92%9c%d0%90s%a5%c2c%81%84%fc%e0%18&peer_id=-TR2940-tw3hjli2yig7&port=51413&uploaded=0&downloaded=0&left=22036480&event=started"
  --response <- getWith (buildParams peer_id info_hash maybeSingleFileInfo maybeDirectoryInfo) (UTF8.toString url)
  print response
  return $ show response
  where left = if isJust maybeSingleFileInfo
              then  getSingleFileLength $ fromJust maybeSingleFileInfo
              else sum $ fmap getDirectoryFileLength $ getDirectoryInfoFiles $ fromJust maybeDirectoryInfo

buildParams peer_id info_hash maybeSingleFileInfo maybeDirectoryInfo =
  defaults & param "info_hash" .~ [TE.decodeUtf8 $ info_hash]
                        & param "port" .~ [T.pack $ show 6882]
                        & param "uploaded" .~ ["0"]
                        & param "downloaded" .~ ["0"]
                        & param "left" .~ [T.pack $ show left]
                        & param "compact".~ ["1"]
                        & param "event".~ ["started"]
                        & param "numwant".~ ["80"]
                        & param "supportcrypto".~ ["1"]
                        & param "peer_id".~ [TE.decodeUtf8 peer_id]

  where left = if isJust maybeSingleFileInfo
              then  getSingleFileLength $ fromJust maybeSingleFileInfo
              else sum $ fmap getDirectoryFileLength $ getDirectoryInfoFiles $ fromJust maybeDirectoryInfo

trackerRequestTest :: BS.ByteString -> BS.ByteString -> Tracker -> IO String
trackerRequestTest url peer_id (Tracker _ _ _ (InfoHash info_hash) (maybeSingleFileInfo) (maybeDirectoryInfo) _) =
  fmap show $ getWith p $ UTF8.toString url
  where p = defaults & param "info_hash" .~ [TE.decodeUtf8 . fst . B16.decode $ info_hash]
                          & param "port" .~ [T.pack $ show 6882]
                          & param "uploaded" .~ ["0"]
                          & param "downloaded" .~ ["0"]
                          & param "left" .~ [T.pack $ show left]
                          & param "compact".~ ["1"]
                          & param "event".~ ["started"]
                          & param "peer_id".~ [TE.decodeUtf8 peer_id]

        left = if isJust maybeSingleFileInfo
               then  getSingleFileLength $ fromJust maybeSingleFileInfo
               else sum $ fmap getDirectoryFileLength $ getDirectoryInfoFiles $ fromJust maybeDirectoryInfo

shaHash :: BS.ByteString -> BS.ByteString
shaHash = BS.pack . BS.unpack . (BA.convert . (BAE.convertToBase BAE.Base16 :: C.Digest C.SHA1 -> BS.ByteString) .  (C.hashWith C.SHA1 :: BS.ByteString -> C.Digest C.SHA1))

decodedHex = "123456789abcdef123456789abcdef123456789a" :: BS.ByteString
--(Char8.pack $ Char8.unpack x) == x => True
--Their lengths are also the same
--shaHash2 :: String -> String
-- shaHash2 = Char8.unpack . B16.encode .   (C.hashWith C.SHA1) . BS.unpack . Char8.pack

