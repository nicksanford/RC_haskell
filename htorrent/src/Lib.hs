{-# LANGUAGE OverloadedStrings #-}
module Lib where


import BEncode

import qualified Network.HTTP.Simple as HTTP
import Data.List
import qualified  System.Random as R


import qualified Network.Simple.TCP as TCP
import Network.Socket (setSocketOption, SocketOption (KeepAlive))


import qualified Data.ByteString as BS


import qualified Data.Word8 as W

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Numeric

import qualified Data.Map as M

import qualified Crypto.Hash as C

import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE

import Data.Maybe (isNothing, fromJust, isJust, listToMaybe)

import Crypto.Random (getRandomBytes)
import qualified Data.Binary as B

--import qualified Network.URI as URI
--import qualified Data.ByteString.Char8 as Char8
--import qualified Data.Byteable as Byteable
--import Control.Lens
--import Network.Wreq
--import Control.Monad (join)

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
newtype PeerId e = PeerId e deriving (Eq, Show)
newtype Peers e = Peers e deriving (Eq, Show)
newtype TrackerId e = TrackerId e deriving (Eq, Show)
newtype Warning e = Warning e deriving (Eq, Show)
newtype Interval e = Interval e deriving (Eq, Show)
newtype MinInterval e = MinInterval e deriving (Eq, Show)
newtype Complete e = Complete e deriving (Eq, Show)
newtype InComplete e = InComplete e deriving (Eq, Show)
newtype Choking e = Choking e deriving (Eq, Show)
newtype Interested e = Interested e deriving (Eq, Show)

data SingleFileInfo = SingleFileInfo (Name BS.ByteString) (Length Integer) (MD5Sum (Maybe BS.ByteString)) deriving (Eq, Show)
data DirectoryFile = DirectoryFile (Path BS.ByteString) (Length Integer) (MD5Sum (Maybe BS.ByteString)) deriving (Eq, Show)
data DirectoryInfo = DirectoryInfo (Name BS.ByteString) (Files [DirectoryFile]) deriving (Eq, Show)

type IP = BS.ByteString
type Port = Integer
-- TODO: You will need to implement a Maybe PeerId here as it is possible for that to be provided. If it is, then you need to verify that the peer id you get back from the handshake is the same as what the tracker said.
data Peer = Peer IP Port deriving (Eq, Show)

data Tracker = Tracker (PeerId BS.ByteString) (Announce BS.ByteString) (PieceLength Integer) (Pieces BS.ByteString) (InfoHash BS.ByteString) (Maybe SingleFileInfo) (Maybe DirectoryInfo) (Maybe (Encoding BS.ByteString)) deriving (Eq, Show)

data TrackerResponse = TrackerResponse (Peers [Peer]) 
                                       (Maybe (TrackerId BS.ByteString)) 
                                       (Maybe (Warning BS.ByteString))
                                       (Interval Integer)
                                       (Maybe (MinInterval Integer))
                                       (Maybe (Complete BS.ByteString))
                                       (Maybe (InComplete BS.ByteString)) deriving (Eq, Show)

data ChokeState = ChokeState (Choking Bool) (Interested Bool) deriving (Eq, Show)

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

toTracker :: BS.ByteString -> BEncode -> Either BS.ByteString Tracker
toTracker peer_id (BDict d) = maybe (Left "ERROR: tracker invalid") Right $  buildTracker <$> (Just $ PeerId peer_id) <*> maybeAnnounce <*> maybePieceLength <*> maybePieces <*> maybeInfoHash >>= validateTracker
  where buildTracker :: PeerId BS.ByteString -> Announce BS.ByteString -> PieceLength Integer -> Pieces BS.ByteString -> InfoHash BS.ByteString-> Tracker
        buildTracker pid a pl p i = Tracker pid a pl p i singleFileInfo directoryInfo maybeEncoding
        validateTracker :: Tracker -> Maybe Tracker
        validateTracker t@(Tracker _ _ _ _ _ sfi dfi _)
          | isNothing sfi && isNothing dfi = Nothing
          | otherwise = Just t
        l :: BEncode -> Maybe BEncode
        l x = M.lookup (BString "info") d >>= bencodeToMaybeDict >>= M.lookup x
        maybeInfoHash :: Maybe (InfoHash BS.ByteString)
        maybeInfoHash = InfoHash . shaHash . encode <$> M.lookup (BString "info") d
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
toTracker _ _ = Left "ERROR: Tracker invalid due to absence of both single file dict and directory dict"


test filename host port = do
  peer_id <- getPeerID
  maybeBencode <- maybeReadBencode filename
  let maybeTracker = maybeBencode >>= toTracker peer_id
  case maybeTracker of
    Right tracker ->
      trackerRequest tracker host port >>= print
    Left error ->
      print error

randomBytes :: Int -> IO BS.ByteString
randomBytes  = getRandomBytes

unescape :: BS.ByteString -> BS.ByteString
unescape x = case fmap (\(a,b) -> (BS.singleton a, b)) (BS.uncons x) of
  Nothing -> x
  (Just ("%", rest)) -> BS.concat [BS.take 2 rest, unescape $ BS.drop 2 rest]
  -- figure out how to do the ord stuff
  (Just (_, rest)) -> BS.concat [BS.take 2 rest, unescape $ BS.drop 2 rest]

allowed :: BS.ByteString
allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZYZ0123456789.-_~"

escape :: BS.ByteString -> BS.ByteString
escape x = case fmap (\(a,b) -> (BS.singleton a, b)) (BS.uncons x) of
  Nothing -> x
  _ -> do
    let nextByte = BS.take 2 x
    let charOfByte = fst . B16.decode $ nextByte
    if isJust $ BS.findSubstring charOfByte allowed
      then BS.concat [charOfByte, escape $ BS.drop 2 x]
      else BS.concat ["%", nextByte, escape $ BS.drop 2 x]
  
-- def decode(x):
--     if not x:
--         return ''

--     if x[0] == '%':
--         return x[1:3] + decode(x[3:])
--     else: --         return format(ord(x[0]), '02x') + decode(x[1:])


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
  return $ BS.concat ["-TR2940-",  BS.pack $ take 12 $ alphaNums g]


createTrackerRequestPayload (Tracker (PeerId peer_id) (Announce url) _ _ (InfoHash info_hash) maybeSingleFileInfo maybeDirectoryInfo _) port =
  requestString
  where left = if isJust maybeSingleFileInfo
              then  getSingleFileLength $ fromJust maybeSingleFileInfo
              else sum $ fmap getDirectoryFileLength $ getDirectoryInfoFiles $ fromJust maybeDirectoryInfo
        requestString = UTF8.toString $ BS.concat [ url
                                                  , "?peer_id=", peer_id
                                                  , "&left=", UTF8.fromString $ show left
                                                  , "&event=started"
                                                  , BS.concat [ "port=", UTF8.fromString $ show port]
                                                  , "&uploaded=0"
                                                  , "&downloaded=0"
                                                  , "&numwant=80"
                                                  , "&info_hash=", escape info_hash
                                                  ]


data PeerResponse = PeerResponse (InfoHash BS.ByteString) (PeerId BS.ByteString) deriving (Eq, Show)

-- TODO: If the peer_id differs from what the tracker sent (if the tracker sent
-- anything regarding the peer_id) then it should be dropped.
-- If the info_hash is different from the info hash in the tracker, then it should
-- also be dropped
--
-- TODO try to send 16k blocks
-- Try downloading pieces in random order first, then, as an optimization, try rairest first.
-- Only change choaked peers once every 10 seconds (to prevent fibralation)
readHandShake :: BS.ByteString -> Maybe PeerResponse
readHandShake r = PeerResponse <$> (InfoHash <$> infoHash) <*> (PeerId <$> peerId)
  where word8s = BS.unpack r
        afterProtocol :: Maybe BS.ByteString
        afterProtocol = BS.pack <$> flip drop word8s . (1+)
                                <$> fromIntegral
                                <$> listToMaybe word8s
        afterEmpty :: Maybe BS.ByteString
        afterEmpty = BS.drop 8 <$> afterProtocol
        infoHash :: Maybe BS.ByteString
        infoHash = BS.take 20 <$> afterEmpty
        peerId :: Maybe BS.ByteString
        peerId = BS.drop 20 <$> afterEmpty

validateHandshake :: Tracker -> PeerResponse -> Maybe PeerResponse
validateHandshake (Tracker _ _ _ _ (InfoHash info_hash) _ _ _)
                  pr@(PeerResponse (InfoHash peer_info_hash) (PeerId peer_peer_id)) =
                  if unhex info_hash == peer_info_hash then
                    Just pr
                  else
                    Nothing

initiateHandshake :: Tracker -> Peer -> IO (Maybe PeerResponse)
initiateHandshake tracker@(Tracker (PeerId peer_id) _ _ _ (InfoHash info_hash) _ _ _) 
                  (Peer ip port) = do
  response <- sendToPeer (UTF8.toString ip) (Prelude.show port) handshake
  let handshakeResponse = response >>= readHandShake >>= validateHandshake tracker
  return handshakeResponse
  where handshake =  BS.concat [pstrlen, pstr, reserved, unhex info_hash, peer_id]
        pstr = "BitTorrent protocol"
        pstrlen = BS.singleton $ fromIntegral (BS.length pstr)
        reserved = BS.replicate 8 W._nul
initiateHandshake _ _ = return Nothing

sendToPeer :: String -> String -> BS.ByteString -> IO (Maybe BS.ByteString)
sendToPeer ip port bs = TCP.connect ip port f
  where f (conn, remoteAddr) = do
          print "connected"
          setSocketOption conn KeepAlive 1
          TCP.send conn bs
          print $ BS.concat ["sent: ", bs]
          TCP.recv conn 1024
          -- TODO send interested message


trackerRequest :: Tracker -> BS.ByteString -> Integer -> IO (Maybe [Maybe PeerResponse])
trackerRequest tracker host port = do
  maybeTrackerResponse <- HTTP.parseRequest (createTrackerRequestPayload tracker port) >>=
                         HTTP.httpBS >>=
                         handleTrackerRequest

  case maybeTrackerResponse of
    Nothing -> return Nothing
    (Just (TrackerResponse (Peers peers) _ _ _ _ _ _)) ->
    -- right now this just takes 2 peers, but it could easily take as many as desired.
    -- Currently, if any of the tcp connections thow, I don't know how to deal with that
          Just <$> (traverse (initiateHandshake tracker) $ take 30 peers)


unhex :: BS.ByteString -> BS.ByteString
unhex x =
  BS.pack $ (fmap fromIntegral nums)
  where z = Prelude.zip [0..] (UTF8.toString x)
        evens :: String
        evens = [snd tuple | tuple <- z, even $ fst tuple]
        odds :: String
        odds = [snd tuple | tuple <- z, odd $ fst tuple]
        func a b = fst . Prelude.head . Numeric.readHex $ [a,b]
        nums = Prelude.zipWith func evens odds


bDictToPeer :: BEncode -> Maybe Peer
bDictToPeer (BDict d) = Peer <$> (M.lookup (BString "ip") d >>= bencodeToMaybeString)
                             <*> (M.lookup (BString "port") d >>= bencodeToMaybeInteger)
bDictToPeer _ = Nothing

handleTrackerRequest :: HTTP.Response BS.ByteString -> IO (Maybe TrackerResponse)
handleTrackerRequest response =
  print response >>
  case decode $ HTTP.getResponseBody response of
    (Run "" (Just (BDict d))) ->
      -- TODO refactor this case statement
      print (M.keys d) >>
      case (M.lookup (BString "peers") d) of
        (Just (BList l)) ->
          return $ TrackerResponse <$> fmap Peers (traverse bDictToPeer l)
                                   <*> Just (fmap TrackerId (M.lookup (BString "tracker id") d >>= bencodeToMaybeString))
                                   <*> Just (fmap Warning (M.lookup (BString "warning message") d >>= bencodeToMaybeString))
                                   <*> fmap Interval (M.lookup (BString "interval") d >>= bencodeToMaybeInteger)
                                   <*> Just (fmap MinInterval (M.lookup (BString "min interval") d >>= bencodeToMaybeInteger))
                                   <*> Just (fmap Complete (M.lookup (BString "complete") d >>= bencodeToMaybeString))
                                   <*> Just (fmap InComplete (M.lookup (BString "incomplete") d >>= bencodeToMaybeString))
        (Just (BString s)) ->
          print "got string from peers" >>
          print s >>
          return Nothing
        other ->
          print "ERROR: Got different value than expected from peers" >>
          print other >>
          return Nothing
    decoded ->
      print "ERROR: Didn't parse correctly" >>
      print decoded >>
      return Nothing

shaHash :: BS.ByteString -> BS.ByteString
shaHash = BS.pack . BS.unpack . (BA.convert . (BAE.convertToBase BAE.Base16 :: C.Digest C.SHA1 -> BS.ByteString) .  (C.hashWith C.SHA1 :: BS.ByteString -> C.Digest C.SHA1))

shaHashRaw :: BS.ByteString -> BS.ByteString
shaHashRaw = BS.pack . BS.unpack . (BA.convert . (BAE.convertToBase BAE.Base16 :: C.Digest C.SHA1 -> BS.ByteString) .  (C.hashWith C.SHA1 :: BS.ByteString -> C.Digest C.SHA1))


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


-- start :: String -> IO (Either BS.ByteString (HTTP.Response BS.ByteString)))
-- start filePath = do
--   peer_id <- getPeerID
--   case (maybeReadBencode filePath) >>= toTracker of
--     (Left error) -> print $ Left error
--     (Right value) -> print $ Right $ trackerRequest peer_id value

-- case decode $ HTTP.getResponseBody response of
--   Run "" (Just (BDict x)) ->
--     case (M.lookup (BString "failure reason") x) of
--       Nothing ->
--         return response
--       _ ->
--   Run _ _ ->
--     return response

-- trackerRequest :: BS.ByteString -> Tracker -> IO (HTTP.Response BS.ByteString)
-- trackerRequest peer_id (Tracker (Announce url) _ _ (InfoHash info_hash) (maybeSingleFileInfo) (maybeDirectoryInfo) _) = do
--   request <- HTTP.parseRequest $ UTF8.toString $ BS.concat [url,
--                                                             "?peer_id=", peer_id,
--                                                             "&left=", UTF8.fromString $ show left,
--                                                             "&event=started",
--                                                             "port=6882",
--                                                             "&uploaded=0",
--                                                             "&downloaded=0",
--                                                             "&numwant=80",
--                                                             "&compact=1",
--                                                             "&info_hash=", escape info_hash]

--   secondRequest <- HTTP.parseRequest $ UTF8.toString $ BS.concat [secondRequestURL,
--                                                             "?peer_id=", peer_id,
--                                                             "&left=", UTF8.fromString $ show left,
--                                                             "&event=started",
--                                                             "port=6882",
--                                                             "&uploaded=0",
--                                                             "&downloaded=0",
--                                                             "&numwant=80",
--                                                             "&compact=1",
--                                                             "&info_hash=", escape info_hash]
--   print "first http request"
--   print url
--   response <- HTTP.httpBS request
--   print response
--   print $ HTTP.getResponseBody response

--   -- join $ listToMaybe . (BS.split W._period) <$> (listToMaybe $ Data.List.reverse $ BS.split W._slash a)
--   case decode $ HTTP.getResponseBody response of
--     Run "" (Just (BDict x)) -> case (M.lookup (BString "failure reason") x) of
--       Nothing ->
--         print "got back a result but no reason for failure" >>
--         return response
--       _ ->
--         if isJust $ (listToMaybe $ Data.List.reverse $ BS.split  W._slash url) >>= (BS.findSubstring "announce")
--           then print "making second request" >>
--                print secondRequestURL >>
--                print secondRequest >>
--                HTTP.httpBS secondRequest 
--           else
--             return response
--     Run _ _ ->
--       return response
--   --return response
--   where left = if isJust maybeSingleFileInfo
--               then  getSingleFileLength $ fromJust maybeSingleFileInfo
--               else sum $ fmap getDirectoryFileLength $ getDirectoryInfoFiles $ fromJust maybeDirectoryInfo
--         secondRequestURL = TE.encodeUtf8 $ T.replace "announce" "scrape" $ TE.decodeUtf8 url

  --HTTP.httpLbs "http://tracker.archlinux.org:6969/announce?info_hash=V%96%e5W%28%c4%0c%92%9c%d0%90s%a5%c2c%81%84%fc%e0%18&peer_id=-TR2940-tw3hjli2yig7&port=51413&uploaded=0&downloaded=0&left=22036480&event=started"
  --response <- getWith (buildParams peer_id info_hash maybeSingleFileInfo maybeDirectoryInfo) (UTF8.toString url)

-- trackerRequestTest :: BS.ByteString -> BS.ByteString -> Tracker -> IO String
-- trackerRequestTest url peer_id (Tracker _ _ _ (InfoHash info_hash) (maybeSingleFileInfo) (maybeDirectoryInfo) _) =
--   fmap show $ getWith p $ UTF8.toString url
--   where p = defaults & param "info_hash" .~ [TE.decodeUtf8 . fst . B16.decode $ info_hash]
--                           & param "port" .~ [T.pack $ show 6882]
--                           & param "uploaded" .~ ["0"]
--                           & param "downloaded" .~ ["0"]
--                           & param "left" .~ [T.pack $ show left]
--                           & param "compact".~ ["1"]
--                           & param "event".~ ["started"]
--                           & param "peer_id".~ [TE.decodeUtf8 peer_id]

--         left = if isJust maybeSingleFileInfo
--                then  getSingleFileLength $ fromJust maybeSingleFileInfo
--                else sum $ fmap getDirectoryFileLength $ getDirectoryInfoFiles $ fromJust maybeDirectoryInfo

--(Char8.pack $ Char8.unpack x) == x => True
--Their lengths are also the same
--shaHash2 :: String -> String
-- shaHash2 = Char8.unpack . B16.encode .   (C.hashWith C.SHA1) . BS.unpack . Char8.pack

-- buildParams peer_id info_hash maybeSingleFileInfo maybeDirectoryInfo =
--   defaults & param "info_hash" .~ [TE.decodeUtf8 $ info_hash]
--                         & param "port" .~ [T.pack $ show 6882]
--                         & param "uploaded" .~ ["0"]
--                         & param "downloaded" .~ ["0"]
--                         & param "left" .~ [T.pack $ show left]
--                         & param "compact".~ ["1"]
--                         & param "event".~ ["started"]

--                         & param "supportcrypto".~ ["1"]
--                         & param "peer_id".~ [TE.decodeUtf8 peer_id]

--   where left = if isJust maybeSingleFileInfo
--               then  getSingleFileLength $ fromJust maybeSingleFileInfo
--               else sum $ fmap getDirectoryFileLength $ getDirectoryInfoFiles $ fromJust maybeDirectoryInfo
