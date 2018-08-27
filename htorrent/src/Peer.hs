{-# LANGUAGE OverloadedStrings #-}
module Peer where

import qualified Tracker                   as T (InfoHash (..), Peer (..),
                                                 PeerId (..), Tracker (..), getTrackerPieces)
import           Utils                     (unhex)

import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)

import qualified Data.Word8                as W
import qualified Data.Bits                 as Bits

import qualified Data.Either               as Either
import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as UTF8

import           Control.Monad             (unless)
import           Data.Maybe                (fromJust, isNothing, listToMaybe)
import qualified Data.Map                  as M


newtype Conn e = Conn e deriving (Eq, Show)
newtype InfoHash e = InfoHash e deriving (Eq, Show)
newtype PeerId e = PeerId e deriving (Eq, Show)

data PeerResponse = PeerResponse (InfoHash BS.ByteString) (PeerId BS.ByteString) (Conn Socket) deriving (Eq, Show)

type PieceMap = (M.Map BS.ByteString Bool)

data PeerState = PeerState (PeerId BS.ByteString) (Conn Socket) (Maybe PieceMap) deriving (Eq, Show)

maybeUpdatePieceMap :: PeerState -> Maybe PieceMap -> PeerState
maybeUpdatePieceMap peerState@(PeerState _ _ (Just _)) _ = peerState
maybeUpdatePieceMap (PeerState peer_id conn _) maybePieceMap = PeerState peer_id conn maybePieceMap

-- TODO Got to figure out how to send a keep alive to every peer every 30 seconds w/o blocking the thread
recvLoop :: T.Tracker -> PeerState -> IO ()
recvLoop tracker peerState@(PeerState (PeerId peer_id) (Conn conn) _maybePieceMap) =  do
  print $ "Blocked on recvLoop " ++ show peer_id
  msg <- recv conn 4096
  let parsed = parseRPC tracker msg
  let pieceMap = case parsed of
        Right (BitField bitfield) ->
          Just bitfield
        _ ->
          Nothing

  mapM_ print $ Either.lefts [parsed]
  let maybeNewPeerState = maybeUpdatePieceMap peerState pieceMap
  unless (maybeNewPeerState == peerState) $
    print $ "peerStateUpdated to: " ++ (show maybeNewPeerState)

  unless (BS.null msg) $
    recvLoop tracker maybeNewPeerState

startPeer :: T.Tracker -> T.Peer -> IO ()
startPeer tracker peer =  do
  maybePeerResponse <- initiateHandshake tracker peer
  print $ "STARTPEER maybePeerResponse: " ++ show maybePeerResponse
  unless (isNothing maybePeerResponse) $ do
    let peerResponse@(PeerResponse (InfoHash info_hash) (PeerId peer_id) (Conn conn)) = fromJust maybePeerResponse
    print $ "STARTPEER sending interested for: " ++ show peer
    msg <- sendInterested peerResponse

    let parsed = parseRPC tracker msg
    let pieceMap = case parsed of
          Right (BitField bitfield) ->
            Just bitfield
          _ ->
            Nothing

    mapM_ print $ Either.lefts [parsed]

    unless (isNothing pieceMap) $
      print $ BS.concat ["STARTPEER pieceMap parsed successfuly for peer ", peer_id]

    let peerState = PeerState (PeerId peer_id) (Conn conn) pieceMap
    recvLoop tracker peerState

-- I can start parsing the bitfield so I can start trying to leach data

-- TODO This is going to need to be a
-- parseRPC :: BS.ByteString -> Run BS.ByteString  (Maybe [PeerRPC])
-- B/c you might not yet have all the data you need and multiple messages can be received in a given receive block
-- there are also situations where you might want to drop the connection if the parsing rules are violated
-- Right now this makes the assumption that we are only ever getting a single RPC in a given recv call (which is not true)
parseRPC :: T.Tracker -> BS.ByteString -> Either BS.ByteString  PeerRPC
parseRPC tracker = (parseRPC' tracker) . BS.unpack

parseRPC' tracker unpackedMsg
  | unpackedMsg == [0,0,0,0] = Right PeerKeepAlive
  | unpackedMsg == [0,0,0,1,0] = Right Choke
  | unpackedMsg == [0,0,0,1,1] = Right UnChoke
  | unpackedMsg == [0,0,0,1,2] = Right Interested
  | unpackedMsg == [0,0,0,1,3] = Right NotInterested
  | take 5 unpackedMsg == [0,0,0,5,4] = Right $ Have $ BS.pack $ take 4 $ drop 5 unpackedMsg
  | take 3 unpackedMsg == [0,0,0] &&
    (drop 4 (take 5 unpackedMsg) == [5])  = if ((unpackedMsg !! 3) - 1) == fromIntegral (length $ drop 5 unpackedMsg) then do
                                            let word8s = (drop 5 unpackedMsg)
                                            let boolsBeforeCheck = word8s >>= (\ x -> reverse [Bits.testBit x i | i <- [0 .. 7]])
                                            let extraBits = drop (length $ T.getTrackerPieces tracker) boolsBeforeCheck
                                            if  length boolsBeforeCheck  <  length (T.getTrackerPieces tracker)  then
                                              Left "ERROR parseRPC in BitField parse, too few bytes sent in bitfield"
                                             else
                                              if  or extraBits then
                                                Left "ERROR parseRPC in BitField parse, extra bits were set"
                                              else
                                                Right $ BitField $ M.fromList $ zip (T.getTrackerPieces tracker) (take (length $ T.getTrackerPieces tracker) boolsBeforeCheck) 
                                          else
                                            Left $ BS.concat ["ERROR parseRPC in BitField parse, length does not match bitfield length: ", BS.pack unpackedMsg]
  | take 5 unpackedMsg == [0,0,1,3,6] = if length unpackedMsg == 8 then
                                          Right $ Request (fromIntegral $ unpackedMsg !! 5)
                                                          (fromIntegral $ unpackedMsg !! 6)
                                                          (fromIntegral $ unpackedMsg !! 7)
                                        else
                                          Left $ BS.concat ["ERROR parseRPC in Request parse, expected length 8, but message is length ", UTF8.fromString $ show $ length unpackedMsg]
  | otherwise = Left $ BS.concat ["ERROR parseRPC in unmatched parse ", BS.pack unpackedMsg]

data PeerRPC = PeerKeepAlive
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have BS.ByteString
             | BitField PieceMap
             | Request Integer Integer Integer
             deriving (Eq, Show)


keepAlive :: BS.ByteString
keepAlive = BS.pack [0,0,0,0]

choke :: BS.ByteString
choke = BS.pack [0,0,0,1,0]

unchoke :: BS.ByteString
unchoke = BS.pack [0,0,0,1,1]

interested :: BS.ByteString
interested = BS.pack [0,0,0,1,2]

notInterested :: BS.ByteString
notInterested = BS.pack [0,0,0,1,3]

have :: BS.ByteString -> BS.ByteString
have pieceIndex = BS.concat [BS.pack [0,0,0,5,4], pieceIndex]

--bitfield :: BS.ByteString -> BS.ByteString
--bitfield payload = BS.concat [BS.pack [0,0,0,1 + (fromIntegral . BS.length $ payload), 5], payload]

isValidBitField :: BS.ByteString -> Bool
isValidBitField bs = len == fromIntegral (length $ drop 5 xs)
  where xs = BS.unpack bs
        len = xs !! 3 - 1

request :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
request index begin len = BS.concat [BS.pack [0,0,1,3,6], index, begin, len]

sendInterested :: PeerResponse -> IO BS.ByteString
sendInterested pr@(PeerResponse (InfoHash peer_info_hash) (PeerId peer_peer_id) (Conn conn)) = do
  sendAll conn interested
  recv conn 4096

trackerToPeerHandshake :: T.Tracker -> BS.ByteString
trackerToPeerHandshake (T.Tracker (T.PeerId peer_id) _ _ _ (T.InfoHash info_hash) _ _ _) =
  handshake info_hash peer_id

initiateHandshake :: T.Tracker -> T.Peer -> IO (Maybe PeerResponse)
initiateHandshake tracker peer = do
  (response, conn) <- sendHandshake peer $ trackerToPeerHandshake tracker
  return $ readHandShake conn response >>= validateHandshake tracker

handshake :: BS.ByteString -> BS.ByteString -> BS.ByteString
handshake info_hash peer_id =  BS.concat [pstrlen, pstr, reserved, unhex info_hash, peer_id]
  where pstr = "BitTorrent protocol"
        pstrlen = BS.singleton $ fromIntegral (BS.length pstr)
        reserved = BS.replicate 8 W._nul


-- TODO: If the peer_id differs from what the tracker sent (if the tracker sent
-- anything regarding the peer_id) then it should be dropped.
-- DONE: If the info_hash is different from the info hash in the tracker, then it should
-- also be dropped
--
-- TODO try to send 16k blocks
-- Try downloading pieces in random order first, then, as an optimization, try rairest first.
-- Only change choaked peers once every 10 seconds (to prevent fibralation)
readHandShake :: Socket -> BS.ByteString  ->  Maybe PeerResponse
readHandShake conn r = PeerResponse <$> (InfoHash <$> infoHash) <*> (PeerId <$> peerId) <*> (Conn <$> Just conn)
  where word8s = BS.unpack r
        afterProtocol :: Maybe BS.ByteString
        afterProtocol = BS.pack . flip drop word8s . (1+) . fromIntegral <$> listToMaybe word8s
        afterEmpty :: Maybe BS.ByteString
        afterEmpty = BS.drop 8 <$> afterProtocol
        infoHash :: Maybe BS.ByteString
        infoHash = BS.take 20 <$> afterEmpty
        peerId :: Maybe BS.ByteString
        peerId = BS.drop 20 <$> afterEmpty

validateHandshake :: T.Tracker -> PeerResponse -> Maybe PeerResponse
validateHandshake (T.Tracker _ _ _ _ (T.InfoHash info_hash) _ _ _)
                  pr@(PeerResponse (InfoHash peer_info_hash) (PeerId peer_peer_id) _) =
                  if unhex info_hash == peer_info_hash then
                    Just pr
                  else
                    Nothing


-- This can return whatever you want (I believe including the socket)
sendHandshake :: T.Peer -> BS.ByteString -> IO (BS.ByteString, Socket)
sendHandshake (T.Peer ip port) bs = do
  --print "sending"
  sock <- addrIO >>= open
  sendAll sock bs
  --print $ BS.concat ["sent: ", bs]
  -- This is to make it so that I don't overfetch here, given that I know
  -- exactly how many bytes I need to read in for the handshake response.
  -- 49 + (length "BitTorrent protocol") == 58
  msg <- recv sock 68
  --print $ BS.concat ["got: ", msg]
  return (msg, sock)

  where hints = defaultHints { addrSocketType = Stream }
        addrIO = head <$> getAddrInfo (Just hints) (Just $ UTF8.toString ip) (Just $ show port)
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          connect sock $ addrAddress addr
          -- print "in open addr"
          -- print "addr"
          -- print addr
          -- print "sock"
          -- print sock
          return sock
