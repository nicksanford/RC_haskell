{-# LANGUAGE OverloadedStrings #-}
module Peer where

import Utils (unhex)
import qualified Tracker as T ( Peer (..)
                              , InfoHash (..)
                              , Tracker (..)
                              , PeerId (..)
                              )

import Network.Socket hiding (recv, send, sendAll)
import Network.Socket.ByteString (recv, send, sendAll)

import qualified Data.Word8 as W

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

import qualified Data.Map as M

import Control.Monad (unless)
import Data.Maybe ( isJust
                  , fromJust
                  , isNothing
                  , listToMaybe
                  )


newtype Conn e = Conn e deriving (Eq, Show)
newtype InfoHash e = InfoHash e deriving (Eq, Show)
newtype PeerId e = PeerId e deriving (Eq, Show)

data PeerResponse = PeerResponse (InfoHash BS.ByteString) (PeerId BS.ByteString) (Conn Socket) deriving (Eq, Show)

-- TODO Got to figure out how to send a keep alive to every peer every 30 seconds w/o blocking the thread
recvLoop :: T.Tracker -> T.Peer -> PeerResponse -> IO ()
recvLoop tracker peer pr@(PeerResponse _ _ (Conn conn)) =  do
  print $ "Blocked on recvLoop " ++ (show peer)
  msg <- recv conn 4096
  print $ BS.concat ["Got: ", msg, " from ", (UTF8.fromString $ show peer), " RPC: ", (UTF8.fromString $ show $ parseRPC msg), " aka: ", UTF8.fromString $ show $ BS.unpack msg]
  unless (BS.null msg) $ do
    recvLoop tracker peer pr

startPeer :: T.Tracker -> T.Peer -> IO ()
startPeer tracker peer =  do
  maybePeerResponse <- initiateHandshake tracker peer
  print maybePeerResponse
  unless (isNothing maybePeerResponse) $ do
    let peerResponse = fromJust maybePeerResponse
    print $ "STARTPEER sending interested for: " ++ (show peer)
    msg <- sendInterested peerResponse
    print $ "STARTPEER interested response from: " ++ (show peer) ++ " RPC: " ++ (show $ parseRPC msg) ++ " msg: " ++ (show msg) ++ " aka: " ++ (show $ BS.unpack msg)
    recvLoop tracker peer peerResponse

-- I can start parsing the bitfield so I can start trying to leach data

-- TODO This is going to need to be a 
-- parseRPC :: BS.ByteString -> Run BS.ByteString  (Maybe [PeerRPC])
-- B/c you might not yet have all the data you need and multiple messages can be received in a given receive block
-- there are also situations where you might want to drop the connection if the parsing rules are violated
-- Right now this makes the assumption that we are only ever getting a single RPC in a given recv call (which is not true)
parseRPC :: BS.ByteString -> Either BS.ByteString  PeerRPC
parseRPC = parseRPC' . BS.unpack

parseRPC' unpackedMsg
  | unpackedMsg == [0,0,0,0] = Right PeerKeepAlive
  | unpackedMsg == [0,0,0,1,0] = Right Choke
  | unpackedMsg == [0,0,0,1,1] = Right UnChoke
  | unpackedMsg == [0,0,0,1,2] = Right Interested
  | unpackedMsg == [0,0,0,1,3] = Right NotInterested
  | (take 5 unpackedMsg) == [0,0,0,5,4] = Right $ Have $ BS.pack $ take 4 $ drop 5 unpackedMsg
  | (take 3 unpackedMsg) == [0,0,0] &&
    ((drop 4 $ take 5 unpackedMsg) == [5])  = if ((unpackedMsg !! 3) - 1) == (fromIntegral $  length $ drop 5 unpackedMsg) then
                                              Right $ BitField $ BS.pack $ drop 5 unpackedMsg
                                            else
                                              Left "ERROR parseRPC in BitField parse, length does not match bitfield length"
  | (take 5 unpackedMsg) == [0,0,1,3,6] = if length unpackedMsg == 8 then
                                            Right $ Request (fromIntegral $ unpackedMsg !! 5)
                                                            (fromIntegral $ unpackedMsg !! 6)
                                                            (fromIntegral $ unpackedMsg !! 7)
                                          else
                                            Left $ BS.concat ["ERROR parseRPC in Request parse, expected length 8, but message is length ", (UTF8.fromString $ show $ length unpackedMsg)]
  | otherwise = Left $ BS.concat ["ERROR parseRPC in unmatched parse ", BS.pack unpackedMsg]

data PeerRPC = PeerKeepAlive
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have BS.ByteString
             | BitField BS.ByteString
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
have pieceIndex = BS.concat [(BS.pack [0,0,0,5,4]), pieceIndex]

bitfield :: BS.ByteString -> BS.ByteString
bitfield payload = BS.concat [(BS.pack [0,0,0,1 + (fromIntegral . BS.length $ payload), 5]), payload]

isValidBitField :: BS.ByteString -> Bool
isValidBitField bs = len == (fromIntegral $ length $ drop 5 xs)
  where xs = BS.unpack bs
        len = (head $ drop 3 xs) - 1

request :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString 
request index begin length = BS.concat [(BS.pack [0,0,1,3,6]), index, begin, length]

sendInterested :: PeerResponse -> IO (BS.ByteString)
sendInterested pr@(PeerResponse (InfoHash peer_info_hash) (PeerId peer_peer_id) (Conn conn)) = do
  sendAll conn interested
  response <- recv conn 4096
  return response

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
-- If the info_hash is different from the info hash in the tracker, then it should
-- also be dropped
--
-- TODO try to send 16k blocks
-- Try downloading pieces in random order first, then, as an optimization, try rairest first.
-- Only change choaked peers once every 10 seconds (to prevent fibralation)
readHandShake :: Socket -> BS.ByteString  ->  (Maybe PeerResponse)
readHandShake conn r = PeerResponse <$> (InfoHash <$> infoHash) <*> (PeerId <$> peerId) <*> (Conn <$> Just conn)
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
  print "sending"
  sock <- addrIO >>= open
  sendAll sock bs
  print $ BS.concat ["sent: ", bs]
  -- This is to make it so that I don't overfetch here, given that I know 
  -- exactly how many bytes I need to read in for the handshake response.
  -- 49 + (length "BitTorrent protocol") == 58
  msg <- recv sock 68
  print $ BS.concat ["got: ", msg]
  return (msg, sock)

  where hints = defaultHints { addrSocketType = Stream }
        addrIO = getAddrInfo (Just hints) (Just $ UTF8.toString ip) (Just $ show port) >>= return . head
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          connect sock $ addrAddress addr
          print "in open addr"
          print "addr"
          print addr
          print "sock"
          print sock
          return sock

          -- TODO send interested message
          -- interested: <len=0001><id=2>
          -- The interested message is fixed-length and has no payload. 
          -- The length prefix is a four byte big-endian value. The message ID is a single decimal byte.
