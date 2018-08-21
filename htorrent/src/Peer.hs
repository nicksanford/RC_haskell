{-# LANGUAGE OverloadedStrings #-}
module Peer where

import Utils (unhex)
import qualified Tracker as T ( Peer (..)
                              , InfoHash (..)
                              , Tracker (..)
                              , PeerId (..)
                              )

import qualified Network.Simple.TCP as TCP
import Network.Socket ( setSocketOption
                      , SocketOption (KeepAlive)
                      )

import qualified Data.Word8 as W

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

import qualified Data.Map as M

import Data.Maybe ( isJust
                  , fromJust
                  , listToMaybe
                  )


newtype Conn e = Conn e deriving (Eq, Show)
newtype InfoHash e = InfoHash e deriving (Eq, Show)
newtype PeerId e = PeerId e deriving (Eq, Show)

data PeerResponse = PeerResponse (InfoHash BS.ByteString) (PeerId BS.ByteString) (Conn (TCP.Socket, TCP.SockAddr)) deriving (Eq, Show)

interested :: BS.ByteString
interested = BS.pack [0,0,0,1,2]

sendInterested :: PeerResponse -> IO (Maybe BS.ByteString)
sendInterested pr@(PeerResponse (InfoHash peer_info_hash) (PeerId peer_peer_id) (Conn (conn, addr))) = do
  print "sending interested"
  print addr
  TCP.send conn interested
  print $ BS.concat ["sent: ", interested]
  response <- TCP.recv conn 1024
  print "interested response"
  print response
  return response

initiateHandshake :: T.Tracker -> T.Peer -> IO (Maybe PeerResponse)
initiateHandshake tracker@(T.Tracker (T.PeerId peer_id) _ _ _ (T.InfoHash info_hash) _ _ _) 
                  (T.Peer ip port) = do
  (response, connTup) <- sendToPeer (UTF8.toString ip) (Prelude.show port) (handshake info_hash peer_id)
  let handshakeResponse :: Maybe PeerResponse
      handshakeResponse = response >>= readHandShake connTup >>= validateHandshake tracker
  if isJust handshakeResponse then
    (sendInterested $ fromJust handshakeResponse) >> return handshakeResponse
  else
    return Nothing
initiateHandshake _ _ = return Nothing

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
readHandShake :: (TCP.Socket, TCP.SockAddr)  -> BS.ByteString  ->  (Maybe PeerResponse)
readHandShake connTup r = PeerResponse <$> (InfoHash <$> infoHash) <*> (PeerId <$> peerId) <*> (Conn <$> Just connTup)
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
sendToPeer :: String -> String -> BS.ByteString -> IO (Maybe BS.ByteString, (TCP.Socket, TCP.SockAddr))
sendToPeer ip port bs = TCP.connect ip port f
  where f (conn, remoteAddr) = do
          print "connected"
          print remoteAddr
          setSocketOption conn KeepAlive 1
          TCP.send conn bs
          print $ BS.concat ["sent: ", bs]
          handshakeResponse <- TCP.recv conn 1024
          return (handshakeResponse, (conn, remoteAddr))

          -- TODO send interested message
          -- interested: <len=0001><id=2>
          -- The interested message is fixed-length and has no payload. 
          -- The length prefix is a four byte big-endian value. The message ID is a single decimal byte.
