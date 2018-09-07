{-# LANGUAGE OverloadedStrings #-}
module Server (start) where

import Control.Concurrent.Chan as Chan
import Control.Concurrent (forkFinally, threadDelay) -- threadWaitRead, threadWaitWrite
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll, send)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Text.Printf (printf)
import Data.Maybe (isJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified System.Clock as Clock

import qualified Peer as Peer
import qualified Shared as Shared
import qualified Tracker as Tracker

start :: String -> Tracker.Tracker -> Chan.Chan Shared.WorkMessage -> Chan.Chan Shared.ResponseMessage -> Chan.Chan a -> IO ()
start port tracker workChan responseChan broadcastChan = do
  print "Running, and listening on port %s\n"
  printf "HINT: run echo \"did you get this message?\"|  nc localhost %s\n" port
  E.bracket (addrIO >>= open) close loop
  where hints = defaultHints { addrSocketType = Stream
                             , addrFlags = [AI_PASSIVE]
                             }
        addrIO = getAddrInfo (Just hints) Nothing (Just port) >>= return . head
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          bind sock (addrAddress addr)
          let fd = fdSocket sock
          setCloseOnExecIfNeeded fd
          listen sock 10
          return sock
        loop sock = forever $ do
          (conn, peer) <- accept sock
          putStrLn $ "LOOP: Accepted connection " ++ (show conn) ++ " from " ++ (show peer)

          void $ forkFinally (talk conn peer broadcastChan) (\_ -> print ("closing " ++ (show conn) ++ "  from " ++ (show peer)) >> close conn)

        -- loopTalk conn peer peerRPCParse broadcastChan = do
        --   -- 3. respond to requests
        --   -- 4. send have messages as you get more data
        --   --sendAll conn Peer.keepAlive
        --   msg <- recv conn 16384
        --   let newPeerRPCParse@(Peer.PeerRPCParse _ maybeErrors _) = Peer.parseRPC tracker msg peerRPCParse
        --   print $ "LOOPTALK: newPeerRPCParse " ++ show newPeerRPCParse
        --   unless (BS.null msg || isJust maybeErrors) $ do
        --     print $ "LOOPTALK: ending " ++ (show $ BS.null msg) ++ " " ++ (show $ isJust maybeErrors)
        --     loopTalk conn peer newPeerRPCParse broadcastChan

        talk conn peer broadcastChan = do
          -- 1. send handshake
          -- 1.1. send interested
          -- 1.2. send unchoke
          msg <- recv conn 68
          let maybeHandshakeResponse = Peer.readHandShake msg >>= Peer.validateHandshake tracker
          case maybeHandshakeResponse of
            Just (Peer.PeerResponse _ (Peer.PeerId peer_id)) -> do
              let handshake = Peer.trackerToPeerHandshake tracker
              sendAll conn handshake
              sendAll conn Peer.interested
              sendAll conn Peer.unchoke
              -- 2. send bitmap
              print $ "Peer " ++ (show peer) ++ " starting recvLoop"
              time <- Clock.getTime Clock.Monotonic
              let peerState = Peer.PeerState (Peer.PeerId peer_id)
                                             (Peer.Conn conn)
                                             (Peer.initPieceMap tracker)
                                             (Peer.Chans (workChan, responseChan))
                                             (Peer.RPCParse Peer.defaultPeerRPCParse)
                                             Nothing
                                             (Peer.PeerChoked False)
                                             (Peer.PeerChoking True)
                                             (Shared.PeerThreadId (UTF8.fromString $ show peer))
                                             (Peer.TimeStamps ((Peer.LastHeartbeat Nothing),
                                                               (Peer.LastKeepAlive time)))
                                             []
                    -- TODO: You may need to close the connection if this fails, not sure of the consequences of this if I don't close it.
              E.catch (Peer.recvLoop tracker peerState) (\e ->
                                                      print $ "SERVER PEER " ++ (show peer) ++ " HIT EXCEPTION " ++ (show (e :: E.SomeException) )
                                                  )
            Nothing -> do
              print $ "Peer " ++ (show peer) ++ " got invalid handshake response " ++ (show maybeHandshakeResponse)
              return ()
