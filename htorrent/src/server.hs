{-# LANGUAGE OverloadedStrings #-}
module Server (run) where

import Control.Concurrent (forkFinally, threadDelay) -- threadWaitRead, threadWaitWrite
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll, send)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Text.Printf (printf)
import Data.Maybe (isJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

import qualified Peer as Peer
import qualified Tracker as Tracker

run :: String -> Tracker.Tracker -> IO ()
run port tracker = do
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

          void $ forkFinally (talk conn peer) (\_ -> print ("closing " ++ (show conn) ++ "  from " ++ (show peer)) >> close conn)

        loopTalk conn peer peerRPCParse = do
          -- 3. respond to requests
          -- 4. send have messages as you get more data
          sendAll conn Peer.keepAlive
          msg <- recv conn 16384
          let newPeerRPCParse@(Peer.PeerRPCParse _ maybeErrors _) = Peer.parseRPC tracker msg peerRPCParse
          print $ "LOOPTALK: newPeerRPCParse " ++ show newPeerRPCParse
          unless (BS.null msg || isJust maybeErrors) $ do
            print $ "LOOPTALK: ending " ++ (show $ BS.null msg) ++ " " ++ (show $ isJust maybeErrors)
            loopTalk conn peer newPeerRPCParse

        talk conn peer = do
          -- 1. send handshake
          -- 1.1. send interested
          -- 1.2. send unchoke
          msg <- recv conn 68
          let maybeHandshakeResponse = Peer.readHandShake conn msg >>= Peer.validateHandshake tracker
          case maybeHandshakeResponse of
            Just _ -> do
              let handshake = Peer.trackerToPeerHandshake tracker
              sendAll conn handshake
              sendAll conn Peer.interested
              sendAll conn Peer.unchoke
              -- 2. send bitmap
              print $ "Peer " ++ (show peer) ++ " starting loopTalk"
              loopTalk conn peer Peer.defaultPeerRPCParse
            Nothing -> do
              print $ "Peer " ++ (show peer) ++ " got invalid handshake response " ++ (show maybeHandshakeResponse)
              return ()
