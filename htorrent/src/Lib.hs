{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified BEncode
import qualified Peer
import Tracker (trackerRequest, toTracker, TrackerResponse (..), Peers (..))
import Utils (getPeerID)

import qualified Data.ByteString as BS
import Control.Concurrent (forkFinally)

import Text.Printf (printf)

run :: String -> BS.ByteString -> Integer -> IO ()
run filename host port = do
  peer_id <- getPeerID
  maybeBencode <- BEncode.maybeReadBencode filename
  let maybeTracker = maybeBencode >>= toTracker peer_id
  case maybeTracker of
    Right tracker -> do
-- TODO delete the host parameter
      printf "tracker: %s" (show tracker)
      maybeTrackerResponse <- trackerRequest tracker host port
      case maybeTrackerResponse of
        Nothing -> do
          print "got empty tracker response"
          return ()
        (Just t@(TrackerResponse (Peers peers) _ _ _ _ _ _)) -> do
              printf "got to just tracker response %s" $ show t
              print "spawning child threads for peers"
              mapM_ (\peer ->
                      forkFinally (Peer.startPeer tracker peer)
                                  (\x -> print $ "FORK FINALLY HIT ERROR ON START PEER: " ++ (show peer) ++ " " ++ (show x))
                     ) peers

              -- spawn a thread per peer with a channel for communicating with
              -- the main thread
--              let maybePeers :: [IO (Maybe Peer.PeerResponse)]
--                  maybePeers = (fmap (Peer.initiateHandshake tracker)  peers)
    Left error ->
      print error

