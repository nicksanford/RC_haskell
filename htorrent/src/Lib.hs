{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified BEncode
import qualified Peer
import Tracker (trackerRequest, toTracker, TrackerResponse (..), Peers (..))
import Utils (getPeerID)

import qualified Data.ByteString as BS

import Text.Printf (printf)

run :: String -> BS.ByteString -> Integer -> IO ()
run filename host port = do
  peer_id <- getPeerID
  maybeBencode <- BEncode.maybeReadBencode filename
  let maybeTracker = maybeBencode >>= toTracker peer_id
  case maybeTracker of
    Right tracker -> do
      maybeTrackerResponse <- trackerRequest tracker host port
      case maybeTrackerResponse of
        Nothing -> do
          print "got empty tracker response"
          return ()
        (Just t@(TrackerResponse (Peers peers) _ _ _ _ _ _)) -> do
              printf "got to just tracker response %s" $ show t
              let maybePeers :: [IO (Maybe Peer.PeerResponse)]
                  maybePeers = (fmap (Peer.initiateHandshake tracker)  peers)
              responses <- head maybePeers
              print "this is the response"
              print responses
    Left error ->
      print error

