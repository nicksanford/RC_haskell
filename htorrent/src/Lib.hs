module Lib where

import qualified BEncode
import qualified Peer
import qualified FileManager
import Tracker (trackerRequest, toTracker, TrackerResponse (..), Peers (..))
import Utils (getPeerID)

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.Chan (newChan)

run :: String -> Integer -> IO ()
run filename port = do
  peer_id <- getPeerID
  maybeBencode <- BEncode.maybeReadBencode filename
  let maybeTracker = maybeBencode >>= toTracker peer_id
  case maybeTracker of
    Right tracker -> do
      --printf "tracker: %s" (show tracker)
      maybeTrackerResponse <- trackerRequest tracker port
      case maybeTrackerResponse of
        Nothing ->
          --print "got empty tracker response"
          return ()
        (Just trackerResponse@(TrackerResponse (Peers peers) _ _ _ _ _ _)) -> do
          workChan <- newChan
          responseChan <- newChan
          -- start file manager with read and response chans
          _ <- forkIO (FileManager.start tracker trackerResponse workChan responseChan)
          putStrLn "spawning child threads for peers"
          mapM_ (\peer ->
                  forkFinally (Peer.start tracker peer workChan responseChan)
                              (\x -> print $ "FORK FINALLY HIT ERROR ON START PEER: " ++ show peer ++ " " ++ show x)
                  ) $ take 3 peers
    Left e ->
      print e
