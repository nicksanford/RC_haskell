module Lib where

import qualified BEncode
import qualified Peer
import qualified FileManager
import Tracker (trackerRequest, toTracker, TrackerResponse (..), Peers (..), Peer)
import Utils (getPeerID)

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.Chan (newChan, writeChan, Chan)

errorHandler :: (Show a, Show b) => Peer -> Chan FileManager.ResponseMessage -> Either a b -> IO ()
errorHandler peer chan (Left x) = do
  writeChan chan $ FileManager.Error peer
  print $ "FORK FINALLY HIT ERROR ON START PEER: " ++ show peer ++ " " ++ show x
errorHandler peer chan (Right x) = do
  writeChan chan $ FileManager.Error peer
  print $ "FORK FINALLY SUCCEEDED: " ++ show peer ++ " " ++ show x

run :: String -> Integer -> Chan () -> IO ()
run filename port killChan = do
  peer_id <- getPeerID
  maybeBencode <- BEncode.maybeReadBencode filename
  let maybeTracker = maybeBencode >>= toTracker peer_id
  case maybeTracker of
    Right tracker -> do
      maybeTrackerResponse <- trackerRequest tracker port
      case maybeTrackerResponse of
        Nothing -> do
          print "got empty tracker response"
          return ()
        (Just trackerResponse@(TrackerResponse (Peers peers) _ _ _ _ _ _)) -> do
          workChan <- newChan
          responseChan <- newChan
          -- start file manager with read and response chans
          _ <- forkIO (FileManager.start tracker trackerResponse workChan responseChan peers killChan)
          putStrLn "spawning child threads for peers"
          print $ Peer.initPieceMap tracker
          mapM_ (\peer -> forkFinally (Peer.start tracker peer workChan responseChan) (errorHandler peer responseChan)) peers
    Left e ->
      print e
