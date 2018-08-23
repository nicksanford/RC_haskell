{-# LANGUAGE OverloadedStrings #-}
module Server (run) where

import Control.Concurrent (forkFinally, threadDelay) -- threadWaitRead, threadWaitWrite
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll, send)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

run :: String -> IO ()
run port =  do
  print "Running, and listening on port %s\n"
  printf "HINT: run echo \"did you get this message?\"|  nc localhost %s\n" port
  E.bracket(addrIO >>= open) close loop
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
          putStrLn $ "Connection " ++ (show conn) ++ " from " ++ (show peer)
          void $ forkFinally (talk conn peer) (\_ -> print ("closing " ++ (show conn) ++ "  from " ++ (show peer)) >> close conn)
        talk conn peer = do
          print $ "about to read " ++ (show conn) ++ "  from " ++ (show peer)
          msg <- recv conn 4096
          print $ BS.concat ["SERVER READ ", msg, " on ", (UTF8.fromString $ show conn), "  from ", (UTF8.fromString $ show peer)]
          unless (BS.null msg) $ do
            talk conn peer
