-- Echo server program
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkFinally, threadDelay) -- threadWaitRead, threadWaitWrite
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.UTF8 as UTF8
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, sendAll, send)

main :: IO ()
main = withSocketsDo $ do
  -- get your address info for port 3000
  addr <- resolve "3000"
  -- create a socket on port 3000 using that address info and bind to it
  -- once that is done, we set some security stuff and listen for 10 connections??? 
  -- we then enter a loop which accepts the socket (what does it get back?) and forks a thread to handle talking to the socket
  -- that thread itself is also a loop
  -- The child thread waits (I think???) for 1024 bytes from the initiator and responds with (sendAll not sure what this does
  -- This will keep all connections open until an exception is thrown, I wonder  if one will be thrown if I kill the client connection??
  -- what does void do?
  -- I think that actually what is happening is that when the child thread stops receiving data (as denoted by S.null msg) it will stop recursing through talk. That will trigger the forkFinally to execute it's last parameter, which will close the connection.
  -- maybe I can start a simple try keeping all connections open and send a 'Hi! <address>' every 5 second:.
  E.bracket (open addr) close loop
  where
    resolve port = do
      let hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just port) -- what does getAddrInfo do?
      print addr
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr) -- what is an addrFamily?, what is a socketType? what is Protocol in this case?
      print sock -- where can I find out about these sockeets which exest?
      setSocketOption sock ReuseAddr 1 -- why is this needed?
      bind sock (addrAddress addr)
      -- if the prefork technique is not used,
      -- set CloseOnExec for the security reasons.
      let fd = fdSocket sock -- how do I get a file descriptor from a socket?  Is here a file on my system which I can find which maps to this value? Do sockets themselves have files? What information can I get  from looking at those files?
      setCloseOnExecIfNeeded fd -- I don't know what this is doing
      listen sock 10 -- what exactly does this mean?
      return sock
    loop sock = forever $ do
      (conn, peer) <- accept sock
      putStrLn $ "Connection from " ++ show peer
      void $ forkFinally (talk conn peer) (\_ -> print ("closing " ++ (show conn) ++ "from" ++ (show peer)) >> close conn)
    talk conn peer = do
      print "waiting for some stuff"
      msg <- recv conn 1024
      print (S.concat ["got some stuff: ", msg, "from ", (UTF8.fromString $ show peer)])
      unless (S.null msg) $ do
        send conn "I'm going to start saying hi"
        delayLoop conn 0

    delayLoop conn i = do
      sendAll conn $ S.concat ["hi! ", UTF8.fromString $ show i]
      threadDelay 5000000
      delayLoop conn (i+1)

