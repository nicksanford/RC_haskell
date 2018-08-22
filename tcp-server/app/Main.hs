-- Echo server program
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = withSocketsDo $ do
  -- get your address info for port 3000
  addr <- resolve "3000"
  -- create a socket on port 3000 using that address info and bind to it
  -- once that is done, we set some security stuff and listen for 10 connections??? 
  -- we then enter a loop which accepts the socket (what does it get back?) and forks a thread to handle talking to the socket
  -- that thread itself is also a loop
  -- if the thread hits an error it closes the connection
  -- The child thread waits (I think???) for 1024 bytes from the initiator and responds with (sendAll not sure what this does
  -- This will keep all connections open until an exception is thrown, I wonder  if one will be thrown if I kill the client connection??
  -- what does void do?
  E.bracket (open addr) close loop
  where
    resolve port = do
      let hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
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
      void $ forkFinally (talk conn) (\_ -> print ("closing " ++ (show conn) ++ "from" ++ (show peer)) >> close conn)
    talk conn = do
      print "waiting for some stuff"
      msg <- recv conn 1024
      print (S.concat ["got some stuff", msg])
      unless (S.null msg) $ do
        sendAll conn msg
        talk conn
