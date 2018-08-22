{-# LANGUAGE OverloadedStrings #-}
-- Echo client program 
module Main where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad (unless)

main :: IO ()
main = withSocketsDo $ do
  addr <- resolve "127.0.0.1" "3000"
  E.bracket (open addr) (\x -> print "closing" >> close x) talk
  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port) -- this gets the address info of the peer you want to connect to 
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr) -- this opens the remote socket
      connect sock $ addrAddress addr -- this connnects to the remote socket
      return sock
    talk sock = do
      sendAll sock "Hello, world!"
      loop sock
    loop sock = do
      putStrLn "waiting "
      msg <- recv sock 1024
      unless (S.null msg) $ do
        putStrLn "Received: "
        C.putStrLn msg
        loop sock
