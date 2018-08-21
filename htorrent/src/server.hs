{-# LANGUAGE OverloadedStrings #-}
module Server (run) where

import Network.Simple.TCP (serve, HostPreference (Host), Socket, SockAddr, send, recv)
import Text.Printf (printf)
import qualified Data.ByteString as BS

conn :: (Socket, SockAddr) -> IO ()
conn (connectionSocket, remoteAddr) = do
  print $ "TCP connection established from " ++ show remoteAddr
  recv connectionSocket 1024 >>= mapM_ print

run :: String -> String -> IO ()
run host port =  do
  printf "Running, and listening on host: %s, port %s\n" host port
  printf "HINT: run echo \"did you get this message?\"|  nc localhost %s\n" port
  serve (Host host) port conn
