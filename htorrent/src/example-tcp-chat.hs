{-# LANGUAGE OverloadedStrings #-}
module Server (run) where

import Network.Socket
import Control.Exception (SomeException (..), handle)
import Control.Concurrent (Chan (..), forkIO, killThread, dupChan, readChan, writeChan, newChan)
import Control.Monad.Fix (fix)
import Control.Monad (when)
import System.IO (hClose, hPutStrLn, hGetLine, IOMode (..), hSetBuffering, BufferMode (..))
import Text.Printf (printf)
import qualified Data.ByteString as BS

type Msg = (Int, String)

run :: String -> Integer -> IO ()
run host port =  do
  printf "Running, and listening on host: %s, port %s\n" host (show port)
  printf "HINT: run echo \"did you get this message?\"|  nc localhost %s\n" (show port)
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (read $ show port) iNADDR_ANY)
  listen sock 2
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum =  do
  let broadcast msg = writeChan chan (msgNum, msg)
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  hPutStrLn hdl "H! What is your name?"
  name <- init <$> hGetLine hdl
  broadcast ("-->" ++ name ++ " entered chat.")
  hPutStrLn hdl ("Welcome " ++ name ++ "!")

  commLine <- dupChan chan

  reader <- forkIO $ fix $ \loop -> do
    (nextNum, line) <- readChan commLine
    when (msgNum /= nextNum) $ hPutStrLn hdl line
    loop

  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- fmap init (hGetLine hdl)
    case line of 
      "quit" -> hPutStrLn hdl "Bye!"
      _ -> broadcast (name ++ ": " ++ line) >> loop

  killThread reader
  broadcast ("<-- " ++ name ++ "left.")
  hClose hdl

