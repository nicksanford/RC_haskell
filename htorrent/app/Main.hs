{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent   (forkIO)

import qualified Data.ByteString      as BS
import qualified System.Environment   as SE

import qualified Lib
import qualified Server
import System.IO
import Control.Monad
import Data.Char
import System.Exit
import Control.Concurrent.Chan (newChan, readChan)

host :: BS.ByteString
host = "0.0.0.0"

port :: Integer
port = 6862

main :: IO ()
main = do
  args <- SE.getArgs
  killChan <- newChan
  _ <- forkIO $ Lib.run  (head args) port killChan
  _ <- forkIO $ Server.run (show port)
  exitOnQ killChan

exitOnQ killChan = do
    response <- readChan killChan
    exitSuccess
