{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified System.Environment as SE

import qualified BEncode
import qualified Lib
import qualified Server
import qualified Utils

host :: BS.ByteString
host = "0.0.0.0"

port :: Integer
port = 6882

main :: IO ()
main = do
  args <- SE.getArgs
  forkIO $ Lib.run  (head args) host port
  Server.run (UTF8.toString host) (show port)
