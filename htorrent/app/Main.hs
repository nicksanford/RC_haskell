{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent   (forkIO)

import qualified Data.ByteString      as BS
import qualified System.Environment   as SE

import qualified Lib
import qualified Server

host :: BS.ByteString
host = "0.0.0.0"

port :: Integer
port = 6882

main :: IO ()
main = do
  args <- SE.getArgs
  _ <- forkIO $ Lib.run  (head args) port
  Server.run (show port)
