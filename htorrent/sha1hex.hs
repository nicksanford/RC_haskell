#!/usr/bin/env stack
-- stack --resolver lts-12.2 script

import qualified System.Environment as Sys
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Crypto.Hash as C


main :: IO ()
main = do
  args <- Sys.getArgs
  file <- BS.readFile $ head args
  print $ (C.hash file :: C.Digest C.SHA1)

