#!/usr/bin/env stack
-- stack --resolver lts-12.2 script

import qualified System.Environment as Sys
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Crypto.Hash as C

-- import Data.ByteArray as BA
-- import Data.ByteArray.Encoding as BAE
-- hash = C.hash h :: C.Digest C.SHA1
-- bytestringhash = BAE.convertToBase BAE.Base16 (BA.convert hash :: BS.ByteString) :: BS.ByteString 
-- =>"aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"
-- h = Prelude.fst $ Prelude.head $ N.readHex $ show hash
-- => 975987071262755080377722350727279193143145743181
--


main :: IO ()
main = do
  args <- Sys.getArgs
  file <- BS.readFile $ head args
  let hash = (C.hash file :: C.Digest C.SHA1)

