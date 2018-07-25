{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.ByteString.Lazy


test = do
  getMagnetLink "https://download.documentfoundation.org/libreoffice/stable/6.0.5/mac/x86_64/LibreOffice_6.0.5_MacOS_x86-64.dmg.magnet"

getMagnetLink :: String  -> IO (ByteString)
getMagnetLink link = do
  r <- get link
  let body = r ^.responseBody
  return body
