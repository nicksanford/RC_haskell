{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified System.Environment as SE

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as M
import Lib 
import BEncode

main :: IO ()
main = do exec >>= print

run :: IO ()
run = do
  args <- SE.getArgs
  let readFilePath = head args
  let writeFilePath = head $ tail args
  maybeBencode <- f <$> maybeReadBencode readFilePath
  case maybeBencode of
    Just b ->
      BS.writeFile writeFilePath b
    Nothing ->
      putStrLn "ERROR: Hit an error"
  where f :: Maybe BEncode -> Maybe BS.ByteString
        f maybebencode = maybebencode >>= bencodeToMaybeDict >>= (M.lookup (BString "info")) >>= (return . encode)

run2 :: IO ()
run2 = do
  args <- SE.getArgs
  let filePath = head args
  peer_id <- getPeerID
  maybeBencode <- maybeReadBencode filePath
  case maybeBencode >>= toTracker of
    Just tracker ->
      trackerRequest peer_id tracker >>= print
    Nothing ->
      print "ERROR: Hit an error"


exec :: IO (Maybe String)
exec = do
  args <- SE.getArgs
  let filePath = head args
  peer_id <- getPeerID
  maybeBencode <- maybeReadBencode filePath
  traverse (trackerRequest peer_id) (maybeBencode >>= toTracker)

test :: IO ()
test = do
  args <- SE.getArgs
  let filePath = head args
  let url = head $ tail args
  peer_id <- getPeerID
  maybeBencode <- maybeReadBencode filePath
  case maybeBencode >>= toTracker of
    Just tracker ->
      trackerRequestTest (UTF8.fromString url) peer_id tracker >>= putStrLn 
    Nothing ->
      putStrLn "ERROR: Hit an error"



  
-- main = do
--   args <- getArgs
--   case args of
--     [arg] ->
--       BS.readFile arg >>= putStr . show . decode . BS.unpack
--     _ ->
--       putStrLn $ "ERROR: you provided two arguments: " ++ show args ++ " htorrent requires one"
