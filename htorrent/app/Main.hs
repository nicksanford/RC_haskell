module Main where
import qualified Data.ByteString.Char8 as BS
import System.Environment

import BEncode (decode)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] ->
      BS.readFile arg >>= putStr . show . decode . BS.unpack
    _ ->
      putStrLn $ "ERROR: you provided two arguments: " ++ show args ++ " htorrent requires one"
