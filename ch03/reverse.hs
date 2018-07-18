module Reverse where

rvrs :: String -> String
rvrs x = c ++ " " ++ b ++ " " ++ a
  where a = take 5 x
        b = take 2 $ drop 6 x
        c = drop 9 x

main :: IO ()
main = do
  print $ rvrs "Curry is awesome"
