module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = (read . show)

main = do
  print ((roundTrip 4) :: Double)
  print  .id $ 4
