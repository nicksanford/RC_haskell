module Fizz where
func :: Integer -> String
func x
  | x `mod` 15 == 0 = "fizzbuzz"
  | x `mod` 5 == 0 = "buzz"
  | x `mod` 3 == 0 = "fizz"
  | otherwise       = show x

fizz :: [Integer] -> [IO ()]
fizz ints = [putStrLn $ func i | i <- ints] 
