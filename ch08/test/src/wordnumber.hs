module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "invalid"


digits :: Int -> [Int]
digits n = go [] n
  where go xs a
          | a < 10    = (a:xs)
          | otherwise = go ((mod a 10):xs) (div a 10)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord (digits n)

half :: Fractional a => a -> a
half x = x / 2

