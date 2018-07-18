module Exercises where

addOne :: Num a => a -> a
addOne = \x -> x + 1

addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = case odd n of
  True -> (\x -> x + 1) n
  False -> n

addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x y -> (if x > y then y else x) + 5

myFlip f x y = f y x
