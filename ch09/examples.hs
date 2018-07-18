module Examples where

import Data.Char

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x

eft :: Enum a => a -> a -> [a]
eft a b = reverse $ go [] a b
  where go xs aa bb
          | (fromEnum aa) == (fromEnum bb) = (aa:xs)
          | (fromEnum aa) > (fromEnum bb) = []
          | otherwise = go (aa:xs) (succ aa) bb

toWords :: String -> [String]
toWords string = splitOn ' ' string

myLines :: String -> [String]
myLines string = splitOn '\n' string
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
shouldEqual = [ "Tyger Tyger, burning bright"
              , "In the forests of the night"
              , "What immortal hand or eye"
              , "Could frame thy fearful symmetry?"
              ]


splitOn _ [] = []
splitOn char string = word : splitOn char resetMinusSpace
  where word = (takeWhile (\x -> x /= char) string)
        resetMinusSpace = (dropWhile (\y -> y == char) (dropWhile (\x -> x /= char) string))

mySqrt = [x ^ 2 | x <- [1..10]]
test1 = [x | x <- mySqrt, rem x 2 == 0]
test2 = [(x, y) | x <- mySqrt, y <- mySqrt, x < 50, y > 50]
test3 = take 5 test2

myCube = [y ^ 3 | y <- [1..5]]
test4  = [(x,y)| x <- mySqrt, y <- myCube]
test5 = [(x,y) | (x,y) <- test4, x < 50 && y < 50]
test6 = length test5
test7 = filter (\x -> x `mod` 3 == 0) [1..30]
test8 = filter (\x -> not $ x `elem` ["the", "a", "an"]) $ words $ "the brown dog was a goof"

itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

myMap :: (a ->b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x       = x : myFilter f xs
  | otherwise = myFilter f xs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _          = []
myZipWith f _ []          = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip :: [a] -> [b] -> [(a,b)]
myZip =  myZipWith (,)

myAnd :: [Bool] -> Bool
myAnd  [] = True
myAnd (False:xs)  = False
myAnd (_:xs)  = myAnd xs

myOr :: [Bool] -> Bool
myOr  [] = False
myOr (True:xs)  = True
myOr (_:xs)  = myAnd xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x       = True
  | otherwise = myAny f xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x:xs)
  | f x = myAll f xs
  | otherwise = False

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs)
  | a == x    = True
  | otherwise = myElem a xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a = myAny (==a)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish xss = [x | xs <- xss, x <- xs]

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = [y | x <- xs, y <- f x]

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f x xs
  where go _    a []     = a
        go func a (b:bs) = case func a b of
          GT -> go func a bs
          EQ -> go func a bs
          LT -> go func b bs
          

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f x xs
  where go _    a []     = a
        go func a (b:bs) = case func a b of
          LT -> go func a bs
          GT -> go func b bs
          EQ -> go func b bs

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare

main :: IO ()
main =
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
