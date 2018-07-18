module Examples where

import Data.Time

xs = map show [1..5]
y = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs

data DataBaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DataBaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5  1) (secondsToDiffTime 34123))
  ]

f :: DataBaseItem -> Bool
f (DbDate _) = True
f _          = False

filterDbDate :: [DataBaseItem] -> [UTCTime]
filterDbDate d = map (\(DbDate utcTime) -> utcTime) $ filter f d

f2 :: DataBaseItem -> Bool
f2 (DbNumber _) = True
f2 _            = False

filterDbNumber :: [DataBaseItem] -> [Integer]
filterDbNumber d = map (\(DbNumber i) -> i) $ filter f2 d

mostRecent :: [DataBaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DataBaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DataBaseItem] -> Double
avgDb d = dbSum / dbCount
  where dbSum = fromIntegral $ sumDb d
        dbCount = (fromIntegral . length) $ filterDbNumber d

fibs :: Num a => [a]
fibs = 1 : scanl (+) 1 fibs

fact :: (Num a, Enum a) => [a]
fact = scanl (\acc x -> x * acc) 1 [2..]

stops :: String
stops = "pbtdkg"

vouls :: String
vouls = "aeiou"

ex1 :: [(Char, Char, Char)]
ex1 = [(s1,v,s2) | s1 <- stops, v <- vouls, s2 <- stops]

ex2 :: [(Char, Char, Char)]
ex2 = [(s1,v,s2) | s1 <- stops, v <- vouls, s2 <- stops, s1 == 'p']

avgWordLength :: Fractional a => String -> a
avgWordLength x = (sum (map (fromIntegral . length) (words x))) / ((fromIntegral . length) (words x))

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr (\x acc -> f x && acc) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\y acc -> if y == x then True else acc) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (==x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x: acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x:acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\y acc -> if f y acc == GT then y else acc) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\y acc -> if f y acc == LT then y else acc) x xs
