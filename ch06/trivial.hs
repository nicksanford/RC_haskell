module Trivial where

import Data.List (sort)

data Trivial = Trivial' deriving Show

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show)

data  Date = Date DayOfWeek Integer

instance Eq DayOfWeek where
  Mon == Mon = True
  Tue == Tue = True
  Wed == Wed = True
  Thu == Thu = True
  Fri == Fri = True
  Sat == Sat = True
  Sun == Sun = True
  _ == _ = False

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ

instance Eq Date where
  (==) (Date dayOfWeek1 int1)
       (Date dayOfWeek2 int2) = dayOfWeek1 == dayOfWeek2
                                && int1 == int2
  
data Identity a = Identity a deriving Show

instance Eq a => Eq (Identity a) where
  (==) (Identity a') (Identity b') = a' == b'

data TisAnInteger = TisAn Integer
instance Eq (TisAnInteger) where
  (==) (TisAn a) (TisAn b) = a == b

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two a b) (Two aa bb) = a == aa && b == bb
  

data StringOrInt =
    TisAnInt Int
  | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

data Pair a = Pair a a 
instance (Eq a) => Eq (Pair a) where
  (==) (Pair a aa) (Pair b bb) = a == b && aa == bb

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a aa) (Tuple b bb) = a == b && aa == bb

data Which a = ThisOne a | ThatOne a
instance (Eq a ) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b)     = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _                     = False

data Mood = Blah | Woot deriving Eq

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

instance Show Mood where
  show Blah = "Blah"
  show Woot = "Blah"

data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

type Subject = String
type Verb = String
type Object = String

data Sentance = Sentance Subject Verb Object deriving (Show, Eq)

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

--i :: Num a => a
--i = 1

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' a = a

myX :: Int
myX = 1
sigmund' :: Int -> Int
sigmund' _ = myX

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: String -> String
mySort = sort

signifier :: String -> Char
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk atb a b = atb a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith atb i a =  atb a + fromIntegral i

