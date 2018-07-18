module Factorial where

factorial :: (Eq a, Num a) => a -> a
factorial 1 = 1 
factorial n = (factorial a) * n
  where a = n - 1

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes times n = applyTimes times (+1) n

fib :: Integral a => a -> a
fib 0 = 1
fib 1 = 1
fib n = (fib  $ n - 2) + (fib  $ n - 1)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
data DividedResult = DividedByZero | Result Integer Integer deriving (Show)

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom = run num denom
  where go count f n d
         | d == 0      = DividedByZero
         | (n - d) < d = Result (f (toInteger (count + 1))) (toInteger (n - d))
         | otherwise   = go (count + 1) f (n - d) d

        run a b
         | a < 0 && b < 0    = go 0 id (-a) (-b)
         | a < 0 && b > 0    = go 0 negate (-a) (b) | a > 0 && b < 0    = go 0 negate (a) (-b)
         | otherwise         = go 0 id (a) (b)

func :: [a] -> [a] -> [a]
func x y = x ++ y

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appendCatty = cattyConny "woops"

frappe = flippy "haha"

sumTill n = go n
      where go a
              | a <= 0 = a
              | otherwise = a + go (a - 1)
multBy :: Integral a => a -> a -> a
multBy x y = go 0 0 x y
          where go count sum a b
                    | count == b = sum
                    | otherwise = go (count + 1) (sum + a) a b

mcCarthy n
  | n > 100  = n - 10
  | n <= 100 = mcCarthy $ mcCarthy (n+11)
