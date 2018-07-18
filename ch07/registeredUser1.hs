module RegisteredUser where

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

data WherePenguinsLive = Galapagos
                       | Antarctica
                       | Australia
                       | SouthAfrica
                       | SouthAmerica
                       deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Show, Ord)

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber accountNumber)) =
  putStrLn $ name ++ " " ++ show accountNumber

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

k :: (a, b) -> a
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1+2))
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a,d),(c,f))
f (a, _, c) (d, _, f) = ((a,d),(c,f))

functionC x y = if (x > y) then x else y
functionD x y = case x > y of
                  True -> x
                  False -> y

ifEvenAdd2A n = if even n then (n+2) else n
ifEvenAdd2B n = case even n of
                 True -> (n+2)
                 False -> n
nums x = case compare x 0 of
           GT -> -1
           LT -> -1
           EQ -> 0

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip g b a = g a b

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank com e e' =
  case com e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

avgGuard :: (Fractional a, Ord a) => a -> Char
avgGuard x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59  = 'E'
  where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise        = False

numbers :: (Ord a, Eq a, Num a) => a -> a
numbers n
  | n < 0  = -1
  | n == 0 = 0
  | n > 0  = 1

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)


addOne :: Int -> Int
addOne x = x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

tensDigit :: Integral a => a -> a
--tensDigit x = d
--  where xLast = x `div` 10
--        d = xLast `mod` 10

tensDigit = fst . flip divMod 10
hDigit = fst . flip divMod 100

foldBool1 :: a -> a -> Bool -> a
foldBool1 a b bool = case bool of
  True -> a
  False -> b

foldBool2 :: a -> a -> Bool -> a
foldBool2 a b bool
  | bool == True = a
  | bool == False = b

foldBool3 :: a -> a -> Bool -> a
foldBool3 a _ True = a
foldBool3 _ b False = b

foldBool4 :: a -> a -> Bool -> a
foldBool4 a b bool = if bool then a else b

g :: (a -> b) -> (a, c) -> (b, c)
g ab (a,c) = (ab a, c)

main :: IO ()
main = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne 0)
  print (addOnePF 0)
  print ((addOne . addOne) 0)

  print ((addOnePF . addOne) 0 )
  print ((addOne . addOnePF) 0 )
  print ((addOnePF . addOnePF) 0 )
  print (negate  (addOnePF 0))
  print ((negate . addOnePF) 0)
  print ((addOne . addOne . addOne . negate . addOne) 0)
