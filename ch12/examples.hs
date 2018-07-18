module Examples where

-- Defined in prelude
--data Maybe a = Nothing | Just a

import Data.List
import Data.Char

type Name = String
type Age = Integer

type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show
--mkPerson :: Name -> Age -> ValidatePerson Person
--mkPerson name age
--  | name == "" && age < 0 = Left [NameEmpty, AgeTooLow]
--  | name == ""  = Left [NameEmpty]
--  | age < 0     = Left [AgeTooLow]
--  | otherwise   = Right $ Person name age


data PersonInvalid = NameEmpty
                   | AgeTooLow

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

vowels :: String
vowels = "aeiou"

instance Show PersonInvalid where
  show = toString

instance Eq PersonInvalid where
  (==) NameEmpty NameEmpty = True
  (==) AgeTooLow AgeTooLow = True
  (==) _ _                 = False

blah :: PersonInvalid -> String
blah p
  | p == NameEmpty = "NameEmpty"
  | p == AgeTooLow = "AgeTooLow"
  | otherwise = "???"


ageOkay :: Age -> ValidatePerson Age
ageOkay age
  | age >= 0 = Right age
  | otherwise = Left [AgeTooLow]

nameOkay :: Name -> ValidatePerson Name
nameOkay name
  | name /= "" = Right name
  | otherwise  = Left [NameEmpty]


mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name ->
             ValidatePerson Age ->
             ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) =  Left (badName ++ badAge)
mkPerson' (Left badName) _             =  Left badName
mkPerson' _              (Left badAge) =  Left badAge


data Example a = Blah | RoofGoats | Woot a deriving Show

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

notThe  :: String -> Maybe String
notThe "the" =  Nothing
notThe xs    = Just xs

foldRemoveNothing :: Maybe a -> [a] -> [a]
foldRemoveNothing Nothing acc = acc
foldRemoveNothing (Just a) acc = a:acc

replaceThe :: String -> String
replaceThe xs = unwords $ foldr (foldRemoveNothing . notThe) [] $ words xs

fCountTheBeforeVowel :: ([String], Integer) -> String -> ([String], Integer)
fCountTheBeforeVowel (listAcc@(lastWord:_), count) currentWord =
  case (notThe lastWord, (toLower . head) currentWord `elem` vowels) of
    (Nothing, True) -> (currentWord:listAcc, count+1)
    _               -> (currentWord:listAcc, count)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel xs = snd $ foldl' fCountTheBeforeVowel ([y],0) ys

  where (y:ys) = words xs

countVowels :: String -> Integer
countVowels s = (fromIntegral . length) $ filter ((`elem` vowels) . toLower) s

countCons :: String -> Integer
countCons s = (fromIntegral . length) $ filter (not . (`elem` vowels) . toLower) s

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | countVowels s >  countCons s = Nothing
  | otherwise = Just (Word' s)

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ go x Zero
    where go 0 acc = acc
          go i acc = go (i-1) (Succ acc)

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

isJust :: Maybe a -> Bool
isJust  = not . isNothing

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:_)  = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just a:xs) = a : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where f :: Maybe a -> Maybe [a] -> Maybe [a]
        f _        Nothing    = Nothing
        f Nothing  _          = Nothing
        f (Just a) (Just acc) = Just (a:acc)

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f :: Either a b -> [a] -> [a]
        f (Left a) acc = a:acc
        f _        acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f :: Either a b -> [b] -> [b]
        f (Right b) acc = b:acc
        f _        acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([],[])
  where f :: Either a b -> ([a], [b]) -> ([a], [b])
        f (Left a)  (as, bs) = (a:as, bs)
        f (Right b) (as, bs) = (as, b:bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _ ) = Nothing
eitherMaybe'' f (Right b ) = (Just . f) b

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (y:ys) = go (n+y) ys

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (y:ys) = go (n*y) ys


niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go xs [] = xs
        go acc (x:xs) = go (acc ++ x) xs

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing     -> []
  Just (a,b') -> a :myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr  (\x -> Just (x, f x))

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Show, Eq, Ord)
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = 
  case f a of
    Nothing            -> Leaf
    (Just (a1, b, a2)) -> Node (unfold f a1) b (unfold f a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild a = unfold f 0
  where f i = if i >= a then Nothing else Just (i+1,i,i+1)
