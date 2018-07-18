module Examples where

import Data.List (foldl')

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z []     = z
myFoldr f z (x:xs) = f x (foldr f z xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

mySum :: Num a => [a] -> a
mySum = foldr (+) 0

myProduct :: Num a => [a] -> a
myProduct = foldr (*) 1

myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x -> (f x :)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = myFoldr (\x acc -> if f x then x:acc else acc) []

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing     -> []
  Just (a,b') -> a : myUnfoldr f b'

myIterate :: (a -> a) -> a -> [a]
myIterate f = myUnfoldr  (\x -> Just (x, f x))

fibs :: Num a => [a]
fibs = myUnfoldr (\(a,b) -> Just (a, (b, a + b))) (0, 1)

-- Trees!
data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Show, Eq, Ord)


unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Nothing            -> Leaf
    (Just (a1, b, a2)) -> Node (unfold f a1) b (unfold f a2)

buildTreePlus :: Integer -> BinaryTree Integer
buildTreePlus a = unfold f 0
  where f i = if i >= a
              then Nothing
              else Just (i+1,i,i+1)

buildTreePlus3 = buildTreePlus 3
--                  0
--                 / \
--                1   1
--               / \ / \
--              2  2 2  2

buildTreeMinus :: Integer -> BinaryTree Integer
buildTreeMinus a = unfold f a
  where f i = if i <= 0
              then Nothing
              else Just (i-1,i,i-1)

buildTreeMinus3 = buildTreePlus 3
--                  3
--                 / \
--                2   2
--               / \ / \
--              3  3 3  3
