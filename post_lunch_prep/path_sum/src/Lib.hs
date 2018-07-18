module Lib where

data Tree a = Node (Tree a) a (Tree a) | Leaf deriving (Show, Eq)

ourTree :: Tree Integer
--ourTree = Node Leaf 22 Leaf--(Node (Node Leaf 13 Leaf) 8 (Node (Node Leaf 5 Leaf) 4 (Node Leaf 1 Leaf)))
--ourTree = Node (Node (Node (Node Leaf 2 Leaf) 11 Leaf) 4 Leaf) 5 Leaf--(Node (Node Leaf 13 Leaf) 8 (Node (Node Leaf 5 Leaf) 4 (Node Leaf 1 Leaf)))
ourTree = Node (Node (Node (Node Leaf 7 Leaf) 11 (Node (Node Leaf 0 Leaf) 2 Leaf)) 4 Leaf) 5 (Node (Node Leaf 13 Leaf) 8 (Node (Node Leaf 5 Leaf) 4 (Node Leaf 1 Leaf)))

findSumPaths :: Integer -> Tree Integer -> [[Integer]]
findSumPaths querySum tree = snd (go querySum tree ([], []))
  where go :: Integer -> Tree Integer -> ([Integer], [[Integer]]) -> ([Integer], [[Integer]])
        go s (Node Leaf value Leaf) acc@(pathSoFar, validPaths) =
          if sum (value:pathSoFar) == s
          then (pathSoFar, (reverse (value:pathSoFar)) : validPaths)
          else acc

        go s (Node left value right) acc@(pathSoFar, validPaths) =
          if sum (value:pathSoFar) > s
          then acc
          else ([], (++) (snd $ go s left (value:pathSoFar, validPaths))
                         (snd $ go s right (value:pathSoFar, validPaths)))

        go s Leaf acc@(pathSoFar, validPaths) =
          if sum pathSoFar == s
          then (pathSoFar, reverse pathSoFar : validPaths)
          else acc

test :: [[Integer]]
test = findSumPaths 22 ourTree
  
