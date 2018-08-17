module Lib where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import qualified Data.DList as DL


someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Die = DieOne
         | DieTwo
         | DieThree | DieFour
         | DieFive
         | DieSix
         deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got an int outside of 1 - 6: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
  where go sum count gen
          | sum >= limit = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = rollsToGetN 20 g

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit g = go 0 (0,  []) g
  where go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum (count, xs) g
                | sum >= limit = (count, xs)
                | otherwise = let (newRoll, newG) = randomR (1, 6) g
                              in go (sum + newRoll) (count + 1, (intToDie newRoll):xs) newG

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
--  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f s = Moi (\x -> let (a, y) = (runMoi s) x
                        in (f a, y))

-- I don't know if this is correct / if I really need to have the monoid constraint in order to get these to work. If I don't then I'm not sure how to compose the s values
instance Applicative (Moi s) where
  pure a = Moi $ (\s -> (a, s))
  --(<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
--  (<*>) (Moi f) (Moi g) =  Moi (\x -> (runMoi $ fmap g f) x)
--  s -> ((a -> b), s)
--  s -> (a, s)
  (<*>) f g = Moi (\s -> let (a2b, newS1) = runMoi f s
                             (a, newS2) = runMoi g newS1
                         in (a2b a, newS2))

instance Monad (Moi s) where
  return = pure
  -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (>>=) f g = Moi $ \s ->
                  let (a, newS) = (runMoi f) s
                      (b, newNewS) = (runMoi $ g a) newS
                  in (b, newNewS)


fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n`mod`5 == 0 = "Buzz"
  | n`mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

-- main :: IO ()
-- main = mapM_ putStrLn $ fizzBuzzList [1..100]

moiGet :: Moi s s
moiGet = Moi $ \x -> (x, x)

moiPut :: s -> Moi s ()
moiPut s = Moi $ const ((), s)

moiExec :: Moi s a -> s -> s
moiExec m = snd . runMoi m

moiEval :: Moi s a -> s -> a
moiEval m = fst . runMoi m

moiModify :: (s -> s) -> Moi s ()
moiModify f = Moi $ \s -> ((), f s)
