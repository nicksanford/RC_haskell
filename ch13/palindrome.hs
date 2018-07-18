module Palindrome where

import Control.Monad (forever)
import System.Exit   (exitSuccess)
import Data.Char (isAlpha, toLower)

palendrome :: IO ()
palendrome = forever $ do
  rawLine <- getLine
  let line = filter isAlpha $ map toLower rawLine
  if line == reverse line
  then putStrLn "It's a palindrome"
  else  do putStrLn "Nope"
           exitSuccess

main :: IO ()
main = palendrome
