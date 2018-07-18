module Cipher where

import Data.Char

--map (\x -> (((ord x) - 97), x)) ['a'..'z']

lowercaseCharMod :: Int
lowercaseCharMod =
  26

lowerCaseBase :: Int
lowerCaseBase = 97

cesarChar :: Int -> Char -> Char
cesarChar offset x = chr moddedWithBaseBack
  where charBaseOrd = (ord x) - lowerCaseBase
        rightShiftedOrd = charBaseOrd + offset
        moddedWithBaseBack = (rightShiftedOrd `mod`  lowercaseCharMod) + lowerCaseBase

unCesarChar :: Int -> Char -> Char
unCesarChar offset x = chr moddedWithBaseBack
  where charBaseOrd = (ord x) - lowerCaseBase
        leftShiftedOrd = charBaseOrd - offset
        moddedWithBaseBack = (leftShiftedOrd `mod`  lowercaseCharMod) + lowerCaseBase

cesar :: Int -> String -> String
cesar offset = map (cesarChar offset)

unCesar :: Int -> String -> String
unCesar offset = map (unCesarChar offset)

charShift :: Char -> Int
charShift c = (ord c) - 97

shiftList :: String -> [Int]
shiftList string = map charShift ((concat . repeat) string)

type VigenereKeyword = String
vigenere :: VigenereKeyword -> String -> String
vigenere keyword string = unwords $ map cesarWord charOffsetByWord
  where charOffsetByWord = map (zip (shiftList keyword)) (words string)
        cesarWord = map (\(offset, ch) -> cesarChar offset ch)

--map (zip shiftlist) (words string)
unVigenere :: VigenereKeyword -> String -> String
unVigenere = undefined

data Option = Encrypt | Decrypt deriving (Show, Eq)
data CypherOption = Ces | Vig deriving (Show, Eq)

inputToOption :: String -> Maybe Option
inputToOption input = case (unwords $ words input) of
  "e" -> Just Encrypt
  "d" -> Just Decrypt
  _   -> Nothing

inputToCypherOption :: String -> Maybe CypherOption
inputToCypherOption input = case (unwords $ words input) of
  "c" -> Just Ces
  "v" -> Just Vig
  _   -> Nothing

