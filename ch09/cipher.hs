module Cipher where

import Data.Char


--map (\x -> (((ord x) - 97), x)) ['a'..'z']

lowercaseCharMod :: Int
lowercaseCharMod = 26



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
data CypherOption = C | V deriving (Show, Eq)

inputToOption :: String -> Maybe Option
inputToOption input = case (unwords $ words input) of
  "e" -> Just Encrypt
  "d" -> Just Decrypt
  _   -> Nothing

inputToCypherOption :: String -> Maybe Option
inputToCypherOption input = case (unwords $ words input) of
  "c" -> Just C
  "v" -> Just V
  _   -> Nothing

main = do
  putStrLn "Encrypt or decrypt (e|d)"
  optionInput <- getLine
  let option = inputToOption optionInput

  case option of
    Nothing ->
      putStrLn $ optionInput ++ "is an invalid option, please select e (encrypt) or d (decrypt)"
    (Just Encrypt) -> do
      putStrLn "Please input your key"
      keyInput <- getLine
      putStrLn "Please input message"
      msgInput <- getLine
      putStrLn $ vigenere keyInput msgInput
    (Just Decrypt) -> do
      putStrLn "Please input your key"
      keyInput <- getLine
      putStrLn "Please input message"
      msgInput <- getLine
      putStrLn $ vigenere keyInput msgInput
