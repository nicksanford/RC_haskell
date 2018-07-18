module Main where

import Cipher

main :: IO ()
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
