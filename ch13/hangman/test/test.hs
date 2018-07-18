module Main where

import Test.Hspec
--import Test.QuickCheck

import Hangman

--fillInCharacter :: Puzzle -> Char -> Puzzle
--fillInCharacter (Puzzle word filledInSoFar s) c =
--  Puzzle word newFilledInSoFar newGuesses
--  where zipper guessed wordChar guessChar = if wordChar == guessed
--                                            then Just wordChar
--                                            else guessChar
--        newFilledInSoFar = zipWith (zipper c) word filledInSoFar
--        newGuesses = if c `elem` s then s else c:s


-- -handleGuess :: Puzzle -> Char -> IO Puzzle
-- handleGuess puzzle guess = do
--   putStrLn $ "Your guess was: " ++ [guess]
--   case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
--       (_, True) -> do
--           putStrLn  "You already guessed that character, pick something else!"

--           return puzzle
--       (True, _) -> do
--           putStrLn "This character was in the word, filling in the word accordingly"

--           return (fillInCharacter puzzle guess)
--       (False, _) -> do
--           putStrLn "This character wasn't in the word, try again.'"

--           return (fillInCharacter puzzle guess)

main :: IO ()
main = hspec $ do
  describe "handleGuess" $ do
    it "when the character has already been guessed, regardless of whether it is in the word or not, it returns the same puzzle" $ do
      result <- (handleGuess (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['p','q']) 'q')
      result `shouldBe`  (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['p','q'])
    it "when the character is in the word, but has not been guessed yet, it shows the character and adds it to the list of guesses" $ do
      result <- (handleGuess (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['p','q']) 'h')
      result `shouldBe`  (Puzzle "hello" [Just 'h', Nothing, Nothing, Nothing, Nothing] ['h', 'p','q'])
    it "when the character is NOT in the word, and has not been guessed yet, it does not show the character and adds it to the guessed list" $ do
      result <- (handleGuess (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['p','q']) 'i')
      result `shouldBe`  (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['i', 'p','q'])

  describe "fillInCharacter" $ do
    it "adds the guessed character to guesses and does not reveal any characters if the character is not in the word" $ do
      (fillInCharacter (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] []) 'q')
        `shouldBe`
        (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['q'])
    it "adds the guessed character to guesses and does not reveal any characters if the character is not in the word" $ do
      (fillInCharacter (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['q']) 'p')
        `shouldBe`
        (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['p','q'])
    it "does not add characters already guessed to the guessed list" $ do
      (fillInCharacter (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['p','q']) 'p')
        `shouldBe`
        (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['p','q'])
    it "reveals characters when gessed correctly and adds them to the guessed list" $ do
      (fillInCharacter (Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] ['p','q']) 'h')
        `shouldBe`
        (Puzzle "hello" [Just 'h', Nothing, Nothing, Nothing, Nothing] ['h', 'p','q'])

      
