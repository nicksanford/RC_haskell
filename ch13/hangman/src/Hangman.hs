module Hangman where

import           Control.Monad (forever)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust, isNothing)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)
data Puzzle = Puzzle String [Maybe Char] [Char] deriving (Eq)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 7

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x) = x

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar newGuesses
  where zipper guessed wordChar guessChar = if wordChar == guessed
                                            then Just wordChar
                                            else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar
        newGuesses = if c `elem` s then s else c:s

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s nothings []
  where nothings = map (const Nothing) s

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = c `elem` s 

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) guess = guess `elem` guesses

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in      l >= minWordLength
               && l < maxWordLength

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess discovered guessed) =
  if ((length guessed) - (length $ filter isJust discovered) > 7)
      && (any isNothing discovered) then
    do putStrLn "You lose!"
       putStrLn $ "The world was: " ++ wordToGuess
       exitSuccess
   else return ()


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True) -> do
          putStrLn  "You already guessed that character, pick something else!"

          return puzzle
      (True, _) -> do
          putStrLn "This character was in the word, filling in the word accordingly"

          return (fillInCharacter puzzle guess)
      (False, _) -> do
          putStrLn "This character wasn't in the word, try again.'"

          return (fillInCharacter puzzle guess)

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is " ++ show puzzle
  putStr $ "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"
