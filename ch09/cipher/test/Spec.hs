module Main where

import Test.QuickCheck
import Test.Hspec

import Cipher

data LowerAlphanumChars = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Eq, Show)

instance Arbitrary LowerAlphanumChars where
  arbitrary = elements [A , B , C , D , E , F , G , H , I , J , K , L , M , N , O , P , Q , R , S , T , U , V , W , X , Y , Z]

chToChar :: LowerAlphanumChars -> Char
chToChar A = 'a'
chToChar B = 'b'
chToChar C = 'c'
chToChar D = 'd'
chToChar E = 'e'
chToChar F = 'f'
chToChar G = 'g'
chToChar H = 'h'
chToChar I = 'i'
chToChar J = 'j'
chToChar K = 'k'
chToChar L = 'l'
chToChar M = 'm'
chToChar N = 'n'
chToChar O = 'o'
chToChar P = 'p'
chToChar Q = 'q'
chToChar R = 'r'
chToChar S = 's'
chToChar T = 't'
chToChar U = 'u'
chToChar V = 'v'
chToChar W = 'w'
chToChar X = 'x'
chToChar Y = 'y'
chToChar Z = 'z'

cesarIdentity :: (Int, [LowerAlphanumChars]) -> Bool
cesarIdentity (i, lowerAlphanumChars) = (unCesar i (cesar i string)) == string
  where string = map chToChar lowerAlphanumChars

main :: IO ()
main = hspec $ do
  describe "chToChar" $ do
    it "converts LowerAlphanumChars to chars" $ do
      map chToChar [A , B , C , D , E , F , G , H , I , J , K , L , M , N , O , P , Q , R , S , T , U , V , W , X , Y , Z] `shouldBe` "abcdefghijklmnopqrstuvwxyz"
  describe "cesar" $ do
    it "it has the identity uncesar. cesar" $ do
      property cesarIdentity
  
