{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples where

import Data.Int
import Data.Char
import Data.List

--data L a = L | Cons a (L a) deriving Show

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge 

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[Int]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int

myDoge = DogueDeBordeaux 10

--badDoge :: DogueDeBordeaux String
--badDoge = DogueDeBordeaux 10
data Doggies a = Husky a
               | Mastiff a
               deriving (Show, Eq)
data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited
             deriving (Eq, Show)

type Size = Integer

data Vehicle = Car Manufacturer Price
             | Plane Size Airline
             deriving (Eq, Show)


isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _       = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

data Example0 = Example0 deriving (Eq,  Show)

data Example1 = Example1 Int deriving (Eq, Show)

data Example2 = Example2 Int String deriving (Eq, Show)

newtype Goats = Goats Int deriving (Show, Eq, TooMany)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, s) = (n + (length s)) > 42

instance TooMany (Int, Int) where
  tooMany (a, b) = (a + b) > 42

instance (Num a, TooMany a, Ord a) => TooMany (a, a) where
  tooMany (a, b) = (a + b) > 42

data BigSmall = Big Bool
              | Small Bool
              deriving (Eq, Show)

data NumberOrBool = Numba Int8
                  | BoolyBool Bool
                  deriving (Eq, Show)
data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving (Eq,  Show)

data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

--data Person = MkPerson String Int deriving (Eq,Show)
data Person = Person { name :: String
                     , age :: Int
                     }
  deriving (Eq, Show)

data Fiction = Fiction deriving (Show)
data NonFiction = NonFiction deriving (Show)

data BookType = FictionBook Fiction
              | NonFictionBook NonFiction
              deriving (Show)

type AuthorName = String

data Author = Author (AuthorName, BookType)

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lialac
                deriving Show


type Gardner = String

data Garden = Garden Gardner FlowerType deriving Show

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Show, Eq)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b
                }
                deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving  (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Show, Eq)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

--type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo
            deriving (Eq, Show)



trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product (Name "Simon") True

data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

myRecord :: RecordProduct Integer Float
--myRecord = RecordProduct 42 0.00001
myRecord = RecordProduct { pfirst = 42
                         , psecond = 0.0001
                         }

data OperatingSystem = GNU
                     | BSD
                     | Mac
                     | Win
                     deriving (Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
              deriving (Eq, Show)


data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgLang
                             }
                deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell
                        }

feelingWizarly :: Programmer
feelingWizarly = Programmer { lang = Agda
                            , os = GNU
                            }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GNU
                      , BSD
                      , Mac
                      , Win
                      ]
allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]                       
allProgrammers = [Programmer os lang | os <- allOperatingSystems,
                                       lang <- allLanguages]


data ThereYet = There Float Int Bool deriving (Eq, Show)

notYet :: Int -> Bool -> ThereYet
notYet = There (25.5 :: Float)

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yus :: ThereYet
yus = notQuite False

newtype Name = Name String deriving (Show, Eq)
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show
data FarmerRec = FarmerRec { fname :: Name
                           , acres :: Acres
                           , farmerType :: FarmerType
                           }

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _                        = False

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False

data Quantum = Yes
             | No
             | Both
             deriving (Eq, Show)

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

data DocVersion = DocVersion deriving Show

data EsResultFound a = EsResultFound { _version :: DocVersion
                                     , _sourece :: a
                                     }

data List a = Nil | Cons a (List a) deriving Show


data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a) deriving (Show, Eq, Ord)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)
insert' b _ = Node Leaf b Leaf

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = (Node (mapTree f left) (f a) (mapTree f right))

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (preorder left) ++ [a] ++ (preorder right)


testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (preorder left) ++ (preorder right) ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = (f a (foldTree f (foldTree f b left) right))

data Weekday = Monday
             | Tuesday
             | Wendsday
             | Thursday
             | Friday

asF :: Show a => (a, b) -> IO (a, b)
asF t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x:xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _      = True
isSubseqOf _  []     = False
isSubseqOf xss@(x:xs) (y:ys) = if x == y then isSubseqOf xs ys else isSubseqOf xss ys

capitalizeWords :: String -> [(String, String)]

capitalizeWords s = map (\x -> (x,  capitalizeWord x)) (words s)


capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (x:xs) = (toUpper x):xs

capitalizeParagraph :: String -> String
capitalizeParagraph s = reverse $ go True s []
  where go _     []     ys = ys
        go True  (x:xs) ys = if isAlpha x then go False xs (toUpper x:ys) else go True xs (x:ys)
        go False (x:xs) ys = if x == '.' then go True xs (x:ys) else go False xs (x:ys)

capitalizeParagraphFold :: (String, Bool) -> Char -> (String, Bool)
capitalizeParagraphFold (xs, True) x  = if isAlpha x then ((toUpper x:xs), False) else ((toUpper x:xs), True)
capitalizeParagraphFold (xs, False) x = if x == '.' then ((x:xs), True) else ((x:xs), False)

capitalizeParagraph' :: String -> String
capitalizeParagraph' s = reverse . fst $ foldl capitalizeParagraphFold ("", True) $ s

-- |1    |2ABC |3DEF |
-- |4GHI |5JKL |6MNO |
-- |7PQRS|8TUV |9WXYZ|
-- |*^   |0+_  |#.,  |

data DaPhone = DaPhone [(Digit, [Digit])]

phone :: DaPhone
phone = DaPhone [ ('1',"1"),
                  ('2', "abc2"),
                  ('3', "def3"),
                  ('4', "ghi4"),
                  ('5', "jkl5"),
                  ('6', "mno6"),
                  ('7', "pqrs7"),
                  ('8', "tuv8"),
                  ('9', "wxyz9"),
                  ('*', "^*"),
                  ('0', " 0"),
                  ('#', ".,#") ]

convo :: [String]
convo = [ "Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Just making sure rofl ur turn" ]

type Digit = Char
type Presses = Int

reverseTaps ::DaPhone -> Char -> [(Digit, Presses)]
reverseTaps daphone@(DaPhone p) char =
  if isAlpha char && isUpper char
  then ('*',1::Int):reverseTaps daphone (toLower char)
  else lowerCaseTaps
  where lowerCaseTaps =  [(digit, index+1::Int) | (digit, chars) <- p,
                                                  (index, c) <- (zip [0..] chars),
                                                  c == char]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap taps
  where taps = reverseTaps  p

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

count :: Eq a => [a] -> a -> Int
count xs a = length $ foldr (\x acc -> if (a == x) then x:acc else acc) [] xs

mostFrequent :: Eq a => [a] -> a
mostFrequent xs = fst $ maximumBy (\(a, aCount) (b, bCount) -> compare aCount bCount) $ [(x, count xs x)| x <- xs]

mostPopularLetter :: String -> Char
mostPopularLetter = mostFrequent

coolestLtr :: [String] -> Char
coolestLtr xs = mostPopularLetter $ concat xs

coolestWord :: [String] -> String
coolestWord xs = mostFrequent $ words $ concat $ intersperse " " xs

data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval = error "do it to it"

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)
