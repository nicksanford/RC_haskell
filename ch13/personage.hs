module PersonAge where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age


gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please input your:"

  putStrLn "name"
  name <- getLine

  putStrLn "age"
  rawAge <- getLine
  let age = read rawAge :: Integer

  case mkPerson name age of
    Right p ->
      putStrLn $ "Yay! We got a person: " ++ show p
    Left e ->
      putStrLn  $ "Boo! We got an error: " ++ show e
