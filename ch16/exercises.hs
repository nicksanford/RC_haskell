{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr

x1 :: Maybe Integer
x1 = fmap (+1) $ Just 1 -- => Just 2

x2 :: (Integer, Integer)
x2 = fmap (+1) $ (2,3) -- => (2,4)

x3 :: Either String String
x3 = fmap (++ "yo!") (Right "sup ") -- => "sup yo!"

x4 :: Either String String
x4 = fmap (++ "yo!") (Left "unaffected") -- => "sup yo!"

-- class Sumthin a where
--   s :: a -> a

-- class Else where
--   e :: b -> f (g a b c)

-- class Biffy where
--   slayer :: e a b -> (a -> c) -> (b -> d) -> e c d

--class Impish v where
--  impossibleKind :: v -> v a

--class AlsoImp v where
--  nope :: v a -> v

data FixMePls a = FixM a
                | Pls
              deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ Pls = Pls
  fmap f (FixM a) = FixM $ f a

-- Demo of how functors stack:
n = Nothing
w = Just "woohoo"
ave = Just "Ave"
lms = [ave, n, w]
replaceWithP = const 'p'

x5 = replaceWithP lms -- => 'p'

x6 = fmap replaceWithP lms -- => "ppp"
x7 = (fmap . fmap) replaceWithP lms -- => [Just 'p', Nothing, Just 'p']
x8 = (fmap . fmap . fmap) replaceWithP lms -- => [Just "ppp", Nothing, Just "pppppp"]

a1 = (+1) <$> read "[1]" :: [Int]
b1 = (fmap . fmap) (++ "lol")  (Just ["Hi,", "Hello"])
c1 = fmap (*2) (\x -> x - 2)
d1 = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
e1 :: IO Integer
e1 = let ioi = readIO "1" :: IO Integer
         changed = fmap (read . ("123" ++) .  show) ioi
     in fmap (*3) changed

fI :: (Eq (f a), Functor f) => f a -> Bool
fI x = (fmap id x) ==  id x

fC :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
fC x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

incLifted :: (Num a, Functor f) => f a -> f a
incLifted = fmap (+1)

showLifted :: (Show a, Functor f) => f a -> f String
showLifted = fmap show

newtype Identity a = Identity a deriving (Show, Eq)
newtype Identity2 a = Identity2 a deriving (Show, Eq)
newtype Identity3 a = Identity3 a deriving (Show, Eq)
data Pair a = Pair a a deriving (Show, Eq)
data Two a b = Two a b deriving (Show, Eq)
data Three a b c = Three a b c deriving (Show, Eq)
data Three' a b = Three' a b b deriving (Show, Eq)
data Four a b c d = Four a b c d deriving (Show, Eq)
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Identity) where
  fmap f (Identity a) = Identity $ f a

instance Functor (Identity2) where
  fmap f (Identity2 a) = Identity2 $ f a

instance Functor (Pair) where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c $ f d

type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

type IntToInt = Fun Int Int

type IdentFC = Identity Int ->
               IntToInt ->
               IntToInt ->
               Bool

type IdentFI = Identity Int ->
               Bool

type PairFC = Pair Int ->
              IntToInt ->
              IntToInt ->
              Bool

type PairFI = Pair Int ->
              Bool

type TwoFC = Two String Int ->
              IntToInt -> 
              IntToInt -> 
              Bool

type TwoFI = Two String Int ->
             Bool

type ThreeFC = Three String String Int ->
               IntToInt -> 
               IntToInt -> 
               Bool

type ThreeFI = Three String String Int ->
               Bool

type Three'FC = Three' String Int ->
                IntToInt -> 
                IntToInt -> 
                Bool

type Three'FI = Three' String Int ->
                Bool

type FourFC = Four [(Int, Char)] (Identity (Two Int String)) String Int ->
              IntToInt -> 
              IntToInt -> 
              Bool

type FourFI = Four [(Int, Char)] (Identity (Two Int String)) String Int ->
              Bool

type Four'FC = Four' (Identity (Two Int String)) Int ->
               IntToInt -> 
               IntToInt -> 
               Bool

type Four'FI = Four' (Identity (Two Int String)) Int ->
               Bool

type PossiblyFC = Possibly Int ->
                  IntToInt -> 
                  IntToInt -> 
                  Bool

type PossiblyFI = Possibly Int ->
                  Bool

type SumFC = Sum (Identity String) Int ->
             IntToInt ->
             IntToInt ->
             Bool

type SumFI = Sum (Identity String) Int ->
             Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

data Possibly a = LolNope
                | Yeppers a
                deriving (Show, Eq)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return $ LolNope)
              , (4, return $ Yeppers a)
              ]
data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ First a)
              , (4, return $ Second b)
              ]

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

getInt :: IO Int
getInt = fmap read getLine

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)
data BoolAndMaybeSomethingElse a = Falsish | Trueish a deriving (Eq, Show)
newtype Mu f  = InF { outF :: f (Mu f)}
data D = D (Array Word Word) Int Int deriving (Eq, Show)

data Sum' b a = First'' a
              | Second'' b
              deriving (Eq, Show)

instance Functor (Sum' e) where
  fmap _ (Second'' b) = Second'' b
  fmap f (First'' a) = First'' (f a)

data Company a c b = DeepBlue a c
                   | Something b
                   deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a
              | R b a b
              deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance
               | Desk a
               | Bloor b
               deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return Finance)
              , (1, return $ Desk a)
              , (3, return $ Bloor b)
              ]
type QuantFC = Quant String Int -> IntToInt -> IntToInt -> Bool
type QuantFI = Quant String Int -> Bool

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = (K a)

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return $ K a

type KFC = K String Int -> IntToInt -> IntToInt -> Bool
type KFI = K String Int -> Bool

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = do
    a <- arbitrary
    return $ Flip (K a)

type FlipKFC = Flip K String Int -> IntToInt -> IntToInt -> Bool
type FlipKFI = Flip K String Int -> Bool

data EvilGoateeConst a b = GoateeConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoateeConst b) = GoateeConst $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    return $ GoateeConst b

type EvilGoateeConstFC = EvilGoateeConst String Int -> IntToInt -> IntToInt -> Bool
type EvilGoateeConstFI = EvilGoateeConst String Int -> Bool

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut b) = LiftItOut $ fmap f b

-- TODO I don't know how to write an arbitrary instance for this, but this seems better than nothing? Maybe?
instance Arbitrary a => Arbitrary (LiftItOut Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ LiftItOut (Identity a)

type LiftItOutIdentityFC = LiftItOut Identity Int ->
                           IntToInt ->
                           IntToInt ->
                           Bool
type LiftItOutIdentityFI = LiftItOut Identity Int -> Bool

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap func (DaWrappa f g) = DaWrappa (fmap func f) (fmap func g)

instance Arbitrary a => Arbitrary (Parappa Identity Identity2 a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ DaWrappa (Identity a) (Identity2 b)

type ParappaIdentityFC = Parappa Identity Identity2 Int ->
                           IntToInt ->
                           IntToInt ->
                           Bool

type ParappaIdentityFI = Parappa Identity Identity2 Int -> Bool

data IgnoreOne f g a b = IgnoreSomething (f a) (g b) deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap func (IgnoreSomething f g) = IgnoreSomething f (fmap func g)

instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne Identity3 Identity2 a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ IgnoreSomething (Identity3 a) (Identity2 b)

type IgnoreOneIdentityFC = IgnoreOne Identity3 Identity2 String Int ->
                           IntToInt ->
                           IntToInt ->
                           Bool

type IgnoreOneIdentityFI = IgnoreOne Identity3 Identity2 String Int -> Bool

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious a b c) = Notorious a b (fmap f c)

instance (Arbitrary o, Arbitrary a, Arbitrary t) => Arbitrary (Notorious Identity o a t) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Notorious (Identity a) (Identity b) (Identity c)

type NotoriousIdentityFC = Notorious Identity [String] String Int ->
                           IntToInt ->
                           IntToInt ->
                           Bool

type NotoriousIdentityFI = Notorious Identity [String] String Int -> Bool

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    fmap (foldr Cons Nil) (listOf arbitrary)

-- sample $ (arbitrary :: Gen (List Int))
type ListFC = List Int->
              IntToInt ->
              IntToInt ->
              Bool

type ListFI = List Int -> Bool

data GoatLord a = NoGoat
                | OneGoat a
                | MoarGoats (GoatLord a)
                            (GoatLord a)
                            (GoatLord a)
                deriving (Show, Eq)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoarGoats a b c) = MoarGoats (fmap f a) (fmap f b) (fmap f c)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = do
    x <- arbitrary
    c <- resize 1 arbitrary
    d <- resize 1 arbitrary
    e <- resize 1 arbitrary
    resize 1 $ frequency [ (1, return $ NoGoat)
                         , (1, return $ OneGoat x)
                         , (1, return $ MoarGoats c d e) ]
type GoatLordFC = GoatLord Int->
                  IntToInt ->
                  IntToInt ->
                  Bool

type GoatLordFI = GoatLord Int -> Bool

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Read x) = Read (fmap f x)
  fmap f (Print s a) = Print s (f a)

main = do
  quickCheck (fC :: IdentFC)
  quickCheck (fI :: IdentFI)

  quickCheck (fC :: PairFC)
  quickCheck (fI :: PairFI)

  quickCheck (fC :: TwoFC)
  quickCheck (fI :: TwoFI)

  quickCheck (fC :: ThreeFC)
  quickCheck (fI :: ThreeFI)

  quickCheck (fC :: Three'FC)
  quickCheck (fI :: Three'FI)

  quickCheck (fC :: FourFC)
  quickCheck (fI :: FourFI)

  quickCheck (fC :: Four'FC)
  quickCheck (fI :: Four'FI)

  quickCheck (fC :: PossiblyFC)
  quickCheck (fI :: PossiblyFI)

  quickCheck (fC :: SumFC)
  quickCheck (fI :: SumFI)

  quickCheck (fC :: QuantFC)
  quickCheck (fI :: QuantFI)

  quickCheck (fC :: KFC)
  quickCheck (fI :: KFI)

  quickCheck (fC :: FlipKFC)
  quickCheck (fI :: FlipKFI)

  quickCheck (fC :: EvilGoateeConstFC)
  quickCheck (fI :: EvilGoateeConstFI)

  quickCheck (fC :: LiftItOutIdentityFC)
  quickCheck (fI :: LiftItOutIdentityFI)

  quickCheck (fC :: ParappaIdentityFC)
  quickCheck (fI :: ParappaIdentityFI)

  quickCheck (fC :: IgnoreOneIdentityFC)
  quickCheck (fI :: IgnoreOneIdentityFI)

  quickCheck (fC :: NotoriousIdentityFC)
  quickCheck (fI :: NotoriousIdentityFI)

  quickCheck (fC :: ListFC)
  quickCheck (fI :: ListFI)

  quickCheck (fC :: GoatLordFC)
  quickCheck (fI :: GoatLordFI)
  print "DONE!"
