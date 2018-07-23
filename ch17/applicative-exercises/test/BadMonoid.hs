module BadMonoid where

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Bull = Fools
          | Twoo
          deriving (Eq, Show)

instance Semigroup Bull where
  (<>) Twoo _ = Twoo
  (<>) _ Twoo = Twoo
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools


instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo)
                        ]

instance EqProp Bull where
  (=-=) = eq
