module Test.QuickCheck.Combinators where

import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (chooseInt, sized)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Typelevel.Num (class Nat, toInt')
import Data.Unfoldable (class Unfoldable, replicateA)
import Data.Traversable (class Traversable)
import Type.Proxy (Proxy (..))



newtype AtLeast n t a = AtLeast (t a)

derive instance genericAtLeast :: (Generic (t a) ta) => Generic (AtLeast n t a) _

instance arbitraryAtLeast :: (Arbitrary a, Nat n, Unfoldable t, Traversable t) => Arbitrary (AtLeast n t a) where
  arbitrary = sized \s -> do
    let n = toInt' (Proxy :: Proxy n)
    l <- chooseInt n s
    AtLeast <$> replicateA l arbitrary
