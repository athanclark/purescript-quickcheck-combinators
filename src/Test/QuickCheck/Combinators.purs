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
getAtLeast :: forall n t a. AtLeast n t a -> t a
getAtLeast (AtLeast xs) = xs

derive instance genericAtLeast :: (Generic (t a) ta) => Generic (AtLeast n t a) _

instance arbitraryAtLeast :: (Arbitrary a, Nat n, Unfoldable t, Traversable t) => Arbitrary (AtLeast n t a) where
  arbitrary = sized \s -> do
    let n = toInt' (Proxy :: Proxy n)
    l <- chooseInt n (if s < n then n else s)
    AtLeast <$> replicateA l arbitrary


newtype AtMost n t a = AtMost (t a)
getAtMost :: forall n t a. AtMost n t a -> t a
getAtMost (AtMost xs) = xs

derive instance genericAtMost :: (Generic (t a) ta) => Generic (AtMost n t a) _

instance arbitraryAtMost :: (Arbitrary a, Nat n, Unfoldable t, Traversable t) => Arbitrary (AtMost n t a) where
  arbitrary = do
    let n = toInt' (Proxy :: Proxy n)
    l <- chooseInt 0 n
    AtMost <$> replicateA l arbitrary


newtype Between n m t a = Between (t a)
getBetween :: forall n m t a. Between n m t a -> t a
getBetween (Between xs) = xs

derive instance genericBetween :: (Generic (t a) ta) => Generic (Between n m t a) _

instance arbitraryBetween :: (Arbitrary a, Nat n, Nat m, Unfoldable t, Traversable t) => Arbitrary (Between n m t a) where
  arbitrary = do
    let n = toInt' (Proxy :: Proxy n)
        m = toInt' (Proxy :: Proxy m)
    l <- chooseInt n m
    Between <$> replicateA l arbitrary
