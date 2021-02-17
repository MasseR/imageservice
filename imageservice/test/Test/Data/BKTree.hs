{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.BKTree where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Deriving

import Data.Foldable (for_)

import Data.Typeable

import           GHC.Generics             (Generic)

import           Data.Monoid              (Sum (..))

import           Data.BKTree

import qualified Data.Set                 as S

data Point = Point !Int !Int
           deriving stock (Show, Generic, Ord, Eq)
           deriving Arbitrary via ((Int, Int) `Isomorphic` Point)
           deriving (Monoid, Semigroup) via ((Sum Int, Sum Int) `Isomorphic` Point)
           deriving Metric via ((Int, Int) `Isomorphic` Point)

prop_empty_range :: Point -> BKTree Point -> Property
prop_empty_range p tree =
  counterexample (show p <> " not elem " <> show tree) $
  property $ p `S.member` S.fromList (search 0 p (insert p tree))

prop_range :: [Point] -> Property
prop_range xs =
  let tree = fromList points
      points = additions <> xs
      additions = [target <> Point x y | x <- [-1,0,1], y <- [-1,0,1]]
      target = Point 0 0
      wanted = S.fromList (filter (\x -> distance target x <= 1) points)
  in collect (show $ length wanted) $ wanted === S.fromList (search 1 target tree)

prop_associative :: (Show a, Semigroup a, Eq a) => a -> a -> a -> Property
prop_associative x y z =
  x <> (y <> z) === (x <> y) <> z

specLaws :: forall a. (Proxy (a :: k) -> Laws) -> Spec
specLaws mkLaws = describe (lawsTypeclass laws) $
  for_ (lawsProperties laws) $ \(rule, p) ->
    it rule p
  where
    laws = mkLaws $ Proxy @a

spec :: Spec
spec = describe "BKTree properties" $ do
  it "forall p. forall t. t in search 0 p (insert p t)" $
    property prop_empty_range
  it "returns all elements within range" $
    property prop_range
  specLaws $ eqLaws @(BKTree Point)
  specLaws $ semigroupLaws @(BKTree Point)
  specLaws $ monoidLaws @(BKTree Point)
