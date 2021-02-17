{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
module Test.Data.BKTree where

import           Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Deriving

import GHC.Generics (Generic)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Ne

import Data.Monoid (Sum(..))

import           Data.BKTree

import qualified Data.Set      as S

data Point = Point !Int !Int
           deriving stock (Show, Generic, Ord, Eq)
           deriving Arbitrary via ((Int, Int) `Isomorphic` Point)
           deriving (Monoid, Semigroup) via ((Sum Int, Sum Int) `Isomorphic` Point)

instance Metric Point where
  distance (Point p1 p2) (Point q1 q2) = abs (p1 - q1) + abs (p2 - q2)

prop_empty_range :: Point -> BKTree Point -> Property
prop_empty_range p tree =
  counterexample (show p <> " not elem " <> show tree) $
  property $ p `S.member` S.fromList (search 0 p (insert p tree))

prop_range :: Point -> [Point] -> Property
prop_range target xs =
  let tree = fromList points
      points = additions <> xs
      additions = [target <> Point x y | x <- [-1,0,1], y <- [-1,0,1]]
      target = Point 0 0
      wanted = S.fromList (filter (\x -> distance target x <= 1) points)
  in collect (show $ length wanted) $ wanted === S.fromList (search 1 target tree)

spec :: Spec
spec = describe "BKTree properties" $ do
  it "forall p. forall t. t in search 0 p (insert p t)" $
    property $ prop_empty_range
  it "returns all elements within range" $
    property prop_range
