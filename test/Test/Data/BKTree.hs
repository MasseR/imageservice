{-# LANGUAGE TypeApplications #-}
module Test.Data.BKTree where

import           Test.Hspec

import           Test.Validity

import           Data.BKTree

import qualified Data.Set      as S

spec :: Spec
spec = describe "BKTree properties" $ do
  genValidSpec @(BKTree Point)
  it "forall p. forall t. t in search 0 p (insert p t)" $
    forAllValid @Point $ \p -> forAllValid $ \tree ->
      p `S.member` S.fromList (search 0 p (insert p tree))
  it "All returned elements are within range" $
    forAllValid $ \tree ->
      let p = Point 0 0
      in all (\p' -> distance p p' <= 1) (search 1 p tree)
