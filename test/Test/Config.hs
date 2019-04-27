{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Config
  ( spec
  ) where

import           Test.Hspec

import           Config
import           Dhall

spec :: SpecWith ()
spec = describe "Config" $
  it "is parsed from sample" $ do
    Config{port} <- input auto "./sample.dhall"
    port `shouldBe` 8000
