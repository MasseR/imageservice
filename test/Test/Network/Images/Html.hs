{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
module Test.Network.Images.Html
  ( spec
  ) where

import           Test.Hspec

import           ClassyPrelude
import           Network.HTTP.Images.Html
import           Network.HTTP.Images.Types
import           Test.Network.Images.Common


spec :: SpecWith ()
spec = describe "Images.Html" $ do
  it "should return no results for empty page" $ do
    urls <- evalTestHTTP (indexer "http://example.com") "<html></html>"
    urls `shouldBe` []
  it "should return images for non-empty page" $ do
    let html = "<html><body><img src=\"example.png\" /></body></html>"
    urls <- evalTestHTTP (indexer "http://example.com") html
    urls `shouldBe` [RawImg "http://example.com/example.png"]
