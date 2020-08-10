{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.Images.Imgur
  ( spec
  ) where

import           Control.Lens
import           MyPrelude
import           Network.HTTP.Images.Imgur
import           Network.HTTP.Images.Types
import           Test.Hspec
import           Test.Network.Images.Common

spec :: Spec
spec = describe "Images.Imgur" $
  it "should return results for json" $ do
    lbs <- view (from strict) <$> readFile "testdata/imgur.json"
    urls <- evalTestHTTP (indexer "http://example.com") lbs
    urls `shouldBe` [RawImg "https://i.imgur.com/asd.jpg", RawImg "https://i.imgur.com/basd.jpg"]
