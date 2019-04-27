{-# LANGUAGE NoImplicitPrelude #-}
module Test.Network.Images.Imgur
  ( spec
  ) where

import           ClassyPrelude
import           Network.HTTP.Images.Imgur
import           Network.HTTP.Images.Types
import           Test.Hspec
import           Test.Network.Images.Common

spec :: Spec
spec = describe "Images.Imgur" $
  it "should return results for json" $ do
    lbs <- fromStrict <$> readFile "testdata/imgur.json"
    urls <- evalTestHTTP (indexer "http://example.com") lbs
    urls `shouldBe` [RawImg "https://i.imgur.com/asd.jpg", RawImg "https://i.imgur.com/basd.jpg"]
