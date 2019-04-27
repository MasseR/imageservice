{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Test.Network.Images.Reddit
  ( spec
  ) where

import           ClassyPrelude
import           Network.HTTP.Images.Reddit
import           Network.HTTP.Images.Types
import           Test.Hspec
import           Test.Network.Images.Common

removeRejects :: [Href] -> [Href]
removeRejects = filter $ \case
  Reject _ -> False
  _ -> True

spec :: Spec
spec = describe "Images.Html" $ do
  it "should return no results for empty json" $ do
    lbs <- fromStrict <$> readFile "testdata/empty.json"
    urls <- evalTestHTTP (indexer "http://example.com") lbs
    removeRejects urls `shouldBe` []
  it "should return results for non-empty json" $ do
    lbs <- fromStrict <$> readFile "testdata/data.json"
    urls <- evalTestHTTP (indexer "http://example.com") lbs
    length (removeRejects urls) `shouldBe` 24
