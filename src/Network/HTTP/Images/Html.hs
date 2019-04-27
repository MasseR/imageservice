{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Images.Html
  ( indexer
  , parser
  ) where

import           ClassyPrelude
import           Network.HTTP.Client
import           Network.HTTP.Images.Types
import           Network.URI
import           Text.HTML.DOM             (parseLBS)
import           Text.XML.Lens
import Network.HTTP.Images.Common

indexer :: MonadHTTP m => String -> m [Href]
indexer url = parser url <$> getLbs url

parser :: String -> ImgParser
parser base = fmap relativize . cata imgAlgebra . toListOf images . parseLBS
  where
    images = root . entire . named "img" . attribute "src"
    relativize :: Href -> Href
    relativize = fmap relativizeUrl
    relativizeUrl :: String -> String
    relativizeUrl url
      | isRelativeReference url = base <> "/" <> url
      | otherwise = url

