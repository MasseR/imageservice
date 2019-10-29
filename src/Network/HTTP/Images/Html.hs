{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Images.Html
  ( indexer
  , parser
  ) where

import           MyPrelude
import           Network.HTTP.Client
import           Network.HTTP.Images.Common
import           Network.HTTP.Images.Types
import           Network.URI
import           Text.HTML.DOM              (parseLBS)
import           Text.XML.Lens
import qualified Data.Text as T

indexer :: MonadHTTP m => Text -> m [Href]
indexer url = parser url <$> getLbs (T.unpack url)

parser :: Text -> ImgParser
parser base = fmap relativize . cata imgAlgebra . toListOf images . parseLBS
  where
    images = root . entire . named "img" . attribute "src"
    relativize :: Href -> Href
    relativize = fmap relativizeUrl
    relativizeUrl :: Text -> Text
    relativizeUrl url
      | isRelativeReference (T.unpack url) = base <> "/" <> url
      | otherwise = url

