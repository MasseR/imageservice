{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Network.HTTP.Images.Reddit
  ( indexer
  ) where

import ClassyPrelude
import           Network.HTTP.Client
import Network.HTTP.Images.Types
import Network.HTTP.Images.Common
import           Data.Aeson.Lens
import           Control.Lens

indexer :: MonadHTTP m => String -> m [Href]
indexer url = parser <$> getLbs url

parser :: ImgParser
parser = cata imgAlgebra . getUrls
  where
    getUrls :: LByteString -> [Maybe Text]
    getUrls json = json ^.. key "data" . key "children" . _Array . traverse . key "data" . key "url" . _String . to Just


