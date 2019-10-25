{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Images.Reddit
  ( indexer
  ) where

import           Control.Lens
import           Data.Aeson.Lens
import           MyPrelude
import           Network.HTTP.Client
import           Network.HTTP.Images.Common
import           Network.HTTP.Images.Types

indexer :: MonadHTTP m => String -> m [Href]
indexer url = parser <$> getLbs url

parser :: ImgParser
parser = cata imgAlgebra . getUrls
  where
    getUrls :: LByteString -> [Maybe Text]
    getUrls json = json ^.. key "data" . key "children" . _Array . traverse . key "data" . key "url" . _String . to Just


