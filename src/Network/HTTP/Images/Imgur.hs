{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Images.Imgur
  ( indexer
  , HasImgur(..)
  ) where

import           Config                     (Token (..))
import           Control.Lens
import           Data.Aeson.Lens
import           MyPrelude
import           Network.HTTP.Client
import           Network.HTTP.Images.Common
import           Network.HTTP.Images.Types

class HasImgur m where
  getImgurApp :: m Token

indexer :: (HasImgur m, MonadHTTP m) => String -> m [Href]
indexer url = do
  Token token <- getImgurApp
  parser <$> getLbsAuth url (Authorization ("Client-ID " <> encodeUtf8 token))

parser :: ImgParser
parser = cata imgAlgebra . getUrls
  where
    getUrls :: LByteString -> [Maybe Text]
    getUrls json = json ^.. key "data" . _Array . traverse . key "link" . _String . to Just

