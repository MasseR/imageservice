{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Network.HTTP.Images
  ( getUrls
  ) where

import           MyPrelude
import           Network.HTTP.Client
import qualified Network.HTTP.Images.Imgur as Imgur
import           Network.HTTP.Images.Types

getUrls :: (Imgur.HasImgur m, MonadHTTP m) => Href -> m [String]
getUrls = \case
  RawImg img -> pure [img]
  Reject _ -> pure []
  ImgurAlbum album -> fmap concat (Imgur.indexer album >>= traverse getUrls)
