{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Network.HTTP.Images
  ( getUrls
  ) where

import           MyPrelude
import           Network.HTTP.Client
import qualified Network.HTTP.Images.Html  as Html
import qualified Network.HTTP.Images.Imgur as Imgur
import           Network.HTTP.Images.Types

getUrls :: (Imgur.HasImgur m, MonadHTTP m) => Href -> m [String]
getUrls = \case
  RawImg img -> pure [img]
  ImgurAlbum album -> fmap concat (Imgur.indexer album >>= traverse getUrls)
  Html page -> fmap concat (Html.indexer page >>= traverse getUrls)
  Reject _ -> pure []
