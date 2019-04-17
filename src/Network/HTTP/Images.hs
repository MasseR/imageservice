{-# LANGUAGE LambdaCase #-}
module Network.HTTP.Images
  ( getUrls
  ) where

import           ClassyPrelude
import           Network.HTTP.Images.Types

getUrls :: MonadIO m => Href -> m [String]
getUrls = \case
  RawImg img -> pure [img]
  Reject _ -> pure []
  ImgurAlbum _ -> pure []
