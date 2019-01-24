{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE FlexibleContexts     #-}
module Worker.Indexer.Reddit where

import           ClassyPrelude
import           Control.Lens
import           Data.Aeson.Lens
import Network.HTTP.Client
import Data.Generics.Product
import Control.Monad.Catch (MonadThrow)

newtype Subreddit = Subreddit String
newtype ImgHref = ImgHref { getImg :: String }

-- Consider going to full types, they're just a bit complex given the time I have

images :: (HasType Manager r, MonadThrow m, MonadReader r m, MonadUnliftIO m) => Subreddit -> m [ImgHref]
images (Subreddit r) = do
  manager <- view (typed @Manager)
  lbs <- getLbs manager ("https://www.reddit.com/r/" <> r <> ".json")
  return . map (ImgHref . unpack) . filter (".jpg" `isSuffixOf`) . getImages $ lbs
  where
    getImages json = json ^.. key "data" . key "children" . _Array . traverse . key "data" . key "url" . _String

