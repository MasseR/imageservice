{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TemplateHaskell      #-}
module Worker.Indexer.Reddit where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Catch   (MonadThrow)
import           Control.Monad.Logger
import           Data.Aeson.Lens
import           Data.Generics.Product
import           Network.HTTP.Client

newtype Subreddit = Subreddit String
newtype ImgHref = ImgHref { getImg :: String }

-- Consider going to full types, they're just a bit complex given the time I have

images :: (MonadLogger m, HasType Manager r, MonadThrow m, MonadReader r m, MonadUnliftIO m) => Subreddit -> m [ImgHref]
images (Subreddit r) = do
  $logInfo $ "Fetching images for " <> tshow r
  manager <- view (typed @Manager)
  lbs <- getLbs manager ("https://www.reddit.com/r/" <> r <> ".json")
  return . map (ImgHref . unpack) . filter (".jpg" `isSuffixOf`) . getImages $ lbs
  where
    getImages json = json ^.. key "data" . key "children" . _Array . traverse . key "data" . key "url" . _String

