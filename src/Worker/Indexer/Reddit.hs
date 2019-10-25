{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
module Worker.Indexer.Reddit where

import           Control.Monad.Catch        (MonadThrow)
import           Data.Generics.Product
import           Data.List.Split            (chunksOf)
import           Logging
import           MyPrelude
import           Network.HTTP.Client
import           Network.HTTP.Images.Reddit
import           Network.HTTP.Images.Types

newtype Subreddit = Subreddit { getSubreddit :: String }

-- Consider going to full types, they're just a bit complex given the time I have

images :: (MonadHTTP m, HasLog m, HasType Manager r, MonadThrow m, MonadReader r m, MonadUnliftIO m) => [Subreddit] -> m [Href]
images rs = concat <$> mapM multireddit (mkMultireddit rs)
  where
    multireddit r = do
      logLevel Info $ "Fetching images for " <> tshow r
      urls <- indexer ("https://www.reddit.com/r/" <> r <> "/new.json")
      logLevel Debug $ "Rejecting: " <> tshow (rejected urls)
      return urls
    rejected = filter $ \case
      Reject _ -> True
      _ -> False
    mkMultireddit :: [Subreddit] -> [String]
    mkMultireddit = map (intercalate "+") . chunksOf 5 . map getSubreddit

