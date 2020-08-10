{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Worker.Indexer where

import           App
import           Codec.Picture
import           Config
import           Control.Concurrent        (threadDelay)
import           Control.Lens
import           Data.Fingerprint
import           Data.Generics.Product
import           Database
import           Logging
import           MyPrelude
import           Network.HTTP.Client
import           Network.HTTP.Images       (getUrls)
-- import           Network.HTTP.Images.Types
import qualified Worker.Indexer.Reddit     as Reddit

indexer :: AppM ()
indexer = do
  ws <- view (typed @Config . field @"workers")
  void $ forever $ do
    logLevel Info "Running"
    forM_ ws $ \case
      Reddit subs -> do
        images <- Reddit.images (map (Reddit.Subreddit . unpack) subs)
        pooledMapConcurrentlyN 10 (traverse (upsert . pack) <=< getUrls) images
    logLevel Info "Waiting for a minute"
    liftIO (threadDelay 60_000_000)
  where
    addToTree :: Fingerprint -> AppM ()
    addToTree fp = do
        insertS fp
        logLevel Info $ "Inserted " <> view (field @"imagePath") fp
    upsert :: Text -> AppM ()
    upsert url =
      unlessM (isJust <$> lookupFingerprint url) $
        hashHref url >>= traverse_ addToTree

hashHref :: Text -> AppM (Either String Fingerprint)
hashHref url = do
  now <- Just <$> liftIO getCurrentTime
  withHttpFile (unpack url) $ \path -> do
    img <- liftIO (readImage path)
    return (Fingerprint url <$> (fingerprint DHash <$> img) <*> pure now)
