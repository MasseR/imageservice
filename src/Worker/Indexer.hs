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
import           Control.Concurrent    (threadDelay)
import           Control.Lens
import           Data.Fingerprint
import           Data.Generics.Product
import           Database
import           Logging
import           Metrics               (increaseUpdates)
import           MyPrelude
import           Network.HTTP.Client
import           Network.HTTP.Images   (getUrls)
import qualified Worker.Indexer.Reddit as Reddit

indexer :: AppM ()
indexer = do
  ws <- view (typed @Config . field @"workers")
  void $ forever $ do
    logLevel Info "Starting run"
    forM_ ws $ \case
      Reddit subs -> do
        images <- Reddit.images (map (Reddit.Subreddit . unpack) subs)
        insertable <- concat <$> traverse (filterM isUnseen <=< getUrls) images
        logLevel Info (tshow (length insertable) <> " new items will be inserted")
        pooledMapConcurrentlyN 3 upsert insertable
    logLevel Info "Run complete, waiting"
    liftIO (threadDelay 300_000_000)
  where
    addToTree :: Fingerprint -> AppM ()
    addToTree fp = do
      increaseUpdates -- Metrics
      insertS fp
      logLevel Info $ "Inserted " <> view (field @"imagePath") fp
    isUnseen :: Text -> AppM Bool
    isUnseen url = isNothing <$> lookupFingerprint url
    timeout' n f = note "Timed out" <$> timeout n f
    upsert :: Text -> AppM ()
    upsert =
        either (logLevel Warning . pack) addToTree <=< pure . join <=< timeout' 30_000_000 . hashHref

hashHref :: Text -> AppM (Either String Fingerprint)
hashHref url = do
  now <- Just <$> liftIO getCurrentTime
  withHttpFile (unpack url) $ \path -> do
    img <- liftIO (readImage path)
    return (Fingerprint url <$> (fingerprint DHash <$> img) <*> pure now)
