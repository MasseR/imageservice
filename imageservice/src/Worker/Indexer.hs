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
import           Metrics               (incCounter, timeDistribution)
import           MyPrelude
import           Network.HTTP.Client
import           Network.HTTP.Images   (getUrls)
import qualified Worker.Indexer.Reddit as Reddit

indexer :: AppM ()
indexer = do
  ws <- view (typed @Config . field @"workers")
  void $ forever $ do
    logInfo "Starting run"
    forM_ ws $ \case
      Reddit subs -> do
        images <- Reddit.images (map (Reddit.Subreddit . unpack) subs)
        insertable <- concat <$> traverse (filterM isUnseen <=< getUrls) images
        logInfo (tshow (length insertable) <> " new items will be inserted")
        pooledMapConcurrentlyN 3 upsert insertable
    logInfo "Run complete, waiting"
    liftIO (threadDelay 300_000_000)
  where
    addToTree :: Fingerprint -> AppM ()
    addToTree fp = do
      incCounter "imageservice.inserts" (insertS fp)
      logInfo $ "Inserted " <> view (field @"imagePath") fp
    isUnseen :: Text -> AppM Bool
    isUnseen url = isNothing <$> lookupFingerprint url
    timeout' n f = note "Timed out" <$> timeout n f
    upsert :: Text -> AppM ()
    upsert =
        either (logWarning . pack) addToTree <=< pure . join <=< timeout' 30_000_000 . hashHref

hashHref :: Text -> AppM (Either String Fingerprint)
hashHref url = timeDistribution "imageservice.fetch" $ do
  now <- Just <$> liftIO getCurrentTime
  withHttpFile (unpack url) $ \path -> do
    img <- liftIO (readImage path)
    return (Fingerprint url <$> (fingerprint DHash <$> img) <*> pure now)
