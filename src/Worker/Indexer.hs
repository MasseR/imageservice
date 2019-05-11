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
import           ClassyPrelude
import           Codec.Picture
import           Config
import           Control.Concurrent        (threadDelay)
import           Control.Lens
import           Data.Fingerprint
import           Data.Generics.Product
import           Database
import           Logging
import           Network.HTTP.Client
import           Network.HTTP.Images       (getUrls)
import           Network.HTTP.Images.Types
import qualified Worker.Indexer.Reddit     as Reddit

indexer :: AppM ()
indexer = do
  ws <- view (typed @Config . field @"workers")
  queue <- liftIO newTChanIO
  void $ async (go queue)
  forever $ do
    forM_ ws $ \case
      Reddit subs ->
        Reddit.images (map (Reddit.Subreddit . unpack) subs) >>= mapM_ (push queue)
    liftIO (threadDelay 900_000_000)
  where
    push :: MonadIO m => TChan Href -> Href -> m ()
    push queue = liftIO . atomically . writeTChan queue
    addToTree url = do
      fp <- hashHref url
      forM_ fp $ \fp' ->
        update (Insert fp')
    upsert url =
      unlessM (isJust <$> query (LookupFingerprint (pack url))) $
        addToTree (pack url)
    go :: TChan Href -> AppM ()
    go queue = do
      logLevel Info $ "Querying for next line in the queue"
      next <- atomically (readTChan queue)
      logLevel Info $ tshow next
      res <- timeout 30_000_000 $ do
        urls <- getUrls next
        traverse_ upsert urls
      when (isNothing res) $ logLevel Info $ "Request timed out"
      go queue

hashHref :: Text -> AppM (Either String Fingerprint)
hashHref url = do
  logLevel Info $ "Fetching " <> url
  withHttpFile (unpack url) $ \path -> do
    img <- liftIO (readImage path)
    return (Fingerprint url . fingerprint DHash <$> img)
