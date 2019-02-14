{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Worker.Indexer where

import           App
import           ClassyPrelude
import           Codec.Picture
import           Config
import           Control.Concurrent    (threadDelay)
import           Control.Lens
import           Data.Fingerprint
import           Data.Generics.Product
import           Database
import           Logging
import           Network.HTTP.Client
import qualified Worker.Indexer.Reddit as Reddit

indexer :: AppM ()
indexer = do
  ws <- view (typed @Config . field @"workers")
  queue <- liftIO newTChanIO
  void $ async (go queue)
  forever $ do
    forM_ ws $ \case
      Reddit subs ->
        -- forM_ subs $ \sub -> Reddit.images (Reddit.Subreddit $ unpack sub) >>= mapM_ (push queue . Reddit.getImg)
        Reddit.images (map (Reddit.Subreddit . unpack) subs) >>= mapM_ (push queue . Reddit.getImg)
    liftIO (threadDelay (15 * 15 * 1000000))
  where
    push queue = liftIO . atomically . writeTChan queue
    addToTree url = do
      fp <- hashImgHref url
      forM_ fp $ \fp' ->
        update (Insert fp')
    go :: TChan String -> AppM ()
    go queue = do
      url <- liftIO $ atomically $ readTChan queue
      unlessM (isJust <$> query (LookupFingerprint (pack url))) $ do
        catch @AppM @SomeException (addToTree (pack url)) (const (return ()))
      go queue

hashImgHref :: Text -> AppM (Either String Fingerprint)
hashImgHref url = do
  logLevel Info $ "Fetching " <> url
  m <- view (typed @Manager)
  withHttpFile m (unpack url) $ \path -> do
    img <- liftIO (readImage path)
    return (Fingerprint url . fingerprint DHash <$> img)
