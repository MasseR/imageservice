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

import           App                   (HashTree (..))
import           ClassyPrelude
import           Codec.Picture
import           Config
import           Control.Concurrent    (threadDelay)
import           Control.Lens
import           Control.Monad.Catch   (MonadThrow)
import           Control.Monad.Logger
import           Data.BKTree           (BKTree)
import qualified Data.BKTree           as BK
import           Data.Fingerprint
import           Data.Generics.Product
import           Network.HTTP.Client
import qualified Worker.Indexer.Reddit as Reddit

indexer :: forall r m. (MonadLogger m, HasType HashTree r, HasType Manager r, MonadThrow m, HasType Config r, MonadReader r m, MonadUnliftIO m) => m ()
indexer = do
  ws <- view (typed @Config . field @"workers")
  queue <- liftIO newTChanIO
  void $ async (go queue [])
  forever $ do
    forM_ ws $ \case
      Reddit subs ->
        forM_ subs $ \sub -> Reddit.images (Reddit.Subreddit $ unpack sub) >>= mapM_ (push queue . Reddit.getImg)
    liftIO (threadDelay (60 * 15 * 1000000))
  where
    push queue = liftIO . atomically . writeTChan queue
    addToTree url = do
      fp <- hashImgHref url
      forM_ fp $ \fp' -> withTree (BK.insert fp')
    go :: TChan String -> Set String -> m ()
    go queue seen = do
      url <- liftIO $ atomically $ readTChan queue
      let seen' = [url] <> (seen :: Set String)
      unless (url `member` seen) $
        catch @m @SomeException (addToTree url) (const (return ()))
      go queue seen'

withTree :: (MonadReader r m, HasType HashTree r, MonadIO m) => (BKTree Fingerprint -> BKTree Fingerprint) -> m ()
withTree f = do
  HashTree t <- view (typed @HashTree)
  liftIO $ atomically (modifyTVar t f)

hashImgHref :: (MonadLogger m, HasType Manager r, MonadThrow m, MonadReader r m, MonadUnliftIO m) => String -> m (Either String Fingerprint)
hashImgHref url = do
  $logInfo $ "Fetching " <> tshow url
  manager <- view (typed @Manager)
  withHttpFile manager url $ \path -> do
    img <- liftIO (readImage path)
    return (Fingerprint path . fingerprint DHash <$> img)
