{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Database where

import           Control.Lens           (Lens', view, (^.))
import           Control.Monad.State
import           Data.BKTree            (BKTree)
import qualified Data.BKTree            as BK
import           Data.Fingerprint       (Fingerprint (..))
import           Data.Generics.Product
import           MyPrelude

import           Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as SQL
import           GHC.Stack

data Store = Store
    { inmem   :: IORef (BKTree Fingerprint)
    , persist :: Connection
    }
    deriving Generic

class HasStore a where
  store :: Lens' a Store

size :: (HasCallStack, HasStore r, MonadReader r m, MonadIO m) => m Int
size = do
  conn <- view (store . field @"persist")
  x <- liftIO (SQL.query_ conn "select count(*) from fingerprints")
  case x of
       [SQL.Only c] -> pure c
       _            -> pure 0

insertS :: (HasCallStack, HasStore r, MonadReader r m, MonadIO m) => Fingerprint -> m ()
insertS fp = do
  i <- view (store . field @"inmem")
  conn <- view (store . field @"persist")
  () <- liftIO $ SQL.execute conn "insert into fingerprints (path, hash, checked) values (?, ?, ?)" fp
  () <- atomicModifyIORef' i ((,()) . BK.insert fp)
  pure ()

lookupFingerprint :: (HasCallStack, HasStore r, MonadReader r m, MonadIO m) => Text -> m (Maybe Fingerprint)
lookupFingerprint path = do
  conn <- view (store . field @"persist")
  liftIO (listToMaybe <$> SQL.query conn "select path, hash, checked from fingerprints where path=?" (SQL.Only path))

lookupSimilar :: (HasCallStack, HasStore r, MonadReader r m, MonadIO m) => Int -> Fingerprint -> m [Fingerprint]
lookupSimilar n fp = do
  i <- view (store . field @"inmem")
  BK.search n fp <$> readIORef i

mkStore :: MonadIO m => Connection -> m Store
mkStore conn = do
  i <- liftIO (SQL.fold_ conn "select path, hash, checked from fingerprints" BK.empty (\acc x -> pure (BK.insert x acc)) >>= newIORef)
  pure $ Store i conn

modifyFingerprint :: (HasStore r, MonadReader r m, MonadUnliftIO m) => (Fingerprint -> m (Maybe Fingerprint)) -> m ()
modifyFingerprint f = do
  conn <- view (store . field @"persist")
  withRunInIO $ \runInIO -> SQL.fold_ conn "select path, hash, checked from fingerprints" ()
    (\() fp -> runInIO (filterFP conn fp =<< f fp))
  where
    filterFP :: MonadIO m => Connection -> Fingerprint -> Maybe Fingerprint -> m ()
    filterFP conn old = \case
      Nothing -> liftIO $ SQL.execute conn "delete from fingerprints where path = ?" (SQL.Only (old ^. field @"imagePath"))
      Just new | old == new -> pure ()
               | otherwise ->
                 liftIO $ SQL.withTransaction conn $ do
                  SQL.execute conn "delete from fingerprints where path = ?" (SQL.Only (old ^. field @"imagePath"))
                  SQL.execute conn "insert into fingerprints (path, hash, checked) values (?, ?, ?)" new

