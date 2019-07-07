{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Metrics where

import           ClassyPrelude
import           Control.Lens
import           Data.Generics.Product
import           System.Metrics         (Store, createCounter, newStore)
import           System.Metrics.Counter (Counter, inc)

type WithMetrics r m = (MonadReader r m, HasType Metrics r)

data Metrics =
  Metrics { store   :: Store
          , updates :: Counter }
             deriving Generic

createMetrics :: MonadIO m => m Metrics
createMetrics = liftIO $ do
  store <- newStore
  updates <- createCounter "imageservice.updates" store
  pure Metrics{..}

increaseUpdates :: (WithMetrics r m, MonadIO m) => m ()
increaseUpdates =
  view (typed @Metrics . field @"updates") >>= liftIO . inc
