{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Metrics where

import           Control.Lens
import           Data.Generics.Product
import           MyPrelude
import           System.CPUTime                  (getCPUTime)
import           System.Metrics                  (Sample, Store, newStore)
import qualified System.Metrics
import           System.Metrics.Counter          (Counter)
import qualified System.Metrics.Counter          as Counter
import           System.Metrics.Distribution     (Distribution)
import qualified System.Metrics.Distribution     as Distribution
import           System.Metrics.Gauge            (Gauge)
import           System.Metrics.Label            (Label)

import qualified Data.Map.Strict                 as M

import           GHC.Stack                       (HasCallStack)

type WithMetrics r m = (MonadReader r m, HasMetrics r)

data Register = RCounter Counter
    | RGauge Gauge
    | RDistribution Distribution
    | RLabel Label

data Metrics = Metrics
    { store     :: !Store
    , registers :: IORef (Map Text Register)
    }
    deriving Generic

class HasMetrics a where
  metrics :: Lens' a Metrics

createMetrics :: MonadIO m => m Metrics
createMetrics = liftIO $
  Metrics <$> newStore <*> newIORef mempty

data MetricsError = NoRegister
    | WrongRegisterType
    deriving Show

instance Exception MetricsError

createRegister :: (HasCallStack, WithMetrics r m, MonadIO m) => Text -> (Store -> m Register) -> m ()
createRegister name mkRegister = do
  Metrics{..} <- view metrics
  newR <- mkRegister store
  atomicModifyIORef' registers (\m -> (M.insert name newR m, ()) )

createCounter :: (HasCallStack, WithMetrics r m, MonadIO m) => Text -> m ()
createCounter name = createRegister name (\st -> RCounter <$> liftIO (System.Metrics.createCounter name st))

createDistribution :: (HasCallStack, WithMetrics r m, MonadIO m) => Text -> m ()
createDistribution name = createRegister name (\st -> RDistribution <$> liftIO (System.Metrics.createDistribution name st))

withCounter :: (HasCallStack, WithMetrics r m, MonadIO m) => Text -> (Counter -> m a) -> m a
withCounter name f = do
  r <- view (metrics . field @"registers")
  register <- M.lookup name <$> readIORef r
  case register of
       Just (RCounter counter) -> f counter
       Just _                  -> throwIO WrongRegisterType
       Nothing                 -> throwIO NoRegister

withDistribution :: (HasCallStack, WithMetrics r m, MonadIO m) => Text -> (Distribution -> m a) -> m a
withDistribution name f = do
  r <- view (metrics . field @"registers")
  register <- M.lookup name <$> readIORef r
  case register of
       Just (RDistribution d) -> f d
       Just _                 -> throwIO WrongRegisterType
       Nothing                -> throwIO NoRegister


incCounter :: (HasCallStack, WithMetrics r m, MonadIO m) => Text -> m a -> m a
incCounter name f = withCounter name (\counter -> liftIO (Counter.inc counter) >> f)

timeDistribution :: (MonadUnliftIO m, HasCallStack, WithMetrics r m, MonadIO m) => Text -> m a -> m a
timeDistribution name f = withDistribution name $ \distribution ->
  bracket (liftIO getCPUTime) (\start -> liftIO (getCPUTime >>= record distribution start)) (const f)
    where
      record :: Distribution -> Integer -> Integer -> IO ()
      record distribution start end = do
        let diff = fromIntegral (end - start) / (10 ^ (12 :: Int))
        Distribution.add distribution diff

sampleAll :: (WithMetrics r m, MonadIO m) => m Sample
sampleAll = view (metrics . typed @Store) >>= liftIO . System.Metrics.sampleAll

