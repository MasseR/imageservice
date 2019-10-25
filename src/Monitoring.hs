{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Monitoring where
import           App
import           Control.Lens
import           Control.Monad.Catch         (throwM)
import           Data.Aeson                  (ToJSON)
import           Data.Generics.Product
import           Metrics
import           MyPrelude
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           System.Metrics
import qualified System.Metrics.Distribution as Dist

data GC = GC { num_gcs                  :: Int64
             , bytes_allocated          :: Int64
             , cumulative_bytes_used    :: Int64
             , max_bytes_used           :: Int64
             , current_bytes_used       :: Int64
             , peak_megabytes_allocated :: Int64
             }
        deriving (Generic, ToJSON)

data Latency = Latency { mean     :: Double
                       , variance :: Double
                       , count    :: Int64
                       , sum      :: Double
                       , min      :: Double
                       , max      :: Double }
             deriving (Generic, ToJSON)

data MonitoringRoutes route = Monitoring { getGc :: route :- "gc" :> Get '[JSON] GC
                                         , getRequests :: route :- "requests" :> Get '[JSON] Int64
                                         , getLatency :: route :- "latency" :> Get '[JSON] Latency
                                         }
                            deriving Generic

_Counter :: Prism' Value Int64
_Counter = prism Counter x
  where x (Counter y) = Right y
        x y           = Left y

_Gauge :: Prism' Value Int64
_Gauge = prism Gauge x
  where x (Gauge y) = Right y
        x y         = Left y

_Distribution :: Prism' Value Dist.Stats
_Distribution = prism Distribution x
  where x (Distribution y) = Right y
        x y                = Left y

withSample :: (WithMetrics r m, MonadIO m) => (Sample -> a) -> m a
withSample f = f <$> (view (typed @Metrics . typed @Store) >>= liftIO . sampleAll)

handler :: MonitoringRoutes (AsServerT AppM)
handler = Monitoring{..}
  where
    getGc = maybe (throwM err500) return =<< withSample buildGC
    buildGC x = GC <$> preview (ix "rts.gc.num_gcs" . _Counter) x
                   <*> preview (ix "rts.gc.bytes_allocated" . _Counter) x
                   <*> preview (ix "rts.gc.cumulative_bytes_used" . _Counter) x
                   <*> preview (ix "rts.gc.max_bytes_used" . _Gauge) x
                   <*> preview (ix "rts.gc.current_bytes_used" . _Gauge) x
                   <*> preview (ix "rts.gc.peak_megabytes_allocated" . _Gauge) x
    buildLatency stats = Latency { mean = Dist.mean stats
                                 , variance = Dist.variance stats
                                 , count = Dist.count stats
                                 , sum = Dist.sum stats
                                 , min = Dist.min stats
                                 , max = Dist.max stats }
    getRequests = maybe (throwM err500) return =<< withSample (preview (ix "wai.request_count" . _Counter))
    getLatency = maybe (throwM err500) (return . buildLatency) =<< withSample (preview (ix "wai.latency_distribution" . _Distribution))
