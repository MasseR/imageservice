{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Monitoring where
import           App
import           Data.Aeson                  (ToJSON (..), Value (..), object,
                                              (.=))
import           Metrics
import           MyPrelude
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import qualified System.Metrics
import qualified System.Metrics.Distribution as Dist

newtype MSample
  = MSample System.Metrics.Sample

instance ToJSON MSample where
  toJSON (MSample s) = Object (fmap convert s)
    where
      convert = \case
        System.Metrics.Counter n -> toJSON n
        System.Metrics.Label l -> toJSON l
        System.Metrics.Gauge g -> toJSON g
        System.Metrics.Distribution d ->
          object [ "mean" .= Dist.mean d
                 , "variance" .= Dist.variance d
                 , "count" .= Dist.count d
                 , "sum" .= Dist.sum d
                 , "min" .= Dist.min d
                 , "max" .= Dist.max d
                 ]

newtype MonitoringRoutes route
  = Monitoring { getMetrics :: route :- Get '[JSON] MSample }
                            deriving Generic


handler :: MonitoringRoutes (AsServerT AppM)
handler = Monitoring{..}
  where
    getMetrics :: AppM MSample
    getMetrics = MSample <$> sampleAll
