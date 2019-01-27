{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module API where

import           App
import           ClassyPrelude
import           Control.Lens           (view)
import           Data.Aeson
import           Data.Generics.Product
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Database
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           Worker.Indexer         (hashImgHref)
import Control.Monad (msum)
import qualified Monitoring

newtype Url = Url String deriving (FromHttpApiData, ToJSON)

data Routes route = Routes
                  { getDBSize :: route :- "database" :> "size" :> Get '[JSON] Int
                  , getSimilar :: route :- "similar" :> Capture "range" Int :> QueryParam "url" Url :> Get '[JSON] [Url]
                  , monitoring :: route :- "metrics" :> ToServant Monitoring.MonitoringRoutes AsApi
                  }
                  deriving (Generic)

handler :: Routes (AsServerT AppM)
handler = Routes{..}
  where
    monitoring = toServant Monitoring.handler
    getDBSize :: AppM Int
    getDBSize = query Size
    getSimilar :: Int -> Maybe Url -> AppM [Url]
    getSimilar n = \case
      Nothing -> return []
      Just (Url url) -> do
        fp <- runMaybeT $ msum [MaybeT $ query (LookupFingerprint url), MaybeT $ fetchNew url]
        let limited = if n <= 10 then n else 10
        map (Url . view (typed @String)) <$> maybe (return []) (query . LookupSimilar limited) fp
    fetchNew url = do
      fp <- hashImgHref url
      forM (either (const Nothing) Just fp) $ \fp' ->
        fp' `seq` update (Insert fp') >> pure fp'
