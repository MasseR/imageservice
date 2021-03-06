{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module API where

import           App
import           Control.Lens              (view)
import           Control.Monad             (msum)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Aeson
import           Data.Generics.Product
import           Database
import qualified Monitoring
import           MyPrelude
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           Worker.Indexer            (hashHref)

newtype Url = Url Text deriving (FromHttpApiData, ToJSON)

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
    getDBSize = size
    getSimilar :: Int -> Maybe Url -> AppM [Url]
    getSimilar n = \case
      Nothing -> return []
      Just (Url url) -> do
        fp <- runMaybeT $ msum [MaybeT $ lookupFingerprint url, MaybeT $ fetchNew url]
        let limited = if n <= 10 then n else 10
        map (Url . view (typed @Text)) <$> maybe (return []) (lookupSimilar limited) fp
    fetchNew url = do
      fp <- hashHref url
      forM (either (const Nothing) Just fp) $ \fp' ->
        insertS fp' >> pure fp'
