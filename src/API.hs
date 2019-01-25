{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module API where

import           App
import           ClassyPrelude          hiding (foldMap)
import           Control.Lens           (view)
import           Control.Monad.Catch    (throwM)
import           Data.Aeson
import qualified Data.BKTree            as BK
import           Data.Generics.Product
import           Data.Monoid            (Sum (..))
import           Prelude                (foldMap)
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           Worker.Indexer         (hashImgHref)

newtype Url = Url String deriving (FromHttpApiData, ToJSON)

data Routes route = Routes
                  { getDBSize :: route :- "database" :> "size" :> Get '[JSON] Int
                  , getSimilar :: route :- "similar" :> Capture "range" Int :> QueryParam "url" Url :> Get '[JSON] [Url]
                  }
                  deriving (Generic)

handler :: Routes (AsServerT AppM)
handler = Routes{..}
  where
    getDBSize :: AppM Int
    getDBSize = do
      HashTree t <- view (typed @HashTree)
      getSum . foldMap (const (Sum 1)) <$> liftIO (atomically $ readTVar t)
    getSimilar :: Int -> Maybe Url -> AppM [Url]
    getSimilar n = \case
      Nothing -> return []
      Just (Url url) -> do
        HashTree t <- view (typed @HashTree)
        bk <- liftIO $ atomically $ readTVar t
        similar <- fmap (\h -> BK.search n h bk) <$> hashImgHref url
        either (const (throwM err500)) (return . map (Url . view (typed @String))) similar
