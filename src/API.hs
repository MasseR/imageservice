{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module API where

import           App                    (AppM)
import           ClassyPrelude hiding (foldMap)
import Prelude (foldMap)
import Data.Monoid (Sum(..))
import           Control.Lens           (view)
import           Data.Generics.Product
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

data Routes route = Routes
                  { getDBSize :: route :- "database" :> "size" :> Get '[JSON] Int
                  }
                  deriving (Generic)

handler :: Routes (AsServerT AppM)
handler = Routes{..}
  where
    getDBSize :: AppM Int
    getDBSize = do
      t <- view (field @"tree")
      getSum . foldMap (const (Sum 1)) <$> liftIO (atomically $ readTVar t)
