{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Server where

import           API
import           App
import           Control.Monad.Except   (ExceptT (..))
import           MyPrelude
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

api :: Proxy (ToServantApi Routes)
api = genericApi @Routes Proxy

application :: App -> Application
application st = serve api (hoistServer api nat (genericServerT handler))
  where
    nat f = Handler $ ExceptT $ try (runReaderT (runApp f) st)
