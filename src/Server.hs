{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Server where

import           API
import           App
import           ClassyPrelude          hiding (Handler)
import           Control.Monad.Except   (ExceptT (..))
import           Control.Monad.Logger
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

api :: Proxy (ToServantApi Routes)
api = genericApi @Routes Proxy

application :: App -> Application
application st = serve api (hoistServer api nat (genericServerT handler))
  where
    nat f = Handler $ ExceptT $ try $ runStdoutLoggingT (runReaderT f st)
