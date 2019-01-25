{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App where

import           ClassyPrelude
import           Config
import           Control.Monad.Logger
import           Data.BKTree
import           Data.Fingerprint     (Fingerprint)
import           Network.HTTP.Client  (Manager)

newtype HashTree = HashTree (TVar (BKTree Fingerprint))

data App = App { tree    :: HashTree
               , manager :: Manager
               , conf    :: Config}
         deriving Generic

type AppM = ReaderT App (LoggingT IO)
