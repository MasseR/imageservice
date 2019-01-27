{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App where

import           ClassyPrelude
import           Config
import           Control.Monad.Logger
import           Data.Acid            (AcidState)
import           Data.BKTree
import           Data.Fingerprint     (Fingerprint)
import           Database             (DB)
import           Network.HTTP.Client  (Manager)
import System.Metrics (Store)

newtype HashTree = HashTree (TVar (BKTree Fingerprint))

newtype Metrics = Metrics { store :: Store } deriving Generic
data App = App { tree    :: HashTree
               , manager :: Manager
               , conf    :: Config
               , db      :: AcidState DB
               , metrics :: Metrics }
         deriving Generic

type AppM = ReaderT App (LoggingT IO)
