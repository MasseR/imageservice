{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module App where

import           ClassyPrelude
import           Config
import           Data.Acid           (AcidState)
import           Data.BKTree
import           Data.Fingerprint    (Fingerprint)
import           Database            (DB)
import           Logging
import           Network.HTTP.Client (Manager)
import           System.Metrics      (Store)

newtype HashTree = HashTree (TVar (BKTree Fingerprint))

newtype Metrics = Metrics { store :: Store } deriving Generic

data App = App { tree      :: HashTree
               , manager   :: Manager
               , conf      :: Config
               , db        :: AcidState DB
               , metrics   :: Metrics
               , logAction :: LogAction IO LogMsg
               }
         deriving Generic

type AppM = ReaderT App IO

instance (MonadReader App m) => HasLog m where
  getLog = asks logAction
