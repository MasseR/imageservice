{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module App where

import           ClassyPrelude
import           Config
import           Control.Monad.Catch          (MonadThrow)
import           Data.Acid                    (AcidState)
import           Data.BKTree
import           Data.Fingerprint             (Fingerprint)
import           Database                     (DB)
import           Logging
import           Network.HTTP.Client          (Manager, MonadHTTP (..))
import qualified Network.HTTP.Client.Internal as Client
import           System.Metrics               (Store)

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

newtype AppM a =
  AppM { runApp :: ReaderT App IO a }
  deriving (MonadReader App, Monad, Functor, Applicative, MonadIO, MonadThrow, MonadUnliftIO)


instance MonadHTTP AppM where
  get url f = do
    mgr <- asks manager
    Client.getRaw mgr url f

instance HasLog AppM where
  getLog = asks logAction
