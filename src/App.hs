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

newtype HashTree = HashTree (TVar (BKTree Fingerprint))

data App = App { tree    :: HashTree
               , manager :: Manager
               , conf    :: Config
               , db      :: AcidState DB}
         deriving Generic

type AppM = ReaderT App (LoggingT IO)
