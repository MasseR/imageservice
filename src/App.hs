{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App where

import           ClassyPrelude       hiding (Handler)
import           Config
import           Data.BKTree
import           Data.Fingerprint    (Fingerprint)
import           Network.HTTP.Client (Manager)
import           Servant             (Handler)

newtype HashTree = HashTree (TVar (BKTree Fingerprint))

data App = App { tree    :: HashTree
               , manager :: Manager
               , conf    :: Config}
         deriving Generic

type AppM = ReaderT App Handler
