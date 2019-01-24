{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App where

import           ClassyPrelude    hiding (Handler)
import           Data.BKTree
import           Data.Fingerprint (Fingerprint)
import           Servant          (Handler)

newtype App = App { tree :: TVar (BKTree Fingerprint) } deriving Generic

type AppM = ReaderT App Handler
