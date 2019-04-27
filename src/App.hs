{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module App where

import           ClassyPrelude                hiding (cons)
import           Config
import           Control.Lens
import           Control.Monad.Catch          (MonadThrow)
import           Data.Acid                    (AcidState)
import           Data.BKTree
import           Data.Fingerprint             (Fingerprint)
import           Data.Generics.Product
import           Database                     (DB)
import           Logging
import           Network.HTTP.Client          (Authorization (..), Manager,
                                               MonadHTTP (..))
import qualified Network.HTTP.Client.Internal as Client
import           Network.HTTP.Images.Imgur    (HasImgur (..))
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
  get url auth f = do
    mgr <- asks manager
    Client.getRaw mgr url addAuth f
    where
      authHeader (Authorization a) = ("authorization", a)
      addAuth req = maybe req (\a -> over Client.requestHeaders (cons (authHeader a)) req) auth

instance HasImgur AppM where
  getImgurApp = view (field @"conf" . field @"services" . field @"imgur")

instance HasLog AppM where
  getLog = asks logAction
