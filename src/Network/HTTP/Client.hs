{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
module Network.HTTP.Client
  ( Client.Manager
  , Client.newManager
  , getLbs
  , withHttpFile
  , MonadHTTP(..)
  ) where

import           ClassyPrelude
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary          (sinkLbs, sinkSystemTempFile)
import qualified Network.HTTP.Conduit         as HTTP

import qualified Network.HTTP.Client.Internal as Client

class Monad m => MonadHTTP m where
  get :: String -> (HTTP.Response (ConduitM i ByteString (ResourceT m) ()) -> ResourceT m b) -> m b

getLbs :: MonadHTTP m => String -> m LByteString
getLbs url = get url (\c -> runConduit $ HTTP.responseBody c .| sinkLbs)

withHttpFile :: (MonadHTTP m, MonadIO m) => String -> (FilePath -> ResourceT m b) -> m b
withHttpFile url f =
  get url $ \c -> do
    path <- runConduit $ HTTP.responseBody c .| sinkSystemTempFile "duplicateservice"
    f path
