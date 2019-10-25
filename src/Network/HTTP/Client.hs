{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
module Network.HTTP.Client
  ( Client.Manager
  , Client.newManager
  , getLbs
  , getLbsAuth
  , withHttpFile
  , MonadHTTP(..)
  , Authorization(..)
  ) where

import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary          (sinkLbs, sinkSystemTempFile)
import           MyPrelude
import qualified Network.HTTP.Conduit         as HTTP

import qualified Network.HTTP.Client.Internal as Client

newtype Authorization = Authorization ByteString

class Monad m => MonadHTTP m where
  get :: String -> Maybe Authorization -> (HTTP.Response (ConduitM i ByteString (ResourceT m) ()) -> ResourceT m b) -> m b
  poke :: String -> m (HTTP.Response LByteString)

getLbs :: MonadHTTP m => String -> m LByteString
getLbs url = get url Nothing (\c -> runConduit $ HTTP.responseBody c .| sinkLbs)

getLbsAuth :: MonadHTTP m => String -> Authorization -> m LByteString
getLbsAuth url auth = get url (Just auth) (\c -> runConduit $ HTTP.responseBody c .| sinkLbs)

withHttpFile :: (MonadHTTP m, MonadIO m) => String -> (FilePath -> ResourceT m b) -> m b
withHttpFile url f =
  get url Nothing $ \c -> do
    path <- runConduit $ HTTP.responseBody c .| sinkSystemTempFile "duplicateservice"
    f path
