{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
module Network.HTTP.Client.Internal
  ( getRaw
  , newManager
  , tlsManagerSettings
  , HTTP.Manager
  , requestHeaders
  , responseTimeout
  , method
  , headRaw
  ) where

import           Control.Lens
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           MyPrelude
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import qualified Network.HTTP.Conduit         as HTTP
import           Network.HTTP.Types

requestHeaders :: Lens' HTTP.Request RequestHeaders
requestHeaders f r = (\x -> r{HTTP.requestHeaders = x}) <$> f (HTTP.requestHeaders r)

method :: Lens' HTTP.Request ByteString
method f r = (\x -> r{HTTP.method = x}) <$> f (HTTP.method r)

responseTimeout :: Lens' HTTP.Request HTTP.ResponseTimeout
responseTimeout f r = (\x -> r{HTTP.responseTimeout = x}) <$> f (HTTP.responseTimeout r)

-- | The raw method
--
-- Sets the user agent, otherwise a low level method
getRaw :: (MonadThrow m, MonadUnliftIO m) => HTTP.Manager -> String -> (HTTP.Request -> HTTP.Request) -> (HTTP.Response (ConduitM i ByteString (ResourceT m) ()) -> ResourceT m b) -> m b
getRaw manager url _ f = do
  req <- headers <$> HTTP.parseRequest url
  runResourceT $ do
    response <- HTTP.http req manager
    f response
  where
    headers = over requestHeaders (("User-Agent", "duplicator"):)

headRaw :: (MonadThrow m, MonadUnliftIO m) => HTTP.Manager -> String -> m (HTTP.Response LByteString)
headRaw manager url = do
  req <- headers <$> HTTP.parseRequest url
  HTTP.httpLbs req manager
  where
    headers = over requestHeaders (("User-Agent", "duplicator"):) . set method "HEAD"

-- getLbs :: (MonadThrow m, MonadUnliftIO m) => HTTP.Manager -> String -> m LByteString
-- getLbs manager url = getRaw manager url (\c -> runConduit $ HTTP.responseBody c .| sinkLbs)

newManager :: MonadIO m => m HTTP.Manager
newManager = liftIO $ HTTP.newManager tlsManagerSettings

-- withHttpFile :: (MonadThrow m, MonadUnliftIO m) => HTTP.Manager -> String -> (FilePath -> ResourceT m b) -> m b
-- withHttpFile manager url f =
--   getRaw manager url $ \c -> do
--     path <- runConduit $ HTTP.responseBody c .| sinkSystemTempFile "duplicateservice"
--     f path
