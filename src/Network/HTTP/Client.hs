{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client
  ( HTTP.Manager
  , getLbs
  , newManager
  , withHttpFile
  ) where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary          (sinkLbs, sinkSystemTempFile)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import qualified Network.HTTP.Conduit         as HTTP
import           Network.HTTP.Types

requestHeaders :: Lens' HTTP.Request RequestHeaders
requestHeaders f r = (\x -> r{HTTP.requestHeaders = x}) <$> f (HTTP.requestHeaders r)

-- | The raw method
--
-- Sets the user agent, otherwise a low level method
getRaw :: (MonadThrow m, MonadUnliftIO m) => HTTP.Manager -> String -> (HTTP.Response (ConduitM i ByteString (ResourceT m) ()) -> ResourceT m b) -> m b
getRaw manager url f = do
  req <- over requestHeaders (("User-Agent", "duplicator"):) <$> HTTP.parseRequest url
  runResourceT $ do
    response <- HTTP.http req manager
    f response

getLbs :: (MonadThrow m, MonadUnliftIO m) => HTTP.Manager -> String -> m LByteString
getLbs manager url = getRaw manager url (\c -> runConduit $ HTTP.responseBody c .| sinkLbs)

newManager :: MonadIO m => m HTTP.Manager
newManager = liftIO $ HTTP.newManager tlsManagerSettings

withHttpFile :: (MonadThrow m, MonadUnliftIO m) => HTTP.Manager -> String -> (FilePath -> ResourceT m b) -> m b
withHttpFile manager url f =
  getRaw manager url $ \c -> do
    path <- runConduit $ HTTP.responseBody c .| sinkSystemTempFile "duplicateservice"
    f path
