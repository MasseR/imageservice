{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
module Network.HTTP.Client where

import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Types
import ClassyPrelude
import Control.Lens
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary (sinkLbs)

requestHeaders :: Lens' HTTP.Request RequestHeaders
requestHeaders f r = (\x -> r{HTTP.requestHeaders = x}) <$> f (HTTP.requestHeaders r)

-- | The raw method
--
-- Sets the user agent, otherwise a low level method
getRaw :: (MonadThrow m, MonadUnliftIO m) => HTTP.Manager -> (HTTP.Response (ConduitM i ByteString (ResourceT m) ()) -> ResourceT m b) -> String -> m b
getRaw manager f url = do
  req <- over requestHeaders (("User-Agent", "duplicator"):) <$> HTTP.parseRequest url
  runResourceT $ do
    response <- HTTP.http req manager
    f response

getLbs :: (MonadThrow m, MonadUnliftIO m) => HTTP.Manager -> String -> m LByteString
getLbs manager url = getRaw manager (\c -> runConduit $ HTTP.responseBody c .| sinkLbs) url
