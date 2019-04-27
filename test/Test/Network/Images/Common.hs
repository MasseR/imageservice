{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PackageImports             #-}
module Test.Network.Images.Common
  ( evalTestHTTP
  , TestHTTP(..)
  ) where


import           ClassyPrelude
import           Control.Monad.Trans.Resource
import           Data.Conduit.Binary                        (sourceLbs)
import           Network.HTTP.Client
import           "http-client" Network.HTTP.Client.Internal (Response (..),
                                                             ResponseClose (..))
import           Network.HTTP.Types.Status                  (status200)
import           Network.HTTP.Types.Version                 (http11)


newtype TestHTTP a = TestHTTP (ReaderT LByteString IO a)
  deriving (Functor, Applicative, Monad, MonadReader LByteString, MonadIO, MonadUnliftIO)

evalTestHTTP :: TestHTTP a -> LByteString -> IO a
evalTestHTTP (TestHTTP r) = runReaderT r

instance MonadHTTP TestHTTP where
  get _url f = ask >>= \body -> runResourceT (f (mkResponse body))
    where
      mkResponse x = Response status200 http11 [] (sourceLbs x) mempty (ResponseClose $ return ())
