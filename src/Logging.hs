{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module Logging
  (

    LogState
  , HasLogState(..)
  , WithLog
  , LogStateM(..)
  , Katip
  , KatipContext
  , withLogState

  , logInfo
  , logWarning
  )
  where

import           MyPrelude

import           Control.Lens          (Lens', over, view)
import           Data.Generics.Product (typed)

import           Control.Monad.Reader  (local)

import           GHC.Stack

import           Katip

type WithLog m = (Katip m, KatipContext m, MonadIO m)

data LogState = LogState
    { namespace :: Namespace
    , context   :: LogContexts
    , logEnv    :: LogEnv
    }
    deriving (Generic)

class HasLogState a where
  logState :: Lens' a LogState

newtype LogStateM r a = LogStateM (ReaderT r IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (HasLogState r) => Katip (LogStateM r) where
  getLogEnv = view (logState . typed @LogEnv)
  localLogEnv f = local (over (logState . typed @LogEnv) f)

instance (HasLogState r) => KatipContext (LogStateM r) where
  getKatipContext = view (logState . typed @LogContexts)
  localKatipContext f = local (over (logState . typed @LogContexts) f)
  getKatipNamespace = view (logState . typed @Namespace)
  localKatipNamespace f = local (over (logState . typed @Namespace) f)

withLogState :: (LogState -> IO a) -> IO a
withLogState f = do
  scribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" scribe defaultScribeSettings =<< initLogEnv "imageservice" "production"
  bracket mkLogEnv closeScribes $ \le ->
    f LogState{namespace=mempty, context=mempty, logEnv=le}

logInfo :: (Katip m, KatipContext m) => HasCallStack => Text -> m ()
logInfo = withFrozenCallStack (logLocM InfoS . ls)

logWarning :: (Katip m, KatipContext m) => HasCallStack => Text -> m ()
logWarning = withFrozenCallStack (logLocM WarningS . ls)
