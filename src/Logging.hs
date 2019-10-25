{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Logging
  ( logLevel
  , format
  , LogMsg
  , HasLog(..)
  , LogAction
  , module Colog.Core.Severity
  )
  where

import           Colog.Core          hiding (HasLog)
import           Colog.Core.Severity
import           Data.Time.Format    (iso8601DateFormat)
import           MyPrelude

data LogMsg = LogMsg UTCTime Severity Text

class HasLog m where
  getLog :: m (LogAction IO LogMsg)

logLevel :: forall m r. (MonadIO m, MonadReader r m, HasLog m) => Severity -> Text -> m ()
logLevel severity msg = do
  l <- getLog
  now <- liftIO getCurrentTime
  liftIO (unLogAction l (LogMsg now severity msg))

format :: LogMsg -> Text
format = \case
  LogMsg time severity msg ->
    pack (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) time)
    <> " " <> tshow severity
    <> " " <> msg
