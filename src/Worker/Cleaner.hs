{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Worker.Cleaner
  ( cleaner
  ) where

import           App
import           Logging
import           MyPrelude

import           Data.Fingerprint     (Fingerprint (..))

import           Control.Lens         (Lens', over)
import           Data.Time            (addDays)
import           Network.HTTP.Client  (poke)
import qualified Network.HTTP.Conduit as HTTP
import           Network.HTTP.Types   (status200)

import           Database

days :: Lens' UTCTime Day
days f r = (\x -> r{utctDay=x}) <$> f (utctDay r)


cleaner :: AppM ()
cleaner = do
  now <- liftIO getCurrentTime
  modifyFingerprint $ \case
    fp@Fingerprint{checked=Just _then}
      | over days (addDays 7) _then >= now -> pure (Just fp)
    fp@Fingerprint{imagePath} -> do
      logInfo $ "Checking the status of " <> imagePath
      status <- try @_ @HTTP.HttpException (HTTP.responseStatus <$> poke (unpack imagePath))
      pure (either (const Nothing) (\s -> if s == status200 then Just fp{checked=Just now} else Nothing) status)
