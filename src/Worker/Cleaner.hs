{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Worker.Cleaner
  ( cleaner
  ) where

import           App
import           ClassyPrelude             hiding (foldr)
import           Control.Lens
import qualified Data.BKTree               as BKTree
import           Data.Fingerprint          (Fingerprint (..))
import           Data.Foldable             (foldr)
import           Data.Time.Calendar        (addDays)
import           Database
import           Logging
import           Network.HTTP.Client       (poke)
import qualified Network.HTTP.Conduit      as HTTP
import           Network.HTTP.Types.Status (status200)
import           UnliftIO.Async            (pooledMapConcurrentlyN)

days :: Lens' UTCTime Day
days f r = (\x -> r{utctDay=x}) <$> f (utctDay r)


cleaner :: AppM ()
cleaner = do
  now <- liftIO getCurrentTime
  newIndex <- foldr insertExisting BKTree.empty <$> refresh now
  update (Replace newIndex)
  where
    refresh now = query Dump >>= pooledMapConcurrentlyN 10 (stillExists now)
    insertExisting :: (Bool, Fingerprint) -> BKTree.BKTree Fingerprint -> BKTree.BKTree Fingerprint
    insertExisting (True, fp) acc = BKTree.insert fp acc
    insertExisting (False, _) acc = acc
    newerThan Nothing _          = False
    newerThan (Just checked) now = over days (addDays 7) checked >= now
    stillExists :: UTCTime -> Fingerprint -> AppM (Bool, Fingerprint)
    stillExists now fp@Fingerprint{imagePath, checked}
      | checked `newerThan` now = pure (True, fp)
      | otherwise = do
          status <- try @_ @HTTP.HttpException (HTTP.responseStatus <$> poke (unpack imagePath))
          logLevel Debug (tshow status)
          pure $ either (const (False, fp)) (\s -> (s == status200, fp{checked=Just now})) status
