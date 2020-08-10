{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Worker.Cleaner
  ( cleaner
  ) where

import           App
import           Logging
import           MyPrelude

-- days :: Lens' UTCTime Day
-- days f r = (\x -> r{utctDay=x}) <$> f (utctDay r)


cleaner :: AppM ()
cleaner = do
  logLevel Info $ "Not reimplemented"
  -- now <- liftIO getCurrentTime
  -- refreshed <- refresh now
  -- let (newIndex, totalElements, retained) = L.fold folds refreshed
  -- logLevel Info $ tshow totalElements
  -- logLevel Info $ tshow retained
  -- update (Replace newIndex)
  -- where
  --   folds = (,,) <$> rebuild <*> L.genericLength @Int <*> retain
  --   -- Count how many elements are retained after a restart
  --   retain :: L.Fold (Bool,a) Double
  --   retain = (/) <$> L.foldMap (bool 0 1 . fst) getSum <*> L.genericLength
  --   -- Rebuild the bktree from a list of retained elements
  --   rebuild = L.Fold (flip insertExisting) BKTree.empty id
  --   refresh now = query Dump >>= pooledMapConcurrentlyN 10 (stillExists now)
  --   insertExisting :: (Bool, Fingerprint) -> BKTree.BKTree Fingerprint -> BKTree.BKTree Fingerprint
  --   insertExisting (True, fp) acc = BKTree.insert fp acc
  --   insertExisting (False, _) acc = acc
  --   newerThan Nothing _          = False
  --   newerThan (Just checked) now = over days (addDays 7) checked >= now
  --   stillExists :: UTCTime -> Fingerprint -> AppM (Bool, Fingerprint)
  --   stillExists now fp@Fingerprint{imagePath, checked}
  --     | checked `newerThan` now = pure (True, fp)
  --     | otherwise = do
  --         status <- try @_ @HTTP.HttpException (HTTP.responseStatus <$> poke (unpack imagePath))
  --         pure $ either (const (False, fp)) (\s -> (s == status200, fp{checked=Just now})) status
