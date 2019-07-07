{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
module Worker.Cleaner
  ( cleaner
  ) where

import           App
import           ClassyPrelude             hiding (foldr)
import qualified Data.BKTree               as BKTree
import           Data.Fingerprint          (Fingerprint (..))
import           Data.Foldable             (foldr)
import           Database
import           Logging
import           Network.HTTP.Client       (poke)
import qualified Network.HTTP.Conduit      as HTTP
import           Network.HTTP.Types.Status (status200)
import           UnliftIO.Async            (pooledMapConcurrentlyN)

cleaner :: AppM ()
cleaner = do
  newIndex <- foldr filtered BKTree.empty <$> refresh
  update (Replace newIndex)
  where
    refresh = query Dump >>= pooledMapConcurrentlyN 10 stillExists
    filtered :: (Bool, Fingerprint) -> BKTree.BKTree Fingerprint -> BKTree.BKTree Fingerprint
    filtered (True, fp) acc = BKTree.insert fp acc
    filtered (False, _) acc = acc
    stillExists :: Fingerprint -> AppM (Bool, Fingerprint)
    stillExists fp@Fingerprint{imagePath} = do
      status <- HTTP.responseStatus <$> poke (unpack imagePath)
      logLevel Debug (tshow status)
      pure (status200 == status, fp)
