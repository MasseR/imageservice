{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Database where

import           Control.Lens          (at, view)
import           Control.Monad.State
import           Data.Acid             (AcidState, Query, QueryEvent, Update,
                                        UpdateEvent, makeAcidic)
import qualified Data.Acid             as Acid
import           Data.Acid.Advanced    (MethodResult, MethodState)
import           Data.BKTree           (BKTree)
import qualified Data.BKTree           as BK
import           Data.Fingerprint      (Fingerprint (..))
import           Data.Generics.Product
import qualified Data.Map.Strict       as M
import           Data.Monoid
import           Data.SafeCopy
import           MyPrelude

data DB = DB { index  :: BKTree Fingerprint
             , urlMap :: Map Text Fingerprint }
        deriving (Generic)

deriveSafeCopy 0 'base ''DB

insert :: Fingerprint -> Update DB ()
insert fp@Fingerprint{imagePath} = modify alt
  where
    alt DB{..} = DB (BK.insert fp index) (M.insert imagePath fp urlMap)

lookupFingerprint :: Text -> Query DB (Maybe Fingerprint)
lookupFingerprint url = view (at url) <$> asks urlMap

lookupSimilar :: Int -> Fingerprint -> Query DB [Fingerprint]
lookupSimilar n fp = BK.search n fp <$> asks index

dump :: Query DB (BKTree Fingerprint)
dump = asks index

replace :: BKTree Fingerprint -> Update DB ()
replace newIndex = put DB { index = newIndex
                          , urlMap = foldMap toMap newIndex
                          }
  where
    toMap fp@Fingerprint{imagePath} = M.singleton imagePath fp

size :: Query DB Int
size = getSum . foldMap (const (Sum 1)) <$> asks index

makeAcidic ''DB ['insert, 'lookupFingerprint, 'lookupSimilar, 'size, 'dump, 'replace]

initial :: DB
initial = DB BK.empty mempty

type WithDB event r m = (MethodState event ~ DB, HasType (AcidState DB) r, MonadReader r m)

type WithQuery event r m = (QueryEvent event, WithDB event r m)

type WithUpdate event r m = (UpdateEvent event, WithDB event r m)

query :: (WithQuery event r m, MonadIO m) => event -> m (MethodResult event)
query ev = view (typed @(AcidState DB)) >>= \st -> liftIO (Acid.query st ev)

update :: (WithUpdate event r m, MonadIO m) => event -> m (MethodResult event)
update ev = view (typed @(AcidState DB)) >>= \st -> liftIO (Acid.update st ev)
