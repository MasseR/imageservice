{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Test.Database where

import           Test.Hspec

import           Database

import           Test.Validity

import           Data.Fingerprint

import           Control.Monad.State

import           Control.Lens
import           Data.Foldable         (toList)
import           Data.Generics.Product
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

spec :: Spec
spec = describe "DB properties" $ do
  genValidSpec @DB
  it "forall u. forall db. u in db'.index && u in db'.urlMap, db' = insert u db" $
    forAllValid @Fingerprint $ \fp -> forAllValid @DB $ \db ->
      let db' = execState (insert fp) db
          url = view (field @"imagePath") fp
          seen = S.fromList . M.keys . view (field @"urlMap") $ db'
          imgIndex =  S.fromList . fmap (view (field @"imagePath")) . toList . view (field @"index") $ db'
      in url `S.member` seen && url `S.member` imgIndex
  it "Finds with url" $
    forAllValid @Fingerprint $ \fp -> forAllValid @DB $ \db ->
      let db' = execState (insert fp) db
          url = view (field @"imagePath") fp
      in Just fp == lookupFingerprint url db'
