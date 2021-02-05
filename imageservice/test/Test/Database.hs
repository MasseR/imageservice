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
  pure ()
