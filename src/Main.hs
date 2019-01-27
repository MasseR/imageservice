{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           App
import           ClassyPrelude
import           Config
import           Control.Monad.Logger
import           Data.Acid                (closeAcidState, createCheckpoint,
                                           openLocalStateFrom)
import qualified Data.BKTree              as BKTree
import qualified Database                 as DB
import           Dhall                    (auto, input)
import           Network.HTTP.Client      (newManager)
import           Network.Wai.Handler.Warp (run)
import           Options.Generic
import           Server
import           Worker.Indexer           (indexer)
import System.Metrics
import qualified Network.Wai.Metrics as Wai

newtype Cmd = Cmd { config :: Maybe FilePath } deriving (Generic)

instance ParseRecord Cmd

main :: IO ()
main = do
  Cmd{..} <- getRecord "imageservice"
  conf@Config{port, dbPath} <- input auto (maybe "./sample.dhall" pack config)
  bracket (openLocalStateFrom (unpack dbPath) DB.initial) (\st -> createCheckpoint st >> closeAcidState st) $ \db -> do
    metrics@(Metrics{store}) <- Metrics <$> newStore
    registerGcMetrics store
    waiMetrics <- Wai.registerWaiMetrics store
    tree <- HashTree <$> newTVarIO BKTree.empty
    manager <- newManager
    let app = App{..}
    void $ async (runStdoutLoggingT (runReaderT indexer app))
    run (fromIntegral port) (Wai.metrics waiMetrics (application app))
