{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           App
import           ClassyPrelude
import           Colog.Core
import           Config
import           Data.Acid                   (closeAcidState, createCheckpoint,
                                              openLocalStateFrom)
import qualified Data.BKTree                 as BKTree
import qualified Database                    as DB
import           Dhall                       (auto, input)
import           Logging
import           Metrics
import           Network.HTTP.Client         (newManager)
import           Network.Wai.Handler.Warp    (run)
import qualified Network.Wai.Metrics         as Wai
import           Network.Wai.Middleware.Cors
import           Options.Generic
import           Server
import           System.Metrics
import           Worker.Cleaner              (cleaner)
import           Worker.Indexer              (indexer)

newtype Cmd = Cmd { config :: Maybe FilePath } deriving (Generic)

instance ParseRecord Cmd

main :: IO ()
main = do
  Cmd{..} <- getRecord "imageservice"
  conf@Config{port, dbPath, carbon} <- input auto (maybe "./sample.dhall" pack config)
  bracket (openLocalStateFrom (unpack dbPath) DB.initial) (\st -> createCheckpoint st >> closeAcidState st) $ \db -> do
    metrics@Metrics{store} <- createMetrics
    registerGcMetrics store
    waiMetrics <- Wai.registerWaiMetrics store
    tree <- HashTree <$> newTVarIO BKTree.empty
    manager <- newManager
    lock <- newMVar ()
    let logAction = LogAction $ \m -> withMVar lock (\_ -> putStrLn (format m))
    let app = App{..}
    _ <- runConcurrently $ (,,) <$> startCarbon carbon app <*> startApp app <*> startWebserver port waiMetrics app
    return ()
  where
    startCarbon Carbon{host, port} app = Concurrently $ void $ runReaderT (forkCarbon host port) app
    startApp app = Concurrently $ void $ runReaderT (runApp (cleaner >> logLevel Info "Starting indexer" >> indexer)) app
    startWebserver port waiMetrics app = Concurrently $ run (fromIntegral port) (Wai.metrics waiMetrics (simpleCors $ application app))
