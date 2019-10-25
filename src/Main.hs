{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           App
import           Colog.Core
import           Config
import           Data.Acid                   (closeAcidState, createCheckpoint,
                                              openLocalStateFrom)
import qualified Data.BKTree                 as BKTree
import qualified Database                    as DB
import           Dhall                       (auto, input)
import           Logging
import           Metrics
import           MyPrelude
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
    hSetBuffering stdout LineBuffering
    metrics@Metrics{store} <- createMetrics
    registerGcMetrics store
    waiMetrics <- Wai.registerWaiMetrics store
    tree <- HashTree <$> newTVarIO BKTree.empty
    manager <- newManager
    lock <- newMVar ()
    let logAction = LogAction $ \m -> withMVar lock (\_ -> putStrLn (format m))
    let app = App{..}
    for_ carbon $ \c -> startCarbon c app
    withAsync (startApp db app) $ \a -> do
      startWebserver port waiMetrics app
      wait a
  where
    startCarbon Carbon{host, port} = runReaderT (forkCarbon host port)
    startApp db app = void $ runReaderT (runApp (cleaner >> liftIO (createCheckpoint db) >> indexer)) app
    startWebserver port waiMetrics app = run (fromIntegral port) (Wai.metrics waiMetrics (simpleCors $ application app))
