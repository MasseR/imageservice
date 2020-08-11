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
import           Control.Concurrent          (threadDelay)
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

import           Database.SQLite.Simple      (execute_, withConnection)
import           System.FilePath             ((</>))

newtype Cmd = Cmd { config :: Maybe FilePath } deriving (Generic)

instance ParseRecord Cmd

main :: IO ()
main = do
  Cmd{..} <- getRecord "imageservice"
  conf@Config{port, dbPath, carbon} <- input auto (maybe "./sample.dhall" pack config)
  withConnection (unpack dbPath </> "imageservice.db") $ \conn -> do
    hSetBuffering stdout LineBuffering
    metrics@Metrics{store} <- createMetrics
    registerGcMetrics store
    waiMetrics <- Wai.registerWaiMetrics store
    tree <- HashTree <$> newTVarIO BKTree.empty
    manager <- newManager
    lock <- newMVar ()
    traverse_ (execute_ conn) schema
    _store <- DB.mkStore conn
    let logAction = LogAction $ \m -> withMVar lock (\_ -> putStrLn (format m))
    let app = App{..}
    for_ carbon $ \c -> startCarbon c app
    withAsync (startApp app) $ \a -> do
      startWebserver port waiMetrics app
      wait a
  where
    schema =
      [ "create table if not exists fingerprints (path, hash, checked)"
      , "create index if not exists fingerprint_path on fingerprints (path)"
      ]
    startCarbon Carbon{host, port} = runReaderT (forkCarbon host port)
    sevenDays :: Int
    sevenDays = 60 * 10 ^ (9 :: Int) * 60 * 24 * 7
    cleanerDaemon = forever $ do
      cleaner
      liftIO $ threadDelay sevenDays
    daemon =
      withAsync cleanerDaemon $ \waitCleaner -> indexer >> wait waitCleaner
    startApp app = void $ runReaderT (runApp daemon) app
    startWebserver port waiMetrics app = run (fromIntegral port) (Wai.metrics waiMetrics (simpleCors $ application app))
