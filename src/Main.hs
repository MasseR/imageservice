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
import qualified Data.BKTree              as BKTree
import           Dhall                    (auto, input)
import           Network.HTTP.Client      (newManager)
import           Network.Wai.Handler.Warp (run)
import           Options.Generic
import           Server
import           Worker.Indexer           (indexer)

newtype Cmd = Cmd { config :: Maybe FilePath } deriving (Generic)

instance ParseRecord Cmd

main :: IO ()
main = do
  Cmd{..} <- getRecord "imageservice"
  conf@Config{port} <- input auto (maybe "./sample.dhall" pack config)
  tree <- HashTree <$> newTVarIO BKTree.empty
  manager <- newManager
  let app = App{..}
  void $ async (runStdoutLoggingT (runReaderT indexer app))
  run (fromIntegral port) (application app)
