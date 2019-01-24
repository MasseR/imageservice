{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           App
import           ClassyPrelude
import qualified Data.BKTree              as BKTree
import           Network.Wai.Handler.Warp (run)
import           Options.Generic
import           Server

newtype Cmd = Cmd { port :: Maybe Int } deriving (Generic)

instance ParseRecord Cmd

main :: IO ()
main = do
  Cmd{..} <- getRecord "imageservice"
  tree <- newTVarIO BKTree.empty
  let app = App{..}
  run (fromMaybe 8000 port) (application app)
