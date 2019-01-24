{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Config where

import           ClassyPrelude
import           Dhall

data Worker = Reddit { subreddits :: [ Text ] } deriving (Generic, Interpret)

data Config = Config { port    :: Integer
                     , workers :: [Worker] } deriving (Generic, Interpret)
