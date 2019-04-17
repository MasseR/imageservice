{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module Config where

import           ClassyPrelude
import           Dhall

newtype Worker = Reddit { subreddits :: [ Text ] } deriving (Generic)

newtype Token = Token Text deriving (Generic, Interpret)

newtype Services = Services { imgur :: Text } deriving (Generic)

data Config = Config { port     :: Integer
                     , dbPath   :: Text
                     , services :: Services
                     , workers  :: [Worker] } deriving (Generic)

instance Interpret Config
instance Interpret Services
instance Interpret Worker
