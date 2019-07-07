{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module Config where

import           ClassyPrelude
import           Dhall

newtype Worker = Reddit { subreddits :: [ Text ] } deriving (Generic)

newtype Token = Token Text deriving (Generic, Interpret)

newtype Services = Services { imgur :: Token } deriving (Generic)

data Carbon = Carbon { host :: Text
                     , port :: Integer }
            deriving (Generic)

data Config = Config { port     :: Integer
                     , dbPath   :: Text
                     , carbon   :: Carbon
                     , services :: Services
                     , workers  :: [Worker] } deriving (Generic)

instance Interpret Config
instance Interpret Services
instance Interpret Worker
instance Interpret Carbon
