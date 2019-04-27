{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Network.HTTP.Images.Types
  ( ImgParser
  , HrefF(..)
  , Href
  ) where

import           ClassyPrelude

type ImgParser = LByteString -> [Href]


data HrefF a = RawImg a
            | ImgurAlbum a
            | Reject a
          deriving (Show, Eq, Generic, Functor)
type Href = HrefF String

