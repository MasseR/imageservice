module Json.Images
  ( Images(..)
  , getImages
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Foldable (class Foldable)

newtype Images = Images (Array String)

getImages :: Images-> Array String
getImages (Images img) = img

derive newtype instance encodeImages :: EncodeJson Images
derive newtype instance decodeImages :: DecodeJson Images
derive newtype instance showImages :: Show Images
derive newtype instance monoidImages :: Monoid Images
derive newtype instance semigroupImages :: Semigroup Images
