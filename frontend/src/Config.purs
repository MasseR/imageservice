module Config
  ( Config(..)
  , BaseURL(..)
  , baseUrlL
  ) where

import Prelude

import Data.Newtype (class Newtype, un, over)

import Data.Lens (lens, Lens')

import Data.Argonaut.Decode.Class (class DecodeJson)

newtype BaseURL = BaseURL String

derive instance newtypeBaseURL :: Newtype BaseURL _

derive newtype instance eqBaseURL :: Eq BaseURL
derive newtype instance ordBaseURL :: Ord BaseURL
derive newtype instance showBaseURL :: Show BaseURL
derive newtype instance decodeBaseURL :: DecodeJson BaseURL

newtype Config
  = Config { baseUrl :: BaseURL }

derive instance newtypeConfig :: Newtype Config _

derive newtype instance eqConfig :: Eq Config
derive newtype instance ordConfig :: Ord Config
derive newtype instance showConfig :: Show Config
derive newtype instance decodeConfig :: DecodeJson Config

baseUrlL :: Lens' Config BaseURL
baseUrlL = lens
  (\conf -> (un Config conf).baseUrl)
  (\conf x -> (over Config (_ {baseUrl = x}) conf))
