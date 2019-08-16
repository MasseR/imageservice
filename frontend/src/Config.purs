module Config
  ( Config(..)
  , BaseURL(..)
  , baseUrlL
  ) where

import Data.Newtype (class Newtype, un, over)

import Data.Lens (lens, Lens')

newtype BaseURL = BaseURL String

derive instance newtypeBaseURL :: Newtype BaseURL _

newtype Config
  = Config { baseUrl :: BaseURL }

derive instance newtypeConfig :: Newtype Config _

baseUrlL :: Lens' Config BaseURL
baseUrlL = lens
  (\conf -> (un Config conf).baseUrl)
  (\conf x -> (over Config (_ {baseUrl = x}) conf))
