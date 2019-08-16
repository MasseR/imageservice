module API.QueryError
  ( QueryError(..)
  , parse
  ) where

import Prelude

import Data.Either (Either)
import Data.Bifunctor (lmap)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson, class DecodeJson)

import Affjax (ResponseFormatError, printResponseFormatError)

data QueryError
  = FormatError ResponseFormatError
  | DecodeError String

instance showQueryError :: Show QueryError where
  show = case _ of
              FormatError e -> printResponseFormatError e
              DecodeError e -> e

parse :: forall a. DecodeJson a => Either ResponseFormatError Json -> Either QueryError a
parse = lmap DecodeError <<< decodeJson <=< lmap FormatError
