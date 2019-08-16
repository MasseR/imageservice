module API.QueryError
  ( QueryError(..)
  ) where

import Prelude

import Affjax (ResponseFormatError, printResponseFormatError)

data QueryError
  = FormatError ResponseFormatError
  | DecodeError String

instance showQueryError :: Show QueryError where
  show = case _ of
              FormatError e -> printResponseFormatError e
              DecodeError e -> e

