module API.Size
  ( fetchSize
  ) where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat (json)
import Affjax (ResponseFormatError)

import API.QueryError (QueryError(..))

import App (configL, Env)
import Config (baseUrlL)

import Data.Either (Either)
import Data.Bifunctor (lmap)

import Data.Lens (view)
import Data.Lens.Iso.Newtype (_Newtype)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)

import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Effect.Aff.Class (class MonadAff, liftAff)

fetchSize :: forall m. MonadAff m => MonadAsk Env m => m (Either QueryError Int)
fetchSize = do
  host <- view hostL <$> ask
  let url = host <> "/database/size"
  parse <<< _.body <$> liftAff (AJAX.get json url)
  where parse :: Either ResponseFormatError Json -> Either QueryError Int
        parse = lmap DecodeError <<< decodeJson <=< lmap FormatError
        hostL = configL <<< baseUrlL <<< _Newtype
