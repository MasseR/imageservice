module API.Images
  ( fetchImages
  , Images(..)
  , getImages
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

import Data.Newtype (class Newtype, un)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)

import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Effect.Aff.Class (class MonadAff, liftAff)

newtype Images = Images (Array String)

getImages :: Images -> Array String
getImages = un Images

derive instance newtypeImages :: Newtype Images _

derive newtype instance encodeImages :: EncodeJson Images
derive newtype instance decodeImages :: DecodeJson Images
derive newtype instance showImages :: Show Images
derive newtype instance monoidImages :: Monoid Images
derive newtype instance semigroupImages :: Semigroup Images

fetchImages :: forall m. MonadAff m => MonadAsk Env m => String -> m (Either QueryError Images)
fetchImages query = do
  host <- view hostL <$> ask
  let url = host <> "/similar/5?url=" <> query
  parse <<< _.body <$> liftAff (corsGet url)
  where parse :: Either ResponseFormatError Json -> Either QueryError Images
        parse = lmap DecodeError <<< decodeJson <=< lmap FormatError
        hostL = configL <<< baseUrlL <<< _Newtype
        corsGet url = AJAX.request
          AJAX.defaultRequest
          { url = url
          , responseFormat = json }
