module API.Size
  ( fetchSize
  ) where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat (json)

import API.QueryError (QueryError, parse)

import App (configL, Env)
import Config (baseUrlL)

import Data.Either (Either)

import Data.Lens (view)
import Data.Lens.Iso.Newtype (_Newtype)

import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Effect.Aff.Class (class MonadAff, liftAff)

fetchSize :: forall m. MonadAff m => MonadAsk Env m => m (Either QueryError Int)
fetchSize = do
  host <- view hostL <$> ask
  let url = host <> "/database/size"
  parse <<< _.body <$> liftAff (AJAX.get json url)
  where hostL = configL <<< baseUrlL <<< _Newtype
