module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)

import Form (component)
import App (Env(..), runAppM)

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Data.Argonaut.Decode.Class (decodeJson)

import Affjax (printResponseFormatError)
import Affjax as AJAX
import Affjax.ResponseFormat (json)

import Data.Either (Either(..))


main :: Effect Unit
main = HA.runHalogenAff $ do
  eConfig <- map decodeJson <<< _.body <$> AJAX.get json "config.json"
  case eConfig of
       Right (Right config) -> do
         body <- HA.awaitBody
         let rootComponent = H.hoist (runAppM env) component
             env = Env { config }
         _ <- runUI rootComponent unit body
         pure unit
       Right (Left e) -> liftEffect $ log e
       Left e -> liftEffect $ log (printResponseFormatError e)
