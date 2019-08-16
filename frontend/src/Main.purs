module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Form (component)
import App (Env(..), runAppM)

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Config (Config(..), BaseURL(..))

main :: Effect Unit
main = HA.runHalogenAff $ do
  liftEffect $ log "Starting app"
  body <- HA.awaitBody
  let rootComponent = H.hoist (runAppM env) component
      env = Env { config: Config { baseUrl: BaseURL "https://duplicates.introitu.info" } }
  runUI rootComponent unit body
