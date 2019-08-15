module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Form (component)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff $ do
  liftEffect $ log "Starting app"
  body <- HA.awaitBody
  runUI component unit body
