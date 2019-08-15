module Form
  ( component
  ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

import Web.Event.Event (Event)
import Web.Event.Event as Event

type State
  = { query :: String }

data Action
  = HandleInput String
  | Find Event

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { query: "" }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section
    [ section ]
    [ HH.div
      [ container ]
      [ HH.h1 [ title ] [ HH.text "Find similar images" ]
      , HH.form
        [ HE.onSubmit (pure <<< Find) ]
        [ HH.input [ input, text, help]
        , HH.button [ button ] [ HH.text "Find" ]
        ]
      ]
    ]
  where section = HP.class_ (H.ClassName "section")
        container = HP.class_ (H.ClassName "container")
        title = HP.class_ (H.ClassName "title")
        subtitle = HP.class_ (H.ClassName "subtitle")
        input = HP.class_ (H.ClassName "input")
        text = HP.type_ HP.InputText
        help = HP.placeholder "URL"
        button = HP.class_ (H.ClassName "button is-primary")

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput str ->
    H.modify_ \st -> st { query = str }
  Find event -> do
     H.liftEffect $ Event.preventDefault event
     H.liftEffect $ log "Asd"
