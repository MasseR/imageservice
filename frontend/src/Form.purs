module Form
  ( component
  ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Effect.Aff.Class (class MonadAff)

import Web.Event.Event (Event)
import Web.Event.Event as Event

import API.Images (fetchImages, getImages, Images)

import App (Env)

import Data.Either (either)
import Data.Array (length)

import Control.Monad.Reader.Trans (class MonadAsk)


type State
  = { query :: String
    , images :: Images
    , size :: Int }

data Action
  = HandleInput String
  | Find Event

component :: forall q o m. MonadAsk Env m => MonadAff m => H.Component HH.HTML q Int o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Int -> State
initialState size = { query: "", images: mempty, size: size }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section
    [ section ]
    [ HH.div
      [ container ]
      [ HH.h1 [ title ] [ HH.text "Find similar images" ]
      , HH.form
        [ HE.onSubmit (pure <<< Find) ]
        [ HH.input [ input, text, help, HE.onValueInput (pure <<< HandleInput)]
        , HH.button [ button ] [ HH.text "Find" ]
        ]
      ]
    , HH.div
      [ container ]
      [ HH.h3 [ title ] [ HH.text resultSize ]
      , HH.ul_
        (map (\src -> HH.li_ [HH.img [ HP.src src ]]) images)
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
        images :: Array String
        images = getImages state.images
        resultSize = show (length images) <> " / " <> show state.size




handleAction :: forall o m. MonadAsk Env m => MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput str ->
    H.modify_ \st -> st { query = str }
  Find event -> do
     H.liftEffect $ Event.preventDefault event
     query <- _.query <$> H.get
     images <- fetchImages query
     H.modify_ \st -> st { images = either mempty identity images }
