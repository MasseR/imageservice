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

import Json.Images (getImages, Images)

import App (Env(..))

import Data.Either (Either, either)
import Data.Bifunctor (lmap)

import Control.Monad.Reader.Trans (class MonadAsk, ask)

import Affjax (ResponseFormatError, printResponseFormatError)
import Affjax as AJAX
import Affjax.ResponseFormat (json)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)


type State
  = { query :: String
    , images :: Images }

data Action
  = HandleInput String
  | Find Event

component :: forall q i o m. MonadAsk Env m => MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { query: "", images: mempty }

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
      [ HH.ul_
        (map (\src -> HH.li_ [HH.img [ HP.src src ]]) (getImages $ state.images))
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

data QueryError
  = FormatError ResponseFormatError
  | DecodeError String

instance showQueryError :: Show QueryError where
  show = case _ of
              FormatError e -> printResponseFormatError e
              DecodeError e -> e


request :: forall m. MonadAff m => MonadAsk Env m => String -> m (Either QueryError Images)
request query = do
  Env{host} <- ask
  let url = host <> "/similar/5?url=" <> query
  parse <<< _.body <$> H.liftAff (corsGet url)
  where parse :: Either ResponseFormatError Json -> Either QueryError Images
        parse = lmap DecodeError <<< decodeJson <=< lmap FormatError
        corsGet url = AJAX.request
          AJAX.defaultRequest
          { url = url
          , responseFormat = json }

handleAction :: forall o m. MonadAsk Env m => MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput str ->
    H.modify_ \st -> st { query = str }
  Find event -> do
     H.liftEffect $ Event.preventDefault event
     query <- _.query <$> H.get
     images <- request query
     H.modify_ \st -> st { images = either mempty identity images }
