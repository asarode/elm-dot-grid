module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Array exposing (Array)
import Time exposing (Time)
import Task exposing (Task)
import AnimationFrame
import Signal
import StartApp

-- MODEL
type alias Radius =
  Float

type alias Dot =
  { radius : Radius
  }

new : Radius -> Dot
new size =
  { radius = size
  }

initDots : Array Dot
initDots =
  Array.fromList
  [ (Dot 10)
  , (Dot 10)
  , (Dot 10)
  ]

type alias Model =
  { dots : Array Dot
  }

initModel : Model
initModel =
  { dots = initDots
  }

-- ACTIONS
type Action
  = NoOp
  | GrowDot Int
  | ShrinkDot Int
  | Tick Time
  | Reset

-- UPDATE
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Tick delta ->
      let
        updatedDots =
          Array.map (\dot -> { dot | radius = dot.radius + (delta / 1000) }) model.dots

        updatedModel = { model | dots = updatedDots }
      in
        (updatedModel, Effects.none)
    GrowDot index ->
      let
        updatedRadius =
          case (Array.get index model.dots) of
            Just size ->
              size.radius + 10

            Nothing ->
              0 -- TODO: Make this an error

        updatedModel = { model | dots = (Array.set index (Dot updatedRadius) model.dots) }
      in
        (updatedModel, Effects.none)
    ShrinkDot index ->
      let
        updatedRadius =
          case (Array.get index model.dots) of
            Just size ->
              size.radius - 5

            Nothing ->
              0 -- TODO: Make this an error

        updatedModel = { model | dots = (Array.set index (Dot updatedRadius) model.dots) }
      in
        (updatedModel, Effects.none)
    Reset ->
      let
        updatedModel = { model | dots = initDots }
      in
        (updatedModel, Effects.none)
    NoOp ->
      (model, Effects.none)

-- VIEW
mapDotNode : Signal.Address Action -> Int -> Dot -> Html
mapDotNode address index dot =
  h1
    [ style
        [ ("width", (toString dot.radius ++ "px"))
        , ("height", (toString dot.radius ++ "px"))
        , ("background", "brown")
        , ("borderRadius", "100%")
        ]
    --, onMouseOver address (GrowDot index)
    --, onMouseLeave address (ShrinkDot index)
    ]
    []

view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    (Array.toList (Array.indexedMap (mapDotNode address) model.dots))

frameSignal : Signal Action
frameSignal =
  Signal.map (\delta -> (Tick delta)) AnimationFrame.frame

-- STARTUP
init : (Model, Effects Action)
init =
  (initModel, Effects.none)

app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [ frameSignal ]
    }

main : Signal.Signal Html
main =
  app.html

-- PORTS
port runner : Signal (Task.Task Never ())
port runner =
  app.tasks



