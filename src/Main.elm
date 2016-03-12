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
import Graphics.Input exposing (hoverable)
import StartApp

-- MODEL
type alias Radius =
  Float

type alias Dot =
  { radius : Radius
  , isHovered : Bool
  }

new : Radius -> Bool -> Dot
new size h =
  { radius = size
  , isHovered = False
  } 

initDots : Array Dot
initDots =
  Array.fromList
  [ (Dot 10 False)
  , (Dot 10 False)
  , (Dot 10 False)
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
  --| GrowDot Int
  --| ShrinkDot Int
  | Tick Time
  | Hover (Bool, Int)

-- UPDATE
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Hover (switch, index) ->
      let
        updatedDot =
          case Array.get index model.dots of
            Just dot ->
              { dot | isHovered = switch }
            Nothing ->
              Dot 10 False
        updatedModel = { model | dots = Array.set index updatedDot model.dots }
      in
        (updatedModel, Effects.none)

    Tick delta ->
      let
        updater dot =
          if dot.isHovered then { dot | radius = dot.radius + (delta / 50) } else dot
          --if dot.isHovered then { dot | radius = dot.radius + (delta / 500) } else { dot | radius = dot.radius + (delta / 500) }
          --{ dot | radius = dot.radius + (delta / 500) }
        updatedDots =
          Array.map (\dot -> updater dot) model.dots
        updatedModel = { model | dots = updatedDots }
      in
        (updatedModel, Effects.none)

    --GrowDot index ->
    --  let
    --    updatedRadius =
    --      case (Array.get index model.dots) of
    --        Just size ->
    --          size.radius + 10
    --        Nothing ->
    --          0 -- TODO: Make this an error

    --    updatedModel = { model | dots = (Array.set index (Dot updatedRadius ) model.dots) }
    --  in
    --    (updatedModel, Effects.none)

    --ShrinkDot index ->
    --  let
    --    updatedRadius =
    --      case (Array.get index model.dots) of
    --        Just size ->
    --          size.radius - 5
    --        Nothing ->
    --          0 -- TODO: Make this an error

    --    updatedModel = { model | dots = (Array.set index (Dot updatedRadius) model.dots) }
    --  in
    --    (updatedModel, Effects.none)

    NoOp ->
      (model, Effects.none)

-- VIEW
mapDotNode : Signal.Address Action -> Int -> Dot -> Html
mapDotNode address index dot =
  toElement (index * 100) (index * 100) (h1
    [ style
        [ ("width", (toString dot.radius ++ "px"))
        , ("height", (toString dot.radius ++ "px"))
        , ("background", "brown")
        , ("borderRadius", "100%")
        ]
    ]
    [])
    |> hoverable (\h -> Signal.message hover.address (h, index))
    |> fromElement

view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    (Array.toList (Array.indexedMap (mapDotNode address) model.dots))

frameSignal : Signal Action
frameSignal =
  Signal.map (\delta -> (Tick delta)) AnimationFrame.frame

hover : Signal.Mailbox (Bool, Int)
hover =
  Signal.mailbox (False, 0)

hoverSignal : Signal Action
hoverSignal =
  Signal.map (\value -> Hover value) hover.signal


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
    , inputs = [ frameSignal, hoverSignal ]
    }

main : Signal.Signal Html
main =
  app.html

-- PORTS
port runner : Signal (Task.Task Never ())
port runner =
  app.tasks



