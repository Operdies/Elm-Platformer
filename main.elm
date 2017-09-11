import Html exposing (Html)
import Svg
import Html.Events exposing (onClick)
import Svg.Attributes
import Keyboard exposing (KeyCode, presses)
import Time exposing (Time, second, minute, hour)

import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Arrows = {x: Int, y:Int}

type alias Model = 
  { stuff : String
  , arrows : Arrows
  , keysDown : String
--   , delta : Time
  }

init : (Model, Cmd Msg)
init = (Model "Hello" (Arrows 0 0) "", Cmd.none)

-- UPDATE

type Msg
  = Tick Time | Input KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Tick newTime -> 
      ({model | keysDown = toString presses}, Cmd.none)

    Input key ->
      (model, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

-- VIEW

view : Model -> Html Msg
view model = collage 300 300
               [ rect 50 50
                   |> filled (rgb 0 0 0)
               ] |> toHtml

