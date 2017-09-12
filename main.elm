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

figure : Model -> Element
figure model = 
  let
    headSize =
      40
    torsoWidth =
      60
    torsoHeight =
      100
    legLength =
      80
    legWidth = 
      20
  
  in
    collage 400 400
      [ rect legWidth legLength
          |> filled (rgb 0 100 0)
          |> move (torsoWidth / 2, -torsoHeight + 25)
          |> rotate (degrees 15)
      , rect legWidth legLength
          |> filled (rgb 0 0 100)
          |> move (-(torsoWidth / 2), -torsoHeight + 25)
          |> rotate (degrees -15)
      ,  rect torsoWidth torsoHeight
          |> filled (rgb 100 0 0)       
      , circle headSize
          |> filled (rgb 0 0 0)
          |> move (0, headSize * 2)      
      ]


view : Model -> Html Msg
view model = figure model |> toHtml

