import Keyboard exposing (KeyCode, presses)
import Time exposing (Time, second, minute, hour)
import Round exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)

import Html exposing (Html, text)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Key exposing (..)

round : Int -> Float -> String
round = Round.round

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

collageSize : (Int, Int)
collageSize = (1600, 900)

boundY : Float
boundY = (toFloat (Tuple.second collageSize) / 2) - 160

boundX : Float
boundX = (toFloat (Tuple.first collageSize) / 2) - 120

type Movement = Left | Right | None
type MoveKey = Up | Down
  
type alias Model = 
  { velocity : Float
  , x : Float
  , y : Float
  , vy : Float  
  , movement : Movement
  , leftKey : Float
  , rightKey : Float
  }

init : (Model, Cmd Msg)
init = (Model 0 -500 0 0 None 0 0, Cmd.none)

-- UPDATE

applyPhysics : Model -> Time -> Model
applyPhysics model dt =
  let
    newPosition position velocity =
      position + velocity * dt

    newX = 
      newPosition model.x model.velocity
    
    newY =
      newPosition model.y model.vy

    getVelocity =
      let 
        speed = model.rightKey - model.leftKey
      in 
        if speed /= 0 then speed else model.velocity * 0.85
      -- case model.movement of
      --   Left -> -1
      --   Right -> 1
      --   None -> model.velocity

    testCollision newValue bound =
      if
        newValue > bound  || newValue < -(bound) then True else False

  in
    { model
    | x = if testCollision newX boundX then model.x else newX
    , y = if testCollision newY boundY then model.y else newY
    , vy = model.vy - (1 / dt)
    , velocity = getVelocity
    }
      

type Msg
  = Tick Time | KeyDown KeyCode | KeyUp KeyCode 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Tick deltaTime -> 
      (applyPhysics model deltaTime, Cmd.none)
    KeyDown keyCode ->
      ( keyDown keyCode model, Cmd.none )
    KeyUp keyCode ->
      ( keyUp keyCode model, Cmd.none )
      
keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
  case (fromCode keyCode) of
    ArrowRight ->
      updateVelocity Right Up model -- updateVelocity 0 model
    ArrowLeft ->
      updateVelocity Left Up model -- updateVelocity 0 model
    Space -> 
      model
    UpArrow ->
      model -- updateVertical 0 model
    DownArrow ->
      model -- updateVertical 0 model      
    _ -> 
      model

keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
  case (fromCode keyCode) of
    ArrowRight ->
      updateVelocity Right Down model
    ArrowLeft ->
      updateVelocity Left Down model
    Space -> 
      model
    UpArrow ->
      updateVertical 1 model
    DownArrow ->
      updateVertical -1 model
    _ ->
      model

updateVelocity : Movement -> MoveKey -> Model -> Model
updateVelocity direction moveKey model = 
  case direction of
    Right ->
      { model | rightKey = if moveKey == Up then 0 else 1 }
    Left ->
      { model | leftKey = if moveKey == Up then 0 else 1}
    None ->
      model
--{model | movement = direction}

updateVertical : Float -> Model -> Model
updateVertical newVertical model = {model | vy = newVertical}

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs Tick
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp 
    ]

-- VIEW

modelStats : Model -> String
modelStats model = 
  "Velocity:  " ++ 
  "  " ++ 
  round 2 model.velocity ++ 
  "  X:  " ++ round 2 model.x ++
  "  Y:  " ++ round 2 model.y  
  

figure : Model -> Element
figure model = 
  let
    baseX = 
      model.x
    baseY =
      model.y
    headSize =
      40
    torsoWidth =
      60
    torsoHeight =
      120
    legLength =
      120
    legWidth = 
      20
    armLength =
      80
    armWidth =
      16

    myMove (x, y) part =
      move (x + baseX, y + baseY) part
  
  in
    (collageSize |> uncurry collage)
       [ rect legWidth legLength
          |> filled (Color.green)
          |> myMove ((torsoWidth / 2), -torsoHeight + 25)
          |> rotate (degrees 15)
      , rect legWidth legLength
          |> filled (Color.blue)
          |> myMove ((-(torsoWidth / 2)), -torsoHeight + 25)
          |> rotate (degrees -15)
      
      ,  rect torsoWidth torsoHeight
          |> filled (Color.red)
          |> myMove (0, 0)

      , rect armWidth armLength
          |> filled (Color.blue)
          |> myMove (torsoWidth, torsoHeight / 2)
          |> rotate (degrees 110)
      , rect armWidth armLength
          |> filled (Color.green)
          |> myMove ((-(torsoWidth)), torsoHeight / 2)
          |> rotate (degrees -110)   

      , circle headSize
          |> filled (rgb 100 40 40)
          |> myMove (0, headSize * 2)      
      ]


view : Model -> Html Msg
view model = Html.div []
     [ figure model |> toHtml
     , Html.text <| modelStats model
     ]

