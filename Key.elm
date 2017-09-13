module Key exposing (..)

type Key
  = Space
  | ArrowLeft
  | ArrowRight
  | UpArrow
  | DownArrow
  | Unknown

fromCode : Int -> Key
fromCode keyCode =
  case keyCode of
    32 ->
      Space

    65 -> -- A
      ArrowLeft
    37 -> 
      ArrowLeft

    87 -> -- W
      UpArrow
    38 -> 
      UpArrow

    68 -> -- D
      ArrowRight
    39 -> 
      ArrowRight
    
    83 -> -- S
      DownArrow
    40 ->
      DownArrow

    _ ->
      Unknown