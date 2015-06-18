import Skeleton exposing (..)

import Dict
import Keyboard
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (Task)
import Debug
import State exposing (..)

main =
  skeleton view getStorage

view : Model -> Html
view (Model model) =
  div [] (List.map (\(_, player) -> single player) (Dict.toList model.players))

port getStorage : Maybe State

type alias Call =
  { tipe  : String
  , id    : String -- may be "", means all, cf. Blur
  , name  : String
  , color : String
  , num   : Int
  }

port focus : Signal (Maybe Call)
port focus =
  let
    f a s =
      case a of
        Open  i   -> Just { tipe = "focus", id = "input" ++ toString i, name = "", color = "", num = 0 }
        Close i   -> Just { tipe = "blur",  id = "input" ++ toString i, name = "", color = "", num = 0 }
        Blur      -> Just { tipe = "blur",  id = "",                    name = "", color = "", num = 0 }
        Name  i s -> Just { tipe = "name",  id = "", name  = s, color = "", num = i }
        Color i c -> Just { tipe = "color", id = "", name = "", color = toCss c, num = i }
        _         -> Nothing
  in
    Signal.foldp f Nothing
    <| Signal.merge (Signal.map (always Blur) Keyboard.enter)
    <| updates.signal

