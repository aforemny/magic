import Skeleton exposing (..)

import Dict
import Keyboard
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (Task)
import Debug

main =
  skeleton view [initialPlayer 0, initialPlayer 1] android

view : Model -> Html
view (Model model) =
  div [] (List.map (\(_, player) -> single player) (Dict.toList model.players))

port android : Signal String

type alias Call =
  { tipe  : String
  , id    : String -- may be "", means all, cf. Blur
  , name  : String
  , color : String
  }

port focus : Signal (Maybe Call)
port focus =
  let
    f a s =
      case a of
        Open  i   -> Just { tipe = "focus", id = "input" ++ toString i, name = "", color = "" }
        Close i   -> Just { tipe = "blur",  id = "input" ++ toString i, name = "", color = "" }
        Blur      -> Just { tipe = "blur",  id = "",                    name = "", color = "" }
        Name  i s -> Just { tipe = "name",  id = toString i, name  = s, color = "" }
        Color i c -> Just { tipe = "color", id = toString i, name = "", color = toCss c }
        _         -> Nothing
  in
    Signal.foldp f Nothing
    <| Signal.merge (Signal.map (always Blur) Keyboard.enter)
    <| updates.signal

