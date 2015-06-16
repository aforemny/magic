import Skeleton exposing (..)

import Dict
import Html exposing (..)
import Debug

main =
  skeleton view [initialPlayer 0, initialPlayer 1] android

view : Model -> Html
view (Model model) =
  div [] (List.map (\(_, player) -> single player) (Dict.toList model.players))

port redirect : Signal (Maybe String)
port redirect =
  layout.signal

port android : Signal String

