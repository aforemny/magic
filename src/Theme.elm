module Theme where

import Debug

type Color = Blue | Purple | Green | Orange | Yellow

default : Int -> Color
default i =
  case i of
    0 -> Blue
    1 -> Purple
    2 -> Green
    3 -> Orange
    4 -> Yellow

toCss : Color -> String
toCss c =
  case c of
    Blue   -> "blue"
    Purple -> "purple"
    Green  -> "green"
    Orange -> "orange"
    Yellow -> "yellow"

fromCss : String -> Color
fromCss s =
  case s of
    "blue"   -> Blue
    "purple" -> Purple
    "green"  -> Green
    "orange" -> Orange
    "yellow" -> Yellow
    _        -> Debug.crash <| "fromCss: unknown color (" ++ s ++ ")"

