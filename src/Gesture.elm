module Gesture where

import Touch exposing (Touch,touches)
import Dict exposing (Dict)
import Debug

import Graphics.Element as Element

type alias Model =
  { touches  : Dict Int Touch
  , gestures : Dict Int Gesture
  }

def =
  { touches  = Dict.empty
  , gestures = Dict.empty
  }

toDict ts =
  Dict.fromList (List.map (\t -> (t.id, t)) ts)

type Gesture =
    Tap
  | Swipe { x:Int, y:Int }
  | Swiping { x:Int, y:Int }

model : Signal Model
model =
  Signal.foldp update def (Signal.map toDict touches)

update : Dict Int Touch -> Model -> Model
update t s =
  let
    g = Dict.diff s.touches t
  in
    { s | touches  <- t
        , gestures <-
            Dict.union (Dict.map fromJust (Dict.filter isJust (Dict.map ongoing  t)))
                       (Dict.map fromJust (Dict.filter isJust (Dict.map finished g)))
    }

ongoing : Int -> Touch -> Maybe Gesture
ongoing id {x,y,x0,y0,t0} = Nothing

--  if | (abs (x - x0) > abs (y - y0)) && (x - x0 >  50) -> Just <| Swiping { x = x-x0, y =  0 }
--     | (abs (x - x0) > abs (y - y0)) && (x - x0 < -50) -> Just <| Swiping { x = x-x0, y =  0 }
--     | (abs (y - y0) > abs (x - x0)) && (y - y0 >  50) -> Just <| Swiping { x =  0, y = y-y0 }
--     | (abs (y - y0) > abs (x - x0)) && (y - y0 < -50) -> Just <| Swiping { x =  0, y = y-y0 }
--     | otherwise                                       -> Nothing

finished : Int -> Touch -> Maybe Gesture
finished id {x,y,x0,y0,t0} =

  if | (abs (x - x0) > abs (y - y0)) && (x - x0 >  150) -> Just <| Swipe { x = -1, y =  0 }
     | (abs (x - x0) > abs (y - y0)) && (x - x0 < -150) -> Just <| Swipe { x =  1, y =  0 }
     | (abs (y - y0) > abs (x - x0)) && (y - y0 >  150) -> Just <| Swipe { x =  0, y = -1 }
     | (abs (y - y0) > abs (x - x0)) && (y - y0 < -150) -> Just <| Swipe { x =  0, y =  1 }
     | otherwise                                        -> Just Tap

fromJust : a -> Maybe x -> x
fromJust _ x =
  case x of
    Just v -> v
    _      -> Debug.crash "fromJust: Nothing"

isJust : a -> Maybe x -> Bool
isJust _ x =
  case x of
    Just v -> True
    _      -> False

main =
  Signal.map (Element.show << .gestures) model

gesture : Signal (Maybe Gesture)
gesture =
  Signal.map (Maybe.map snd << List.head << Dict.toList << .gestures) model

