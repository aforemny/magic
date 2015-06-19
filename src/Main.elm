module Main where

import Dict
import Keyboard
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (Task)
import Debug

import Storage exposing (..)
import View exposing (..)
import Model exposing (..)
import Json.Decode exposing (Value)
import Storage exposing (..)
import Time exposing (timestamp)
import Update exposing (..)
import Theme exposing (..)
import Action exposing (..)
import Result exposing (..)

main : Signal Html
main =
  Signal.map view model

view : Model -> Html
view s =
  div
    []
    ( List.map2 single
        (Dict.values s.match.contexts)
        (Dict.values s.match.players)
    )

port getStorage : Maybe Value

model : Signal Model
model =
  let

    input =
        Signal.map (\(ms,x) -> (ms/1000.0,x)) <| timestamp <| Signal.mergeMany
          [ updates.signal
          , Time.delay (90*Time.millisecond) (Signal.map clear updates.signal)
          -- , Signal.map (\dt -> Tick (dt/1000)) (Time.fps 24)
          ]

    clear action =
      case action of
        Inc    i _ -> Clear (Just i)
        Poison i _ -> Clear (Just i)
        _          -> Clear Nothing

    start =
      case getStorage of
        Just stored ->
          case decode stored of
            Ok  s -> s
            Err e -> initialModel -- Debug.crash e
        Nothing     -> initialModel

  in
    Signal.foldp update start input

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
        _         -> Nothing
  in
    Signal.foldp f Nothing
    <| Signal.merge (Signal.map (always Blur) Keyboard.enter)
    <| updates.signal

port setStorage : Signal (Maybe Value)
port setStorage =
  Signal.map (\s -> Just (encode s)) model

