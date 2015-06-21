module View where

import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Signal exposing (Mailbox)
import Text
import Window
import Dict exposing (Dict)
import Time
import Json.Decode as Json

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Debug

import Model exposing (..)
import Update exposing (..)
import Theme exposing (..)
import Action exposing (..)
import View.Play as Play
import View.History as History

view : Model -> Html
view s =
  let
    m n =
      List.head (List.drop n s.history)

    main =
      case s.mode of
        Play      -> Play.view (Debug.log "play" s.match)
        History n -> History.view n (Debug.log "history" (m n))

    prev =
      let n = case s.mode of
              Play      -> 0
              History n -> n+1

      in History.view n (m n)

    next =
      case s.mode of
        Play -> Play.view initialMatch
        History 0 -> Play.view s.match
        History n -> History.view n (m (n-1))
  in
    div
      [ class "body"
      ]
      [ div [class "main"] [main]
      , div [class "prev"] [prev]
      , div [class "next"] [next]
      , div [class "buttons"]
          [ div
              [ id "go-prev"
              , onMouseEnter updates.address (Peek   "go-prev")
              , onMouseLeave updates.address (Unpeek "go-prev")
              , onClick      updates.address  GoPrev
              ]
              []
          , div
              [ id "go-next"
              , onMouseEnter updates.address (Peek   "go-next")
              , onMouseLeave updates.address (Unpeek "go-next")
              , onClick      updates.address  GoNext
              ]
              []
          ]
      ]

