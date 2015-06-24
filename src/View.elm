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
        Play      -> Play.view s.match
        History n -> History.view n (m n)

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
      [ div
          [ classList [
              ("main",       True),
              ("a-goprev",   s.go == Just True),
              ("a-gonext",   s.go == Just False && s.lastMode /= Play),
              ("a-peekprev", s.peek == Just True),
              ("a-peeknext", s.peek == Just False)
            ]
          ]
          [ main
          ]
      , div
          [ classList [
              ("buttons",  True),
              ("a-goprev", s.go == Just True),
              ("a-gonext", s.go == Just False && s.mode /= Play)
            ]
          ]
          [ div
              [ id "go-prev"
              , onMouseEnter updates.address (Peek   "go-prev")
              , onMouseLeave updates.address (Unpeek "go-prev")
              , onClick      updates.address  GoPrev
              , classList [
                  ("hide", lastHistory s.mode s.history)
                ]
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
      , div
          [ classList [
              ("prev",     True),
              ("a-gonextprime",  (s.go == Just False) && s.lastMode /= Play),
              ("a-peekprevprime", s.peek == Just True)
            ]
          ]
          [ prev
          ]
      , div
          [ classList [
              ("next",     True),
              ("a-goprevprime",  (s.go == Just True)  && s.mode /= Play),
              ("a-goresetprime", (s.go == Just False) && s.lastMode == Play),
              ("a-peeknextprime", s.peek == Just False)
            ]
          ]
          [ next
          ]
      ]

swipeOffset n =
  style [("transform", "translateX(" ++ toString 0 ++ "px)")]

lastHistory mode history =
  case mode of
    History n -> n == List.length history
    _         -> False

inHistory mode =
  case mode of
    History 0 -> True
    _         -> False

