module View.History where

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Model exposing (..)
import Update exposing (..)
import Action exposing (..)

view : Int -> Maybe Match -> Html
view n m = case m of
  Just m ->
    div
      [ class "history-layer"
      ]
      [ div
          [ class "players"
          ]
          ( List.map single (Dict.values m.players)
          )
      , div
          [ id "match" ]
          [ text ("#" ++ toString n) ]
      , a
          [ id "delete"
          , onClick updates.address (Delete n)
          ]
          [ span [] [ text "X" ] ]
      ]

  Nothing ->
    div
      [ class "history-layer" ]
      [ div
          [ class "info" ]
          [ h1 [] [ text "You do not have any history." ]
          , h2 [] [ text "If a player died, reset the game to add it to your history." ]
          ]
      ]

single : Player -> Html
single p =
  div
    [ id ("player" ++ toString p.id)
    , classList [
        ("player", True),
        ("lethal", died p)
      ]
    ]
    [ h3 [] [ text p.name                 ]
    , h1 [] [ text (p.life   |> toString) ]
    , h2 [] [ text (p.poison |> toString) ]
    ]

