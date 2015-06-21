module View.Play where

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

view m =
  div
    [ class "play-layer"
    ]
    [ div
        [ class "players"
        ]
        ( List.map2 single (Dict.values m.contexts) (Dict.values m.players)
        )
    ]

single : Context -> Player -> Html
single c p =
  div
    [ id ("player" ++ toString p.id)
    , classList [
        ("player", True),
        ("blue",   p.color == Blue),
        ("purple", p.color == Purple),
        ("green",  p.color == Green),
        ("orange", p.color == Orange),
        ("yellow", p.color == Yellow),
        ("blue",   True),
        ("animate-openprime",  c.showOptions == Just True),
        ("animate-closeprime", c.showOptions == Just False)
      ]
    ]
    [ options c p
    , h3 [] [ text  p.name                ]
    , h1 [] [ text (p.life   |> toString) ]
    , h2 [] [ text (p.poison |> toString) ]

    , div
        [ class "buttons"
        ]
        [ a
            [ classList [
                ("settings",        True),
                ("animation-blink", c.flashSettings)
              ]
            , if Just True == c.showOptions then
                  onClick updates.address (Close p.id)
                else
                  onClick updates.address (Open p.id)
            ]
            [ div [class "bar"] []
            , div [class "bar"] []
            , div [class "bar"] []
            ]

        , incDamage c p
        , decDamage c p
        , incPoison c p
        , decPoison c p
        ]
    ]

--options : Dict Id Player -> Html
--options ps =
--  div [] (List.map options' (Dict.toList ps))

options : Context -> Player -> Html
options c p =
  div
    [ classList [
        ("options-layer",       True),
        ("options-layer-open",  c.showOptions == Just True),
        ("options-layer-close", c.showOptions == Just False),
        ("hidetop",             c.showOptions == Nothing)
      ]
    ]
    [ div
        [ classList [
            ("options", True)
          ]
        ]
        [ div
            [ class "input"
            ]
            [ input
                [ type' "text"
                , id ("input" ++ toString p.id)
                , onChange updates.address (\s -> Name p.id s)
                , value p.name
                ]
                [
                ]
            ]
        , div
            [ class "colors"
            ]
            [ colorButton p Blue
            , colorButton p Purple
            , colorButton p Green
            , colorButton p Orange
            , colorButton p Yellow
            , div [ class "clear" ] []
            ]
        , a
            [ class "confirm"
            , onClick updates.address (Close p.id)
            ]
            [ span [] [ text "OK" ]
            ]
        ]
    ]

colorButton p c =
  a
    [ classList [
        ("button",  True),
        (toCss c,   True),
        ("checked", p.color == c)
      ]
    , onClick updates.address (Color p.id c)
    ]
    []

--buttons : Player -> Html
--buttons p =
--  let
--    button n =
--      a
--        [ class "button"
--        , style [("transform", "translate(0," ++ toString p.scroll ++ "px)")]
--        ]
--        [ span [] [ text (toString n) ] ]
--  in
--    div
--      [ class "buttons" ]
--      ( [ div
--           [ class "scrollup"
--           , onMouseEnter updates.address (ScrollUp p.id)
--           , onMouseLeave updates.address (ScrollStop p.id)
--           ]
--           []
--        , div
--           [ class "scrolldown"
--           , onMouseEnter updates.address (ScrollDown p.id)
--           , onMouseLeave updates.address (ScrollStop p.id)
--           ]
--           []
--        ]
--        ++ ( List.map button [-20 .. 20 ] )
--      )

undo : Html
undo =
  div [ class "undo",  onClick updates.address Undo  ] []

reset : Html
reset =
  div [ class "reset", onClick updates.address Reset ] []

flip : Bool -> Id -> Html
flip pred i =
  if pred then
      div [ class "flip" , onClick updates.address (FlipY i) ] []
    else
      div [ class "flip" , style [ ("display", "none") ] ]      []

incDamage : Context -> Player -> Html
incDamage c p =
  div
    [ classList [
        ("incdamage-trigger", True),
        ("animation-flash",   c.flashDamageInc)
      ]
    , onClick updates.address (Inc p.id 1)
    ]
    []

decDamage : Context -> Player -> Html
decDamage c p =
  div
    [ classList [
        ("decdamage-trigger", True),
        ("animation-flash",   c.flashDamageDec)
      ]
    , onClick updates.address (Inc p.id (-1))
    ]
    []

incPoison : Context -> Player -> Html
incPoison c p =
  div
    [ classList [
        ("incpoison-trigger", True),
        ("animation-flash",   c.flashPoisonInc)
      ]
    , onClick updates.address (Poison p.id 1)
    ]
    [
    ]

decPoison : Context -> Player -> Html
decPoison c p =
  div
    [ classList [
        ("decpoison-trigger", True),
        ("animation-flash",   c.flashPoisonDec)
      ]
    , onClick updates.address (Poison p.id (-1))
    ]
    [
    ]

onChange a f =
  on "input" (Json.at ["target", "value"] Json.string) (\s -> Signal.message a (f s))

