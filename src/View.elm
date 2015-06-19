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

single : Context -> Player -> Html
single c p =
  div
    ([ class ( if c.flipy then
                   "player" ++ toString p.id ++ "flip"
                 else
                   "player" ++ toString p.id )
     ])
    [ div
        [ classList [
            ("info-layer",     True),
            ("animate-openprime",  c.showOptions == Just True),
            ("animate-closeprime", c.showOptions == Just False)
          ]
        ]
        [ life c p
        , poison c p
        -- , history c p
        , name c p
        , a
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
        -- , buttons c p
        ]

    , div
        [ classList [
            ("background", True),
            ("blue",   p.color == Blue),
            ("purple", p.color == Purple),
            ("green",  p.color == Green),
            ("orange", p.color == Orange),
            ("yellow", p.color == Yellow)
          ]
        ]
        []

    , options c p
    ]

--options : Dict Id Player -> Html
--options ps =
--  div [] (List.map options' (Dict.toList ps))

options : Context -> Player -> Html
options c p =
  div
    [ classList [
        ("options-layer", True),
        ("animate-open",  c.showOptions == Just True),
        ("animate-close", c.showOptions == Just False),
        ("hide2",         c.showOptions == Nothing)
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
            [ class "buttons"
            ]
            [ colorButton p Blue
            , colorButton p Purple
            , colorButton p Green
            , colorButton p Orange
            , colorButton p Yellow
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
    [ class "incdamage" ]
    [ div [class "incdamage-icon"] []
    , div
        [ classList [
            ("incdamage-trigger", True),
            ("animation-flash",   c.flashDamageInc)
          ]
        , onClick updates.address (Inc p.id 1)
        ]
        []
    ]

decDamage : Context -> Player -> Html
decDamage c p =
  div
    [ class "decdamage" ]
    [ div [ class "decdamage-icon" ] []
    , div
        [ classList [
            ("decdamage-trigger", True),
            ("animation-flash",   c.flashDamageDec)
          ]
        , onClick updates.address (Inc p.id (-1))]
        []
    ]

incPoison : Context -> Player -> Html
incPoison c p =
  div
    [ class "incpoison" ]
    [ div [ class "incpoison-icon" ] []
    , div
        [ classList [
            ("incpoison-trigger", True),
            ("animation-flash",   c.flashPoisonInc)
          ]
        , onClick updates.address (Poison p.id 1)
        ]
        []
    ]

decPoison : Context -> Player -> Html
decPoison c p =
  div
    [ class "decpoison" ]
    [ div [ class "incpoison-icon" ] []
    , div
        [ classList [
            ("decpoison-trigger", True),
            ("animation-flash",   c.flashPoisonDec)
          ]
        , onClick updates.address (Poison p.id (-1))
        ]
        []
    ]

life : Context -> Player -> Html
life c p =
  div
    [ class "life"
    ]
    [ div
        [ classList [
            ("life-inner", True),
            ("lethal",     (p.life <= 0) || (p.poison >= 10))
          ]
        ]
        [ text (toString p.life)
        ]
   ]

poison : Context -> Player -> Html
poison c p =
  div
    [ class "poison"
    ]
    [ div
        [ classList [
            ("poison-inner", True),
            ("lethal",       (p.life <= 0) || (p.poison >= 10))
          ]
        ]
        [ text (toString p.poison)
        ]
    ]

name : Context -> Player -> Html
name c p =
  div [ class "name" ] [ div [ class "name-inner" ] [ text p.name ] ]

--history : Context -> Player -> Html
--history c p =
--  div [ class "history" ] ( List.map (\n -> div [] [text (toString n)]) (List.reverse hist) )

onChange a f =
  on "input" (Json.at ["target", "value"] Json.string) (\s -> Signal.message a (f s))

