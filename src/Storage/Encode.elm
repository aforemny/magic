module Storage.Encode where

import Json.Encode exposing (..)
import Dict exposing (Dict)

import Model exposing (..)
import Theme exposing (toCss)

encode : Model -> Value
encode s =
  object
    [ ("mode",    s.mode    |> encodeMode )
    , ("match",   s.match   |> encodeMatch)
    , ("history", s.history |> encodeHistory)
    ]

encodeMode : Mode -> Value
encodeMode m =
  case m of
    Play      -> object [ ("tipe", string "play") ]
    History n -> object [ ("tipe", string "history"), ("n", int n) ]

encodeHistory h =
  list (List.map encodeMatch h)

encodeMatch m =
  object [
    ("players",  list (Dict.values (Dict.map encodePlayer  m.players))),
    ("contexts", list (Dict.values (Dict.map encodeContext m.contexts)))
  ]

encodePlayer : Id -> Player -> Value
encodePlayer _ p =
  object [
    ("id",      int p.id),
    ("life",    int p.life),
    ("poison",  int p.poison),
    ("name",    string p.name),
    ("color",   string (toCss p.color))
  ]

encodeContext : Id -> Context -> Value
encodeContext _ c =
  object [
    ("id", int c.id),
    ("flashDamageInc", bool c.flashDamageInc),
    ("flashDamageDec", bool c.flashDamageDec),
    ("flashPoisonInc", bool c.flashPoisonInc),
    ("flashPoisonDec", bool c.flashPoisonDec),
    ("flashSettings",  bool c.flashSettings),
    ("showOptions",    Maybe.withDefault null (Maybe.map bool c.showOptions))
  ]

