module Storage.Decode where

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Result exposing (Result)

import Theme exposing (fromCss)
import Model exposing (..)

decode : Value -> Result String Model
decode o = decodeValue decodeModel o

decodeModel : Decoder Model
decodeModel =
  object3 model ("mode" := decodeMode) ("match" := decodeMatch) ("history" := decodeHistory)

decodeMode : Decoder Mode
decodeMode =
  at ["tipe"] string `andThen` \tipe ->
  case tipe of
    "play"    -> succeed Play
    "history" -> object1 History ("n" := int)

decodeMatch : Decoder Match
decodeMatch =
  object2 match ("players" := decodePlayers) ("contexts" := decodeContexts)

decodePlayers : Decoder (Dict Id Player)
decodePlayers =
  map (\xs -> Dict.fromList (List.map idify xs)) (list decodePlayer)

decodeContexts : Decoder (Dict Id Context)
decodeContexts =
  map (\xs -> Dict.fromList (List.map idify xs)) (list decodeContext)

decodePlayer : Decoder Player
decodePlayer =
  object5 player
    ("id"      := int)
    ("life"    := int)
    ("poison"  := int)
    ("name"    := string)
    ("color"   := map fromCss string)

decodeContext : Decoder Context
decodeContext =
  at ["id"]              int          `andThen` \id ->
  at ["lastUpdate"]      float        `andThen` \lastUpdate ->
  at ["flipy"]           bool         `andThen` \flipy ->
  at ["flashDamageInc"]  bool         `andThen` \flashDamageInc ->
  at ["flashDamageDec"]  bool         `andThen` \flashDamageDec ->
  at ["flashPoisonInc"]  bool         `andThen` \flashPoisonInc ->
  at ["flashPoisonDec"]  bool         `andThen` \flashPoisonDec ->
  at ["flashSettings"]   bool         `andThen` \flashSettings ->
  at ["scroll"]          float        `andThen` \scroll ->
  at ["scrolling"]      (maybe float) `andThen` \scrolling ->
  at ["showOptions"]    (maybe bool)  `andThen` \showOptions ->
  succeed { id             = id
          , lastUpdate     = lastUpdate
          , flipy          = flipy
          , flashDamageInc = flashDamageInc
          , flashDamageDec = flashDamageDec
          , flashPoisonInc = flashPoisonInc
          , flashPoisonDec = flashPoisonDec
          , flashSettings  = flashSettings
          , scroll         = scroll
          , scrolling      = scrolling
          , showOptions    = showOptions
          }

idify x =
  (x.id, x)

decodeHistory : Decoder (List Match)
decodeHistory =
  list decodeMatch

