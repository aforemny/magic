module Storage where

import Json.Encode exposing (Value)
import Result exposing (Result)

import Model exposing (Model)
import Storage.Decode as Decode
import Storage.Encode as Encode

encode : Model -> Value
encode = Encode.encode

decode : Value -> Result String Model
decode = Decode.decode

