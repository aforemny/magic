module State where

import Dict exposing (Dict)

type alias Id = Int

type alias State =
  { names  : List String
  , colors : List String
  }

defaultState : State
defaultState =
  { names  = [ "", "" ]           -- array!
  , colors = [ "blue", "purple" ] -- array!
  }

