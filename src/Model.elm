module Model where

import Dict exposing (Dict)
import Theme exposing (Color)

type alias Model =
  { mode      : Mode
  , match     : Match
  , history   : List Match
  }

model : Mode -> Match -> List Match -> Model
model mode match history =
  { mode = mode, match = match, history = history }

initialModel : Model
initialModel =
  { mode    = Play
  , match   = initialMatch
  , history = []
  }

type Mode =
    Play
  | History Int

type alias Match =
  { players  : Dict Id Player
  , contexts : Dict Id Context
  }

match : Dict Id Player -> Dict Id Context -> Match
match players contexts =
  { players = players, contexts = contexts }

initialMatch : Match
initialMatch =
  { players  = Dict.fromList [ (0, initialPlayer  0), (1, initialPlayer  1) ]
  , contexts = Dict.fromList [ (0, initialContext 0), (1, initialContext 1) ]
  }

reset : Match -> Match
reset m =
  { m | players  <- Dict.map (\i p -> { p | life <- 20, poison <- 0 }) m.players
      , contexts <- Dict.map (\i _ -> initialContext i)                m.players
  }

completed : Match -> Bool
completed m =
  Dict.foldl (\_ p b -> b || died p) False m.players

type alias Player =
  { id          : Id
  , life        : Int
  , poison      : Int
  , name        : String
  , color       : Color
  }

died p = (p.life <= 0) || (p.poison >= 10)

player id life poison name color =
  { id      = id
  , life    = life
  , poison  = poison
  , name    = name
  , color   = color
  }

initialPlayer : Id -> Player
initialPlayer i =
  { id         = i
  , life       = 20
  , poison     = 0
  , color      = Theme.default i
  , name       = ""
  }

type alias Id = Int

type alias Context =
  { id             : Id
  , flashDamageInc : Bool
  , flashDamageDec : Bool
  , flashPoisonInc : Bool
  , flashPoisonDec : Bool
  , flashSettings  : Bool
  , showOptions    : Maybe Bool
  }

initialContext : Id -> Context
initialContext i = 
  { id             = i
  , flashDamageInc = False
  , flashDamageDec = False
  , flashPoisonInc = False
  , flashPoisonDec = False
  , showOptions    = Nothing
  , flashSettings  = False
  }

type alias Modification
  = { player  : Maybe Player
    , context : Maybe Context }

noModification =
  { player  = Nothing
  , context = Nothing
  }

modify : Id -> Model -> (Context -> Player -> Modification) -> Model
modify i s f =
  let

    m  = s.match

    p = Dict.get i m.players  |> Maybe.withDefault (initialPlayer i)
    c = Dict.get i m.contexts |> Maybe.withDefault (initialContext i)

    x = f c p

    m' = { m | players  <- Dict.insert i (Maybe.withDefault p x.player)  m.players
             , contexts <- Dict.insert i (Maybe.withDefault c x.context) m.contexts
         }

  in

    { s | match <- m' }

