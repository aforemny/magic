module Skeleton where

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

skeleton : (Model -> Html) -> List Player -> Signal String -> Signal Html
skeleton view players android =
  let
    idify player = (player.id, player)
    p = initialPlayer 0
    view' (Model model) =
      div
        []
        [ div [ class "noise" ] []
        , options model.players
--        , div
--            [ class "buttons" ]
--            [ undo
--            , reset
--            , twoplayer
--            , twoplayerprime
--            , threeplayer
--            , fourplayer
--            , fiveplayer
--            -- , flip (player.id == 0) player.id
--            , div [ class "clear" ] []
--            ]
        , view (Model model)
        ]
  in
    Signal.map view' (model (initialModel (Dict.fromList (List.map idify players))) android)

-- model

type Model = Model
  { players    : Dict Id Player
  , past       : Maybe Model
  }

initialModel : Dict Id Player -> Model
initialModel players = Model
  { players    = players
  , past       = Nothing
  }

type alias Player =
  { id         : Id
  , life       : Int
  , history    : List Int
  , poison     : Int
  , lastUpdate : Float
  , flipy      : Bool
  , color      : Color
  , flashDamageInc  : Bool
  , flashDamageDec : Bool
  , flashPoisonInc  : Bool
  , flashPoisonDec : Bool
  , name : String
  , scroll : Float
  , scrolling : Maybe Float
  , showOptions: Bool
  , flashSettings : Bool
  }

type alias Id = Int

type Color = Blue | Purple | Green | Orange | Yellow

initialPlayer : Id -> Player
initialPlayer i =
  { id         = i
  , life       = 20
  , history    = []
  , poison     = 0
  , lastUpdate = 0
  , flipy      = False
  , color      = defaultColor i
  , flashDamageInc = False
  , flashDamageDec = False
  , flashPoisonInc = False
  , flashPoisonDec = False
  , name = ""
  , scroll = -864
  , scrolling = Nothing
  , showOptions = False
  , flashSettings = False
  }

defaultColor i =
  case i of
    0 -> Blue
    1 -> Purple
    2 -> Green
    3 -> Orange
    4 -> Yellow

modify : Id -> Model -> (Player -> Player) -> Model
modify i (Model model) f =
  case Dict.get i model.players of

    Just player -> Model { model | players <- Dict.insert i (f player) model.players }

    Nothing -> Debug.crash <| "modify: unknown player (" ++ toString i ++ ")"

single : Player -> Html
single p =
  div
    ([ class ( if p.flipy then
                   "player" ++ toString p.id ++ "flip"
                 else
                   "player" ++ toString p.id )
     ])
    [ life p
    , poison p
    -- , history player.history
    , name p
    , div
        [ classList [
            ("settings",        True),
            ("animation-blink", p.flashSettings)
          ]
        , onClick updates.address (Toggle p.id)
        ]
        []

    , incDamage p
    , decDamage p
    , incPoison p
    , decPoison p
    , buttons   p

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

    ]

options : Dict Id Player -> Html
options ps =
  div [] (List.map options' (Dict.toList ps))

options' : (Id, Player) -> Html
options' (_,p) =
  div
    [ classList [
        ("animation-open",     p.showOptions),
        ("hide",           not p.showOptions)
      ]
    ]
    [ div [class "cancel", onClick updates.address (Close p.id)] []
    , div
        [ classList [
            ("options", True)
          ]
        ]
        [ div
          [ classList [
              ("input",  True),
              ("blue",   p.color == Blue),
              ("purple", p.color == Purple),
              ("green",  p.color == Green),
              ("orange", p.color == Orange),
              ("yellow", p.color == Yellow)
            ]
          ]
          [ input [type' "text", id "input", on "input" (Json.at ["target", "value"] Json.string) (\s -> Signal.message updates.address (Name p.id s))] [text "Hello"]
          ]
      , div
          [ class "button0"
          , onClick updates.address (Color p.id Blue)
          ]
          [div [class "blue"] []]
      , div
          [ class "button1"
          , onClick updates.address (Color p.id Purple)
          ]
          [ div [class "purple"] []
          ]
      , div
          [ class "button2"
          , onClick updates.address (Color p.id Green)
          ]
          [ div [class "green"] []
          ]
      , div
          [ class "button3"
          , onClick updates.address (Color p.id Orange)
          ]
          [ div [class "orange"] []
          ]
      , div
          [ class "button4"
          , onClick updates.address (Color p.id Yellow)
          ]
          [div [class "yellow"] []
          ]
      , div
          [ class "button5"
          , onClick updates.address (Close p.id)
          ]
          [div [class "close"] []
          ]
      ]
    ]

buttons : Player -> Html
buttons p =
  let
    button n =
      a
        [ class "button"
        , style [("transform", "translate(0," ++ toString p.scroll ++ "px)")]
        ]
        [ span [] [ text (toString n) ] ]
  in
    div
      [ class "buttons" ]
      ( [ div
           [ class "scrollup"
           , onMouseEnter updates.address (ScrollUp p.id)
           , onMouseLeave updates.address (ScrollStop p.id)
           ]
           []
        , div
           [ class "scrolldown"
           , onMouseEnter updates.address (ScrollDown p.id)
           , onMouseLeave updates.address (ScrollStop p.id)
           ]
           []
        ]
        ++ ( List.map button [-20 .. 20 ] )
      )

undo : Html
undo =
  div [ class "undo",  onClick updates.address Undo  ] []

twoplayer =
  div [ class "twoplayer", onClick layout.address (toUrl TwoPlayer) ] []

twoplayerprime =
  div [ class "twoplayerprime", onClick layout.address (toUrl TwoPlayerPrime) ] []

threeplayer =
  div [ class "threeplayer", onClick layout.address (toUrl ThreePlayer) ] []

fourplayer =
  div [ class "fourplayer", onClick layout.address (toUrl FourPlayer) ] []

fiveplayer =
  div [ class "fiveplayer", onClick layout.address (toUrl FivePlayer) ] []

reset : Html
reset =
  div [ class "reset", onClick updates.address Reset ] []

flip : Bool -> Id -> Html
flip pred i =
  if pred then
      div [ class "flip" , onClick updates.address (FlipY i) ] []
    else
      div [ class "flip" , style [ ("display", "none") ] ]      []

incDamage : Player -> Html
incDamage p =
  div
    [ class "incdamage" ]
    [ div [class "incdamage-icon"]                                             []
    , div
        [ classList [
            ("incdamage-trigger", True),
            ("animation-flash",   p.flashDamageInc)
          ]
        , onClick updates.address (Inc p.id 1)
        ]
        []
    ]

decDamage : Player -> Html
decDamage p =
  div
    [ class "decdamage" ]
    [ div [ class "decdamage-icon" ] []
    , div
        [ classList [
            ("decdamage-trigger", True),
            ("animation-flash",   p.flashDamageDec)
          ]
        , onClick updates.address (Inc p.id (-1))]
        []
    ]

incPoison : Player -> Html
incPoison p =
  div
    [ class "incpoison" ]
    [ div [ class "incpoison-icon" ] []
    , div
        [ classList [
            ("incpoison-trigger", True),
            ("animation-flash",   p.flashPoisonInc)
          ]
        , onClick updates.address (Poison p.id 1)
        ]
        []
    ]

decPoison : Player -> Html
decPoison p =
  div
    [ class "decpoison" ]
    [ div [ class "incpoison-icon" ] []
    , div
        [ classList [
            ("decpoison-trigger", True),
            ("animation-flash",   p.flashPoisonDec)
          ]
        , onClick updates.address (Poison p.id (-1))
        ]
        []
    ]

life : Player -> Html
life p =
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

poison : Player -> Html
poison p =
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

name : Player -> Html
name p =
  div [ class "name" ] [ div [ class "name-inner" ] [ text p.name ] ]

history : List Int -> Html
history hist =
  div [ class "history" ] ( List.map (\n -> div [] [text (toString n)]) (List.reverse hist) )

-- update

type Action =
    Noop
  | Inc Id Int
  | Poison Id Int
  | Undo
  | Reset
  | FlipY Id
  | Layout Layout
  | Android String
  | Clear (Maybe Id)
  | ScrollUp Id
  | ScrollDown Id
  | ScrollStop Id
  | Tick Float
  | Color Id Color
  | Close Id
  | Open Id
  | Toggle Id
  | Name Id String

type Layout = TwoPlayer | TwoPlayerPrime | ThreePlayer | FourPlayer | FivePlayer

toUrl : Layout -> Maybe String
toUrl layout =
  case layout of
    TwoPlayer      -> Just "./twoplayer.html"
    TwoPlayerPrime -> Just "./twoplayerprime.html"
    ThreePlayer    -> Just "./threeplayer.html"
    FourPlayer     -> Just "./fourplayer.html"
    FivePlayer     -> Just "./fiveplayer.html"

model : Model -> Signal String -> Signal Model
model start android =
  let

    input =
        Signal.map (\(ms,x) -> (ms/1000.0,x)) <| Time.timestamp <| Signal.mergeMany
          [ updates.signal
          , Time.delay (90*Time.millisecond) (Signal.map clear updates.signal)
          -- , Signal.map (\dt -> Tick (dt/1000)) (Time.fps 24)
          ]

    clear action =
      case action of
        Inc    i _ -> Clear (Just i)
        Poison i _ -> Clear (Just i)
        Toggle i   -> Clear (Just i)
        _          -> Clear Nothing
  in
    Signal.foldp update start input

updates : Mailbox Action
updates =
  Signal.mailbox Noop

layout : Mailbox (Maybe String)
layout =
  Signal.mailbox Nothing

update : (Float, Action) -> Model -> Model
update (time, action) (Model model) =
  let
    merge player = time - player.lastUpdate < 2.0
  in
    case action of

      FlipY i ->
        modify i (Model model) <| \player ->
          { player | flipy <- not player.flipy }

      Name i s ->
        modify i (Model model) <| \p -> { p | name <- s }

      Color i c ->
        modify i (Model model) <| \p -> { p | color <- c }

      Close i ->
        modify i (Model model) <| \p -> { p | showOptions <- False }

      Open i ->
        modify i (Model model) <| \p -> { p | showOptions <- True
                                            , flashSettings <- True }

      Toggle i ->
        modify i (Model model) <| \p -> { p | showOptions   <- not p.showOptions
                                            , flashSettings <- True }

      Undo ->
        case model.past of
          Nothing        -> Model model
          Just (Model lastModel) ->
            let
              keep i p =
                case Dict.get i model.players of
                  Just p' -> { p | flipy <- p'.flipy }
                  Nothing -> p
            in
              Model { lastModel | players <- Dict.map keep lastModel.players }

      Tick dt ->
        let
          m0 =
            modify 0 (Model model) <| \p ->
              { p | scroll <- Basics.max (-1968) <| Basics.min 3936 <|
                              p.scroll + 100 * Maybe.withDefault 0 p.scrolling * dt
              }
          m1 = 
            modify 1 m0 <| \p ->
              { p | scroll <- Debug.log "tick" <| Basics.max (-1968) <| Basics.min 3936 <|
                              p.scroll + 100 * Maybe.withDefault 0 p.scrolling * dt
              }
        in
          m1

      Clear Nothing  -> Model model

      Clear (Just i) ->
        modify i (Model model) <| \player ->
          { player | flashDamageInc <- False
                   , flashDamageDec <- False
                   , flashPoisonInc <- False
                   , flashPoisonDec <- False
                   , flashSettings  <- False
                   }

      ScrollUp i ->
        modify i (Model model) <| \p -> { p | scrolling <- Just (-1) }

      ScrollDown i ->
        modify i (Model model) <| \p -> { p | scrolling <- Just 1 }

      ScrollStop i ->
        modify i (Model model) <| \p -> { p | scrolling <- Nothing }

      Inc i n ->
        let
          model' = modify i (Model model) <| \player ->

              { player | life       <- n + player.life
                       , history    <- (if merge player then [] else [player.life]) ++ player.history
                       , lastUpdate <- time
                       , flashDamageInc  <- (n >= 0)
                       , flashDamageDec <- (n <  0)
              }

          player = 
            case Dict.get i model.players of
              Just player -> player
              Nothing -> Debug.crash <| "unknown player (" ++ toString i ++ ")"

        in
          case model' of
            Model model'' -> Model
              { model'' | past <- if merge player then model''.past else Just (Model model) }

      Poison i n ->
        let
          model' = modify i (Model model) <| \player ->

            { player | poison <- Basics.max 0 (n + player.poison)
                     , lastUpdate <- time
                     , flashPoisonInc <- (n >= 0)
                     , flashPoisonDec <- (n <  0)
                     }


          player = 
            case Dict.get i model.players of
              Just player -> player
              Nothing     -> Debug.crash <| "unknown player (" ++ toString i ++ ")"
        in
          case model' of
            Model model'' -> Model { model'' | past <- if merge player then model''.past else Just (Model model) }

      Reset ->
        let
          reset _ player =
            { player | life    <- 20
                     , history <- []
                     , poison  <- 0
                     }
        in
          Model { model | players <- Dict.map reset model.players }

      Noop -> Model model

