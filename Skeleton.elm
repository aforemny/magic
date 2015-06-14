module Skeleton where

import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Signal exposing (Mailbox)
import Text
import Window
import Dict exposing (Dict)
import Time

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Debug

skeleton : (Model -> Html) -> List Player -> Signal Html
skeleton view players =
  let
    idify player = (player.id, player)
    view' model =
      div
        []
        [ div [ class "noise" ] []
        , view model
        ]
  in
    Signal.map view' (model (initialModel (Dict.fromList (List.map idify players))))

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
  }

type alias Id = Int

type Color = Red | Blue | Purple | Green | Brown | Orange | Yellow

initialPlayer : Id -> Player
initialPlayer i =
  { id         = i
  , life       = 20
  , history    = []
  , poison     = 0
  , lastUpdate = 0
  , flipy      = False
  , color      = defaultColor i
  }

defaultColor i =
  case i of
    1 -> Blue
    2 -> Green
    3 -> Purple
    4 -> Yellow
    5 -> Brown
    6 -> Orange
    _ -> Red

modify : Id -> Model -> (Player -> Player) -> Model
modify i (Model model) f =
  case Dict.get i model.players of

    Just player -> Model { model | players <- Dict.insert i (f player) model.players }

    Nothing -> Debug.crash <| "modify: unknown player (" ++ toString i ++ ")"

single : Player -> Html
single player =
  div
    ([ class ( if player.flipy then
                   "player" ++ toString player.id ++ "flip"
                 else
                   "player" ++ toString player.id )
     ])
    [ life player.life
    , poison player.poison
    , history player.history

    , incDamage player.id
    , decDamage player.id
    , incPoison player.id
    , decPoison player.id

    , div
        [ class "buttons" ]
        [ undo
        , reset
        , twoplayer
        , twoplayerprime
        , threeplayer
        , fourplayer
        , fiveplayer
        , flip (player.id == 0) player.id
        , div [ class "clear" ] []
        ]

    , case player.color of
        Red    -> div [ class "red"    ] []
        Blue   -> div [ class "blue"   ] []
        Purple -> div [ class "purple" ] []
        Green  -> div [ class "green"  ] []
        Brown  -> div [ class "brown"  ] []
        Orange -> div [ class "orange" ] []
        Yellow -> div [ class "yellow" ] []
    ]

undo : Html
undo =
  div [ class "undo",  onClick updates.address Undo  ] []

twoplayer =
  div [ class "twoplayer", onClick layout.address (toUrl TwoPlayer) ] [ text "2" ]

twoplayerprime =
  div [ class "twoplayerprime", onClick layout.address (toUrl TwoPlayerPrime) ] [ text "2'"]

threeplayer =
  div [ class "threeplayer", onClick layout.address (toUrl ThreePlayer) ] [text "3"]

fourplayer =
  div [ class "fourplayer", onClick layout.address (toUrl FourPlayer) ] [text "4"]

fiveplayer =
  div [ class "fiveplayer", onClick layout.address (toUrl FivePlayer) ] [text "5"]

reset : Html
reset =
  div [ class "reset", onClick updates.address Reset ] []

flip : Bool -> Id -> Html
flip pred i =
  if pred then
      div [ class "flip" , onClick updates.address (FlipY i) ] []
    else
      div [ class "flip" , style [ ("display", "none") ] ]      []

incDamage : Id -> Html
incDamage i =
  div [class "incdamage", onClick updates.address (Inc i   1)]  []

decDamage : Id -> Html
decDamage i =
  div [class "decdamage", onClick updates.address (Inc i (-1))] []

incPoison : Id -> Html
incPoison i =
  div [class "incpoison", onClick updates.address (Poison i   1)]  []

decPoison : Id -> Html
decPoison i =
  div [class "decpoison", onClick updates.address (Poison i (-1))] []

life : Int -> Html
life life =
  div [ class "life" ] [ div [ class "life-inner" ] [ text (toString life) ] ]

poison : Int -> Html
poison poison =
  div [ class "poison" ] [ div [ class "poison-inner" ] [ text (toString poison) ] ]

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

type Layout = TwoPlayer | TwoPlayerPrime | ThreePlayer | FourPlayer | FivePlayer

toUrl : Layout -> Maybe String
toUrl layout =
  case layout of
    TwoPlayer      -> Just "./twoplayer.html"
    TwoPlayerPrime -> Just "./twoplayerprime.html"
    ThreePlayer    -> Just "./threeplayer.html"
    FourPlayer     -> Just "./fourplayer.html"
    FivePlayer     -> Just "./fiveplayer.html"

model : Model -> Signal Model
model start =
  Signal.foldp update start input

input : Signal (Float, Action)
input = Signal.map (\(ms, a) -> (ms/1000.0, a)) (Time.timestamp updates.signal)

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

      Inc i n ->
        let
          model' = modify i (Model model) <| \player ->

              { player | life       <- n + player.life
                       , history    <- (if merge player then [] else [player.life]) ++ player.history
                       , lastUpdate <- time
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

            { player | poison <- n + player.poison
                     , lastUpdate <- time }


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

