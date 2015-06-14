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

main =
  Signal.map2 view model Window.dimensions

-- model

type Model = Model
  { players    : Dict Id Player
  , past       : Maybe Model
  }

initialModel : Model
initialModel = Model
  { players    = Dict.fromList [(0, initialPlayer 0), (1, initialPlayer 1)]
  , past       = Nothing
  }

type alias Player =
  { id         : Id
  , life       : Int
  , history    : List Int
  , poison     : Int
  , lastUpdate : Float
  , flipy      : Bool
  }

type alias Id = Int

initialPlayer : Id -> Player
initialPlayer i =
  { id         = i
  , life       = 20
  , history    = []
  , poison     = 0
  , lastUpdate = 0
  , flipy      = False
  }

modify : Id -> Model -> (Player -> Player) -> Model
modify i (Model model) f =
  case Dict.get i model.players of

    Just player -> Model { model | players <- Dict.insert i (f player) model.players }

    Nothing -> Debug.crash <| "modify: unknown player (" ++ toString i ++ ")"

-- view

view : Model -> (Int,Int) -> Html
view (Model model) (w,h) =
  case Dict.toList model.players of

    [(_, player1), (_, player2)] ->
      div [] [ view2 (w,h) player1 player2 ]

    _ -> Debug.crash "view: crash"

view2 : (Int,Int) -> Player -> Player -> Html
view2 (w,h) player1 player2 =
  div
    []
    [ single (w,h) player1
    , single (w,h) player2
    ]

single : (Int, Int) -> Player -> Html
single (w, h) player =
  div
    ([ class ( if player.flipy then
                   "player" ++ toString player.id ++ "flip"
                 else
                   "player" ++ toString player.id )
     ])
    [ div
        [ class "display" ]
        [ life (w,h) (player.life) (player.history)
        , poison player.poison
        , buttons player.id
        , poisonButtons player.id
        , div
            [ class "overlay" ]
            [ button [onClick updates.address (FlipY player.id)] [text "Flip"] ]
        ]
    , div
        [ class "buttons" ]
        [ undo, reset ]
    ]

undo : Html
undo =
  button
    [ class "undo"
    , onClick updates.address Undo ]
    [ text "Undo" ]

reset : Html
reset =
  button
    [ class "reset"
    , onClick updates.address Reset
    ]
    [ text "Reset" ]

buttons : Id -> Html
buttons i =
  div
    [ class "buttons-damage" ]
    [ button [class "damage-inc", onClick updates.address (Inc i   1)]  [ text "+1" ]
    , button [class "damage-dec", onClick updates.address (Inc i (-1))] [ text "-1" ]
    ]

poisonButtons : Id -> Html
poisonButtons i =
  div
    [ class "buttons-poison" ]
    [ button [class "poison-inc", onClick updates.address (Poison i   1)]  [ text "+1" ]
    , button [class "poison-dec", onClick updates.address (Poison i (-1))] [ text "-1" ]
    ]

life : (Int,Int) -> Int -> List Int -> Html
life (w,h) life lifeHistory =
  div
    [ class "life" ]
    [
      div
        [ class "history" ]
        ( List.map (\n -> div [] [text (toString n)]) (List.reverse (List.take 5 lifeHistory)) )

    , div
        [ class "current" ]
        [ text (toString life) ]

    ]

poison : Int -> Html
poison poison =
  case poison of
    0 -> div [class "poison"] [div [class "poison-current"] [text (toString poison)]]
    _ -> div [class "poison"] [div [class "poison-current"] [text (toString poison)]]

-- update

type Action =
    Noop
  | Inc Id Int
  | Poison Id Int
  | Undo
  | Reset
  | FlipY Id

model : Signal Model
model =
  Signal.foldp update initialModel input

input : Signal (Float, Action)
input = Signal.map (\(ms, a) -> (ms/1000.0, a)) (Time.timestamp updates.signal)

updates : Mailbox Action
updates =
  Signal.mailbox Noop

update : (Float, Action) -> Model -> Model
update (time, action) (Model model) =
  let
    merge player = time - player.lastUpdate < 2.0
  in
    case action of

      FlipY i ->
        modify i (Model model) <| \player ->
          { player | flipy <- not player.flipy }

      Undo -> case model.past of
                Nothing        -> Model model
                Just lastModel -> lastModel

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
              Nothing -> initialPlayer i

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
              Nothing -> initialPlayer i
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

