module Update where

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
import Theme exposing (..)
import Action exposing (..)

updates : Mailbox Action
updates =
  Signal.mailbox NoOp

update : (Float, Action) -> Model -> Model
update (time, action) model =
  let
    merge player = time - player.lastUpdate < 2.0
  in
    case action of

      NoOp -> model

      Delete n ->
        { model | history <-
           List.take n model.history ++ List.drop (n+1) model.history
        }

      Blur -> model -- cf. focus port

      Name i s ->
        modify i model <| \c p ->
          { noModification | player <- Just { p | name <- s } }

      Color i x ->
        modify i model <| \c p ->
          { noModification | player <- Just { p | color <- x } }

      Close i ->
        modify i model <| \c p ->
          { noModification | context <- Just { c | showOptions <- Just False } }

      Open i ->
        modify i model <| \c p ->
          { noModification |
              context <- Just { c | showOptions <- Just True, flashSettings <- True }
          }

      Inc i n ->
        modify i model <| \c p ->
          { noModification |
              player  <- Just { p | life <- n + p.life },
              context <- Just { c | flashDamageInc <- (n >= 0)
                                  , flashDamageDec <- (n <  0) }
          }

      GoNext ->
        case model.mode of
          Play ->
            { model | match   <- reset model.match
                    , history <- if completed model.match then
                                     model.match :: model.history
                                   else
                                     model.history
            }
          History 0 ->
            { model | mode <- Play }
          History n ->
            { model | mode <- History (n-1) }

      GoPrev ->
        case model.mode of
          Play ->
            { model | mode <- History 0 }
          History n ->
            { model | mode <- if List.length model.history > n then
                                  History (n+1)
                                else
                                  History n
            }

--      FlipY i ->
--        modify i model <| \c p -> { p | flipy <- not p.flipy }
-- , history    <- (if merge player then [] else [player.life]) ++ player.history
-- , lastUpdate <- time

-- { model' | past <- if merge player then model''.past else Just (Model model) }

      Poison i n ->
        modify i model <| \c p ->
          { noModification |
              player  <- Just { p | poison <- Basics.max 0 (n + p.poison) },
              context <- Just { c | flashPoisonInc <- (n >= 0)
                                  , flashPoisonDec <- (n <  0) }
          }

--                     , lastUpdate <- time
--                     }
-- { model'' | past <- if merge player then model''.past else Just (Model model) }

      Clear Nothing -> model

      Clear (Just i) ->
        modify i model <| \c p ->
          { noModification |
              context <- Just { c | flashDamageInc <- False
                                  , flashDamageDec <- False
                                  , flashPoisonInc <- False
                                  , flashPoisonDec <- False
                                  , flashSettings  <- False }
          }

      _ -> model

--      Tick dt ->
--        let
--          m0 =
--            modify 0 (Model model) <| \p ->
--              { p | scroll <- Basics.max (-1968) <| Basics.min 3936 <|
--                              p.scroll + 100 * Maybe.withDefault 0 p.scrolling * dt
--              }
--          m1 = 
--            modify 1 m0 <| \p ->
--              { p | scroll <- Debug.log "tick" <| Basics.max (-1968) <| Basics.min 3936 <|
--                              p.scroll + 100 * Maybe.withDefault 0 p.scrolling * dt
--              }
--        in
--          m1

--      Reset ->
--        let
--          reset _ player =
--            { player | life    <- 20
--                     , history <- []
--                     , poison  <- 0
--                     }
--        in
--          Model { model | players <- Dict.map reset model.players }

--      Undo ->
--        case model.past of
--          Nothing        -> model
--          Just (Model lastModel) ->
--            let
--              keep i p =
--                case Dict.get i model.players of
--                  Just p' -> { p | flipy <- p'.flipy }
--                  Nothing -> p
--            in
--              { lastModel | players <- Dict.map keep lastModel.players }
--
--      ScrollUp i ->
--        modify i (Model model) <| \p -> { p | scrolling <- Just (-1) }
--
--      ScrollDown i ->
--        modify i (Model model) <| \p -> { p | scrolling <- Just 1 }
--
--      ScrollStop i ->
--        modify i (Model model) <| \p -> { p | scrolling <- Nothing }

