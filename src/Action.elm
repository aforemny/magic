module Action where

import Maybe exposing (Maybe)
import Model exposing (Id)
import Theme exposing (Color)

type Action =
    NoOp
  | Inc Id Int
  | Poison Id Int
  | Undo
  | Reset
  | FlipY Id
  | Android String
  | Clear (Maybe Id)
  | ScrollUp Id
  | ScrollDown Id
  | ScrollStop Id
  | Tick Float
  | Color Id Color
  | Close Id
  | Open Id
  | Name Id String
  | Blur

