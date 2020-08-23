module Main where

import           Graphics.Gloss.Interface.Pure.Game (Color, Display (FullScreen), Event (EventKey), Key (..),
                                                     KeyState (..), Picture, SpecialKey (..), blue, color, pictures,
                                                     play, rectangleSolid, translate, white)

class Initialize a where
  initialize :: a

class Draw a where
  draw :: a -> Picture

newtype Size = Size Float

data Point = Point Float Float

data Placeholder = Placeholder Point Size

instance Draw Placeholder where
  draw (Placeholder (Point x y) (Size a)) =
    translate x y $ rectangleSolid a a

newtype Speed = Speed Float

defaultPlayerSpeed :: Speed
defaultPlayerSpeed = Speed 10

data Direction
  = Direction'Left
  | Direction'Down
  | Direction'Up
  | Direction'Right

movePoint :: Direction -> Speed -> Point -> Point
movePoint direction (Speed speed) (Point x y) =
  case direction of
    Direction'Left  -> Point (x - speed) y
    Direction'Down  -> Point x (y - speed)
    Direction'Up    -> Point x (y + speed)
    Direction'Right -> Point (x + speed) y

data MoveOrStay
  = Move Direction Speed
  | Stay

moveOrStay :: Key -> KeyState -> MoveOrStay -> Speed -> MoveOrStay
moveOrStay (SpecialKey KeyLeft) Down _ speed                   = Move Direction'Left speed
moveOrStay (SpecialKey KeyLeft) Up (Move Direction'Left _) _   = Stay
moveOrStay (SpecialKey KeyDown) Down _ speed                   = Move Direction'Down speed
moveOrStay (SpecialKey KeyDown) Up (Move Direction'Down _) _   = Stay
moveOrStay (SpecialKey KeyUp) Down _ speed                     = Move Direction'Up speed
moveOrStay (SpecialKey KeyUp) Up (Move Direction'Up _) _       = Stay
moveOrStay (SpecialKey KeyRight) Down _ speed                  = Move Direction'Right speed
moveOrStay (SpecialKey KeyRight) Up (Move Direction'Right _) _ = Stay
moveOrStay _ _ move _                                          = move

data Player = Player Placeholder MoveOrStay

instance Initialize Player where
  initialize = Player (Placeholder (Point 0 0) (Size 20)) Stay

instance Draw Player where
  draw (Player placeholder _) =
    color blue $ draw placeholder

movePlayer :: Player -> Player
movePlayer (Player (Placeholder point size) (Move direction speed)) =
  let
    point' = movePoint direction speed point
  in
    Player (Placeholder point' size) (Move direction speed)
movePlayer player = player

updatePlayerToMoveOrStay :: Key -> KeyState -> Player -> Player
updatePlayerToMoveOrStay key state (Player placeholder move) = Player placeholder $ moveOrStay key state move defaultPlayerSpeed

updatePlayer :: Event -> Player -> Player
updatePlayer (EventKey key state _ _) player = updatePlayerToMoveOrStay key state player
updatePlayer _ player                        = player

data World = World Player

instance Initialize World where
  initialize = World (initialize::Player)

instance Draw World where
  draw (World player) =
    pictures $ [ draw player ]

background :: Color
background = white

frameRate :: Int
frameRate = 60

update :: Event -> World -> World
update  event (World player) = World $ updatePlayer event player

tick :: Float -> World -> World
tick _ (World player) = World $ movePlayer player

main :: IO ()
main =
  play FullScreen background frameRate initialize draw update tick

