module Main where

import           Graphics.Gloss.Interface.Environment (getScreenSize)
import           Graphics.Gloss.Interface.Pure.Game   (Color, Display (FullScreen), Event (EventKey), Key (..),
                                                       KeyState (..), Picture, SpecialKey (..), blue, color, green,
                                                       pictures, play, rectangleSolid, translate, white)
import           System.Random                        (Random, random, randomR)
import           System.Random.Stateful               (getStdGen)
import System.Random.Stateful (StdGen)

class Draw a where
  draw :: a -> Picture

data Area = Area Float Float

newAreaFromIntegrals :: (Int, Int) -> Area
newAreaFromIntegrals (width, height) = Area (fromIntegral width) (fromIntegral height)

newtype Size = Size Float

data Point = Point Float Float

instance Random Point where
  randomR (Point x1 y1, Point x2 y2) g =
    let
      (x', g') = randomR (x1, x2) g
      (y', g'') = randomR (y1, y2) g'
    in
      (Point x' y', g'')
  random g =
    let
      (x', g') = random g
      (y', g'') = random g'
    in
      (Point x' y', g'')

data Placeholder = Placeholder Point Size

distanceToLeft :: Placeholder -> Area -> Float
distanceToLeft (Placeholder (Point x _) (Size a)) (Area width _) =
  width / 2 + x - a / 2

distanceToDown :: Placeholder -> Area -> Float
distanceToDown (Placeholder (Point _ y) (Size a)) (Area _ height) =
  height / 2 + y - a / 2

distanceToUp :: Placeholder -> Area -> Float
distanceToUp (Placeholder (Point _ y) (Size a)) (Area _ height) =
  height / 2 - y - a / 2

distanceToRight :: Placeholder -> Area -> Float
distanceToRight (Placeholder (Point x _) (Size a)) (Area width _) =
  width / 2 - x - a / 2

instance Draw Placeholder where
  draw (Placeholder (Point x y) (Size a)) =
    translate x y $ rectangleSolid a a

newtype Speed = Speed Float
  deriving (Eq, Ord)

maxSpeed :: Placeholder -> Area -> Direction -> Speed -> Speed
maxSpeed placeholder area Direction'Left speed  = min speed $ Speed $ distanceToLeft placeholder area
maxSpeed placeholder area Direction'Down speed  = min speed $ Speed $ distanceToDown placeholder area
maxSpeed placeholder area Direction'Up speed    = min speed $ Speed $ distanceToUp placeholder area
maxSpeed placeholder area Direction'Right speed = min speed $ Speed $ distanceToRight placeholder area

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

initPlayer :: Player
initPlayer = Player (Placeholder (Point 0 0) (Size 20)) Stay

instance Draw Player where
  draw (Player placeholder _) =
    color blue $ draw placeholder

defaultPlayerSpeed :: Speed
defaultPlayerSpeed = Speed 10

movePlayer :: Area -> Player -> Player
movePlayer area (Player (Placeholder point size) (Move direction speed)) =
  let
    speed' = maxSpeed (Placeholder point size) area direction speed
    point' = movePoint direction speed' point
  in
    Player (Placeholder point' size) (Move direction speed)
movePlayer _ player = player

updatePlayerToMoveOrStay :: Key -> KeyState -> Player -> Player
updatePlayerToMoveOrStay key state (Player placeholder move) = Player placeholder $ moveOrStay key state move defaultPlayerSpeed

updatePlayer :: Event -> Player -> Player
updatePlayer (EventKey key state _ _) player = updatePlayerToMoveOrStay key state player
updatePlayer _ player                        = player

data Monster = Monster Placeholder

initMonster :: Monster
initMonster = Monster (Placeholder (Point 100 100) (Size 20))

spawnMonster :: Area -> StdGen -> (Monster, StdGen)
spawnMonster (Area width height) g =
  let
    (point, g') = randomR (Point (-width/2) (-height/2), Point (width/2) (height/2)) g
  in
    (Monster (Placeholder point (Size 20)), g')

instance Draw Monster where
  draw (Monster placeholder) =
    color green $ draw placeholder

data World = World Area Player Monster

instance Draw World where
  draw (World _ player monster) =
    pictures $ [ draw player, draw monster ]

background :: Color
background = white

frameRate :: Int
frameRate = 60

update :: Event -> World -> World
update  event (World area player monster) = World area (updatePlayer event player) monster

tick :: Float -> World -> World
tick _ (World area player monster) = World area (movePlayer area player) monster

main :: IO ()
main = do
  area <- newAreaFromIntegrals <$> getScreenSize
  g <- getStdGen
  let (monster, _) = spawnMonster area g
  let world = World area initPlayer monster
  play FullScreen background frameRate world draw update tick

