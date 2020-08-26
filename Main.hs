module Main where

import           Data.Bifunctor                       (first)
import           Graphics.Gloss.Interface.Environment (getScreenSize)
import           Graphics.Gloss.Interface.Pure.Game   (Color, Display (FullScreen), Event (EventKey), Key (..),
                                                       KeyState (..), Picture, SpecialKey (..), blue, color, green,
                                                       pictures, play, rectangleSolid, red, translate, white)
import           System.Random                        (Random, random, randomR)
import           System.Random.Stateful               (StdGen, getStdGen)

class Draw a where
  draw :: a -> Picture

data Area = Area Float Float

newAreaFromIntegrals :: (Int, Int) -> Area
newAreaFromIntegrals (width, height) = Area (fromIntegral width) (fromIntegral height)

newtype Size = Size Float

data Point = Point Float Float
  deriving (Eq)

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

newtype Distance = Distance Float
  deriving (Eq, Ord)

distance :: Point -> Point -> Distance
distance (Point x1 y1) (Point x2 y2) =
  Distance $ sqrt $ (x1-x2)**2 + (y1-y2)**2

data Placeholder = Placeholder Point Size

placeholderAtRandomPoint :: Size -> Area -> StdGen -> (Placeholder, StdGen)
placeholderAtRandomPoint (Size size) (Area width height) g =
  first (\p -> Placeholder p (Size size)) $ randomR (Point (-width/2 + size/2) (-height/2 + size/2), Point (width/2 - size/2) (height/2 - size/2)) g

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

approximatePointTo :: Point -> Speed -> Point -> Point
approximatePointTo (Point x y) (Speed speed) (Point toX toY)
  | x > toX   = movePoint Direction'Left (maxSpeed' x toX) (Point x y)
  | y > toY   = movePoint Direction'Down (maxSpeed' y toY) (Point x y)
  | y < toY   = movePoint Direction'Up (maxSpeed' y toY) (Point x y)
  | x < toX   = movePoint Direction'Right (maxSpeed' x toX) (Point x y)
  | otherwise = Point x y
  where
    maxSpeed' a b
      | a > 0 && b < 0 = Speed $ min speed $ a + abs b
      | a < 0 && b > 0 = Speed $ min speed $ abs a + b
      | otherwise      = Speed $ min speed $ abs (a - b)

data MoveOrStay
  = Move Direction
  | Stay

moveOrStay :: Key -> KeyState -> MoveOrStay -> MoveOrStay
moveOrStay (SpecialKey KeyLeft) Down _                     = Move Direction'Left
moveOrStay (SpecialKey KeyLeft) Up (Move Direction'Left)   = Stay
moveOrStay (SpecialKey KeyDown) Down _                     = Move Direction'Down
moveOrStay (SpecialKey KeyDown) Up (Move Direction'Down)   = Stay
moveOrStay (SpecialKey KeyUp) Down _                       = Move Direction'Up
moveOrStay (SpecialKey KeyUp) Up (Move Direction'Up)       = Stay
moveOrStay (SpecialKey KeyRight) Down _                    = Move Direction'Right
moveOrStay (SpecialKey KeyRight) Up (Move Direction'Right) = Stay
moveOrStay _ _ move                                        = move

data Player = Player Placeholder MoveOrStay

initPlayer :: Player
initPlayer = Player (Placeholder (Point 0 0) (Size 20)) Stay

instance Draw Player where
  draw (Player placeholder _) =
    color blue $ draw placeholder

defaultPlayerSpeed :: Speed
defaultPlayerSpeed = Speed 10

movePlayer :: Area -> Player -> Player
movePlayer area (Player (Placeholder point size) (Move direction)) =
  let
    speed' = maxSpeed (Placeholder point size) area direction defaultPlayerSpeed
    point' = movePoint direction speed' point
  in
    Player (Placeholder point' size) (Move direction)
movePlayer _ player = player

updatePlayerToMoveOrStay :: Key -> KeyState -> Player -> Player
updatePlayerToMoveOrStay key state (Player placeholder move) = Player placeholder $ moveOrStay key state move

updatePlayer :: Event -> Player -> Player
updatePlayer (EventKey key state _ _) player = updatePlayerToMoveOrStay key state player
updatePlayer _ player                        = player

data WanderOrHunt
  = Wander Point
  | Hunt

data Monster = Monster Placeholder WanderOrHunt

defaultMonsterSpeed :: Speed
defaultMonsterSpeed = Speed 5

huntingDistance :: Distance
huntingDistance = Distance 256

spawnMonster :: Area -> StdGen -> (Monster, StdGen)
spawnMonster area g =
  let
    size = Size 20
    (placeholder, g') = placeholderAtRandomPoint size area g
    (Placeholder target _, g'') = placeholderAtRandomPoint size area g'
  in
    (Monster placeholder (Wander target), g'')

close :: Monster -> Player -> Bool
close (Monster (Placeholder point _) _) (Player (Placeholder target _) _) =
  distance point target <= huntingDistance

wander :: Monster -> Area -> StdGen -> (Monster, StdGen)
wander (Monster (Placeholder point size) (Wander target)) area g =
  let
    (target', g') = if point == target then first (\(Placeholder t _) -> t) $ placeholderAtRandomPoint size area g else (target, g)
    point' = approximatePointTo point defaultMonsterSpeed target
  in
    (Monster (Placeholder point' size) (Wander target'), g')
wander (Monster (Placeholder point size) _) area g = wander (Monster (Placeholder point size) (Wander point)) area g

hunt :: Monster -> Player -> Monster
hunt (Monster (Placeholder point size) Hunt) (Player (Placeholder target _) _) =
  let
    point' = approximatePointTo point defaultMonsterSpeed target
  in
    Monster (Placeholder point' size) Hunt
hunt (Monster placeholder _) player = hunt (Monster placeholder Hunt) player

moveMonster :: Monster -> Player -> Area -> StdGen -> (Monster, StdGen)
moveMonster monster player area g =
  if close monster player then (hunt monster player, g)
                          else wander monster area g

instance Draw Monster where
  draw (Monster placeholder (Wander _)) =
    color green $ draw placeholder
  draw (Monster placeholder Hunt) =
    color red $ draw placeholder

data World = World StdGen Area Player Monster

instance Draw World where
  draw (World _ _ player monster) =
    pictures $ [ draw player, draw monster ]

background :: Color
background = white

frameRate :: Int
frameRate = 60

update :: Event -> World -> World
update  event (World g area player monster) = World g area (updatePlayer event player) monster

tick :: Float -> World -> World
tick _ (World g area player monster) =
  let
    player' = movePlayer area player
    (monster', g') = moveMonster monster player' area g
  in
    World g' area player' monster'

main :: IO ()
main = do
  area <- newAreaFromIntegrals <$> getScreenSize
  g <- getStdGen
  let (monster, g') = spawnMonster area g
  let world = World g' area initPlayer monster
  play FullScreen background frameRate world draw update tick

