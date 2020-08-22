module Main where

import           Graphics.Gloss.Interface.Pure.Game (Color, Display (FullScreen), Event, Picture, blue, color, pictures,
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

data Player = Player Placeholder

instance Initialize Player where
  initialize = Player (Placeholder (Point 0 0) (Size 20))

instance Draw Player where
  draw (Player placeholder) =
    color blue $ draw placeholder

data World = World Player

instance Initialize World where
  initialize = World (initialize::Player)

instance Draw World where
  draw (World player) =
    pictures $ [ draw player ]

main :: IO ()
main =
  play FullScreen background frameRate initialize draw update tick

background :: Color
background = white

frameRate :: Int
frameRate = 60

update :: Event -> World -> World
update _ world = world

tick :: Float -> World -> World
tick _ world = world
