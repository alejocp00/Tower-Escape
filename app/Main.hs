module Main (main) where

import GameConstants (windowHeight, windowWidth)
import Graphics.Gloss (Color, Display (InWindow), makeColor, play)
import Logic (initialGame, performGame, transformGame)
import Rendering (gameAsPicture)

window :: Display
window = InWindow "Tower Escape" (round windowWidth, round windowHeight) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 255 255 255 255

main :: IO ()
{-
Color                       BackgroundColor
Int                         FPS
world                       The initial world
(world -> Picture)          A function to convert the world a picture (Render)
(Event -> world -> world)   A function to handle input events
(Float -> world -> world)   A function to step the world one iteration. The passed ime is the time in seconds. Use it for things that happens each frame
-}
main = play window backgroundColor 60 initialGame gameAsPicture transformGame performGame