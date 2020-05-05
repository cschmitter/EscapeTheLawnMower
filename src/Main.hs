module Main where

import World
import Rendering
import Handling

import Graphics.Gloss

main :: IO ()
main = play
       myWindow          --- Main (move to Rendering?)
       backgroundColor   --- Main (move to Rendering?)
       fps               --- Main (move to Rendering?)
       initialWorld      --- World
       render            --- Rendering
       handleInput       --- Handling
       updateWorld       --- World

myWindow :: Display
myWindow = InWindow "Escape the Mower" (windowWidth, windowHeight) (250, 200)

windowWidth = 640 * 3 `div` 2
windowHeight = 480 * 3 `div` 2

backgroundColor :: Color
backgroundColor = makeColorI 46 52 64 255

fps :: Int
fps = 60
