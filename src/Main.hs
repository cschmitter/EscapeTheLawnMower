module Main where

import World
import Rendering
import Handling
import TransformWorld

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
myWindow = InWindow
           "Escape the Mower"            ---  Title
           (windowWidth, windowHeight)   ---  Size
           (250, 200)                    ---  Position

windowWidth :: Int
windowWidth = 640 * 3 `div` 2

windowHeight :: Int
windowHeight = 480 * 3 `div` 2

backgroundColor :: Color
backgroundColor = colorSky

fps :: Int
fps = 120
