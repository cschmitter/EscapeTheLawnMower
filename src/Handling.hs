module Handling where

import World
import qualified Data.Set as S

import Graphics.Gloss.Interface.IO.Game

handleInput :: Event -> World -> World
handleInput (EventKey k Down _ _) w = w {keys = S.insert k (keys w)}
handleInput (EventKey k Up _ _) w = w {keys = S.delete k (keys w)}
handleInput (EventMotion pos) w = w {mouse = pos}
handleInput _ w = w

