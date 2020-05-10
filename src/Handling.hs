module Handling where

import World
import TransformWorld

import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as S

handleInput :: Event -> World -> World
handleInput (EventKey k Down _ _) w
  | elem k keyGoLeft = playerDo GoLeft w'
  | elem k keyGoRight = playerDo GoRight w'
  | elem k keyGoJump = playerDo GoJump w'
  | elem k keyGoFlip = playerDo GoFlip w'
  where w' = w {keys = S.insert k (keys w)}
  
handleInput (EventKey k Up _ _) w
  | elem k keyGoLeft = playerDo GoStop w'
  | elem k keyGoRight = playerDo GoStop w'
  where w' = w {keys = S.delete k (keys w)}

handleInput (EventMotion pos) w = w {mouse = pos}

handleInput _ w = w
