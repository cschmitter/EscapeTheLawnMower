module Handling where

import World

import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as S

handleInput :: Event -> World -> World
handleInput (EventKey k Down _ _) w = w {keys = S.insert k (keys w)}
handleInput (EventKey k Up _ _) w = w {keys = S.delete k (keys w)}
handleInput (EventMotion pos) w = w {mouse = pos}
handleInput _ w = w

---------------------------------------------
-- Key Definitions
---------------------------------------------

keyGoLeft :: [Key]
keyGoLeft = [Char 'h', Char 'a', SpecialKey KeyLeft]
  
keyGoRight :: [Key]
keyGoRight = [Char 'l', Char 'd', SpecialKey KeyRight]
  
keyGoJump :: [Key]
keyGoJump = [Char 'k', Char 'w', SpecialKey KeyUp, SpecialKey KeySpace]
  
keyGoFlip :: [Key]
keyGoFlip = [Char 'f']

---------------------------------------------
-- Key Functions
---------------------------------------------

isHeld :: [Key] -> World -> Bool
isHeld ks w = any (`S.member` held) ks
  where held = keys w
