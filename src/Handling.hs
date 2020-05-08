module Handling where

import World

import Graphics.Gloss.Interface.IO.Game

handleInput :: Event -> World -> World
handleInput (EventKey k Down _ _) w
  | elem k keyGoLeft = playerDo GoLeft w
  | elem k keyGoRight = playerDo GoRight w
  | elem k keyGoJump = playerDo GoJump w
  | elem k keyGoFlip = playerDo GoFlip w
  
handleInput (EventKey k Up _ _) w
  | elem k keyGoLeft = playerDo GoStop w
  | elem k keyGoRight = playerDo GoStop w

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
-- Key Actions
---------------------------------------------

playerDo :: Action -> World -> World
playerDo action w = w { player = newPlayer }
  where p = player w
        (oldvX, oldvY) = playerVelocity p
        (oldaX, oldaY) = playerAcceleration p
        
        newPlayer
          | elem action [ GoStop, GoLeft, GoRight, GoJump]
                             = p { playerVelocity = newVelocity }
          | action == GoFlip = p { playerJump = newJump
                                 , playerAcceleration = newAccel }
          | otherwise        = p
          
        newVelocity = case action of GoStop -> (0, oldvY)
                                     GoLeft -> (oldvX - playerSpeed p, oldvY)
                                     GoRight -> (oldvX + (playerSpeed p), oldvY)
                                     GoJump -> (oldvX, playerJump p)
                                     _otherwise -> playerVelocity p

        newAccel = case action of GoFlip -> (oldaX, -oldaY)
                                  _otherwise -> playerAcceleration p

        newJump = case action of GoFlip -> -(playerJump p)
                                 _otherwise -> playerJump p
