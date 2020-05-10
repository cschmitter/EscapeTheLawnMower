module TransformWorld where

import World

import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as S

----------------------------------------------
-- Update World
----------------------------------------------

updateWorld :: Float -> World -> World
updateWorld dt w = w { gameState = gameStateTransform w
                     , t = dt
                     , terrain = terrainTransform w
                     , player = playerTransform w
                     , baddies = baddiesTransform w }

----------------------------------------------
-- Game State Transform
----------------------------------------------

gameStateTransform :: World -> State
gameStateTransform w = case (gameState w) of
                         Menu -> Menu
                         
                         Running -> if (alive $ player w)
                                    then if (not $ won $ player w)
                                         then Running
                                         else GameOver Victory
                                    else GameOver Defeat
                                         
                         GameOver v -> GameOver v
                         
----------------------------------------------
-- Terrain Transform
----------------------------------------------

terrainTransform :: World -> Terrain
terrainTransform w = terrain w 

----------------------------------------------
-- Player Transform
----------------------------------------------

playerTransform :: World -> Player
playerTransform w = (player w) { playerBlock = playerBlockTransform w
                               -- , playerSpeed = playerSpeedTransform w
                               -- , playerJump = playerJumpTransform w
                               , playerVelocity = playerVelocityTransform w
                               -- , playerAcceleration = playerAccelerationTransform w
                               -- , playerSprite = playerSpriteTransform w
                               -- , alive = aliveTransform w
                               -- , won = wonTransform w
                               }
  
playerBlockTransform :: World -> Block
playerBlockTransform w = b { position = (x, y) }
  where b = playerBlock $ player w
        dt = t w
        (oldX, oldY) = position b
        (vX, vY) = playerVelocity $ player w
  
        offset d = headOrZero
                   $ filter (/= 0)
                   $ map (testCollision d)
                   $ playerTerrainCollisions w
        
        testCollision testd (reald, _offset)
          | testd == reald = _offset
          | otherwise = 0
          
        headOrZero xs
          | xs == [] = 0
          | otherwise = head xs
                     
        x = oldX + (vX * dt) - offsetLeft - offsetRight
        y = oldY + (vY * dt) - offsetDown - offsetUp
  
        offsetDown  = offset Downward
        offsetUp    = offset Upward
        offsetLeft  = offset Leftward
        offsetRight = offset Rightward

-- playerSpeedTransform :: World -> Float
-- playerSpeedTransform w = undefined

-- playerJumpTransform :: World -> Float
-- playerJumpTransform w = undefined

playerVelocityTransform :: World -> Velocity
playerVelocityTransform w = (vX, vY)
  where p = player w
        b = playerBlock p
        (oldvX, oldvY) = playerVelocity p
        (accelX, accelY) = playerAcceleration p
        dt = t w
        collisions = map fst $ playerTerrainCollisions w
        
        vX
          | goingLeft  && stopLeft  = 0
          | goingRight && stopRight = 0
          | otherwise = oldvX + (accelX * dt)
        vY
          | goingDown  && stopDown  = 0
          | goingUp    && stopUp    = 0
          | otherwise = oldvY + (accelY * dt)

        goingDown  = oldvY < 0
        goingUp    = oldvY > 0
        goingLeft  = oldvX < 0
        goingRight = oldvX > 0

        stopDown = elem Downward collisions
        stopUp = elem Upward collisions
        stopLeft = elem Leftward collisions
        stopRight = elem Rightward collisions
        
-- playerAccelerationTransform :: World -> Acceleration
-- playerAccelerationTransform w = undefined

-- playerSpriteTransform :: World -> PlayerSprite
-- playerSpriteTransform w = undefined

-- aliveTransform :: World -> Bool
-- aliveTransform w = undefined

-- wonTransform :: World -> Bool
-- wonTransform w = undefined

----------------------------------------------
-- Baddies Transform
----------------------------------------------

baddiesTransform :: World -> Baddies
baddiesTransform w = baddies w

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

isHeld :: [Key] -> World -> Bool
isHeld ks w = any (`S.member` held) ks
  where held = keys w

---------------------------------------------
-- Key Actions
---------------------------------------------

playerDo :: Action -> World -> World
playerDo action w = w { player = newPlayer }
  where p = player w
        (oldvX, oldvY) = playerVelocity p
        (oldaX, oldaY) = playerAcceleration p
        
        newPlayer
          | elem action [GoStop, GoLeft, GoRight, GoJump]
                             = p { playerVelocity = newVelocity }
          | action == GoFlip = p { playerJump = newJump
                                 , playerAcceleration = newAccel }
          | otherwise        = p
          
        newVelocity = case action of GoStop -> goStop
                                     GoLeft -> (oldvX - playerSpeed p, oldvY)
                                     GoRight -> (oldvX + (playerSpeed p), oldvY)
                                     GoJump -> if (elem Downward (map fst $ playerTerrainCollisions w) && oldaY < 0)
                                                  || (elem Upward (map fst $ playerTerrainCollisions w) && oldaY > 0)
                                               then (oldvX, playerJump p)
                                               else playerVelocity p
                                     _otherwise -> playerVelocity p

        newAccel = case action of GoFlip -> (oldaX, -oldaY)
                                  _otherwise -> playerAcceleration p

        newJump = case action of GoFlip -> -(playerJump p)
                                 _otherwise -> playerJump p

        goStop
          | isHeld keyGoRight w = playerVelocity $ player $ playerDo GoRight w
          | isHeld keyGoLeft w = playerVelocity $ player $ playerDo GoLeft w
          | otherwise = (0, oldvY)

