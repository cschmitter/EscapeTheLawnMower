module TransformWorld where

import World
import Handling

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
playerTransform w = (player w') { playerBlock = playerBlockTransform w'
                               -- , playerSpeed = playerSpeedTransform w
                               -- , playerJump = playerJumpTransform w
                               , playerVelocity = playerVelocityTransform w'
                               -- , playerAcceleration = playerAccelerationTransform w
                               -- , playerSprite = playerSpriteTransform w
                               -- , alive = aliveTransform w
                               -- , won = wonTransform w
                               }
  where w' = doAll process as w
        as = [ (keyGoLeft,  GoLeft)
             , (keyGoRight, GoRight)
             , (keyGoJump,  GoJump) 
             , (keyGoFlip,  GoFlip)
             ]
        process (ks, action) world
          | isHeld ks world = playerDo action world
          | otherwise = world
        doAll _ [] world = world
        doAll f (x:xs) world = doAll f xs (f x world)
  
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
          | goingLeft && stopLeft   = 0
          | goingRight && stopRight = 0
          | goingDown  && stopDown  = 0
          | goingUp    && stopUp    = 0
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
-- Key Actions
---------------------------------------------

playerDo :: Action -> World -> World
playerDo action w = w { player = newPlayer }
  where p = player w
        (oldvX, oldvY) = playerVelocity p
        (oldaX, oldaY) = playerAcceleration p
        
        newPlayer
          | elem action [GoLeft, GoRight, GoJump, StopLeft, StopRight]
                             = p { playerVelocity = newVelocity }
          | action == GoFlip = p { playerJump = newJump
                                 , playerAcceleration = newAccel }
          | otherwise        = p
          
        newVelocity = case action of GoLeft -> (-(playerSpeed p), oldvY)
                                     GoRight -> (playerSpeed p, oldvY)
                                     GoJump -> goJump
                                     StopLeft -> stopLeft
                                     StopRight -> stopRight
                                     _otherwise -> playerVelocity p

        newAccel = case action of GoFlip -> (oldaX, -oldaY)
                                  _otherwise -> playerAcceleration p

        newJump = case action of GoFlip -> -(playerJump p)
                                 _otherwise -> playerJump p

        goJump = if (elem Downward (map fst $ playerTerrainCollisions w) && oldaY < 0)
                    || (elem Upward (map fst $ playerTerrainCollisions w) && oldaY > 0)
                 then (oldvX, playerJump p)
                 else playerVelocity p

        stopLeft
          | isHeld keyGoRight w = playerVelocity $ player $ playerDo GoRight w
          | otherwise = (0, oldvY)
        
        stopRight
          | isHeld keyGoLeft w = playerVelocity $ player $ playerDo GoLeft w
          | otherwise = (0, oldvY)

