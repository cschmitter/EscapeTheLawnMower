module World where

import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S

----------------------------------------------
-- World
----------------------------------------------
data World = World { gameState :: State
                   , t :: Float
                   , keys :: S.Set Key
                   , mouse :: Position
                   , terrain :: Terrain
                   , player :: Player
                   , baddies :: Baddies}

data State = Menu | Running | GameOver Victory

data Victory = Victory | Defeat
----------------------------------------------
-- Terrain
----------------------------------------------
type Terrain = [Block]

data Block = Block { position :: Position
                   , size :: Size
                   , blockType :: BlockType }

data BlockType = Actor
               | Dirt
               | Spike
               | Water Direction
               | Oil Direction

----------------------------------------------
-- Player
----------------------------------------------

data Player = Player { playerBlock :: Block
                     , playerSpeed :: Float
                     , playerJump :: Float
                     , playerVelocity :: Velocity 
                     , playerAcceleration :: Acceleration
                     , playerSprite :: PlayerSprite
                     , alive :: Bool
                     , won :: Bool }

data PlayerSprite = Square

----------------------------------------------
-- Baddies
----------------------------------------------

type Baddies = [Baddie]

data Baddie = Baddie { baddieBlock :: Block
                     , baddieVelocity :: Velocity 
                     , baddieAcceleration :: Acceleration
                     , baddieSprite :: BaddieSprite}

data BaddieSprite = Gumba

----------------------------------------------
-- Common
----------------------------------------------

data Direction = Upward | Downward | Left | Right

type Position = (Float, Float)

type Size = (Float, Float)

type Velocity = (Float, Float)
  
type Acceleration = (Float, Float)

getX :: (a, b) -> a
getX = fst

getY :: (a, b) -> b
getY = snd

----------------------------------------------
-- Physics
----------------------------------------------

collision :: Block -> Block -> Bool
collision b1 b2 = xOverlap && yOverlap
  where xOverlap = x1 <= x2 + w2 && x2 <= x1 + w1
        yOverlap = y1 <= y2 + h2 && y2 <= y1 + h1
        (x1, y1) = position b1
        (x2, y2) = position b2
        (w1, h1) = size b1
        (w2, h2) = size b2

----------------------------------------------
-- Update World
----------------------------------------------

updateWorld :: Float -> World -> World
updateWorld dt w = w { gameState = gameStateTransform w
                     , t = dt
                     , keys = keysTransform w
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
-- Keys Transform
----------------------------------------------
keysTransform :: World -> S.Set Key
keysTransform w = S.difference (keys w) pressOnly
  where pressOnly = S.fromList [Char 'k', Char 'w', SpecialKey KeyUp, SpecialKey KeySpace]

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
                               , playerAcceleration = playerAccelerationTransform w }
                               -- , playerSprite = playerSpriteTransform w
                               -- , alive = aliveTransform w
                               -- , won = wonTransform w }
  
playerBlockTransform :: World -> Block
playerBlockTransform w = b { position = newPos }
                            -- , size = newSize }
  where b = playerBlock $ player w
        (vX, vY) = playerVelocity $ player w
        newPos = if any (collision b) (terrain w) then position b else changePos $ position b
        -- newSize = undefined
        changePos (x, y) = (x + (vX * t w), y + (vY * t w))
        -- changeSize = undefined

-- playerSpeedTransform :: World -> Float
-- playerSpeedTransform w = undefined

-- playerJumpTransform :: World -> Float
-- playerJumpTransform w = undefined

playerVelocityTransform :: World -> Velocity
playerVelocityTransform w = (vX, vY)
  where p = player w
        vX
          | goLeft && not goRight = -playerSpeed p
          | not goLeft && goRight = playerSpeed p
          | otherwise             = 0
        vY = jump + (getY $ playerAcceleration p)
        jump
          | goJump    = playerJump p
          | otherwise = 0
        goLeft  = any (`S.member` keys w) [Char 'h', Char 'a', SpecialKey KeyLeft]
        goRight = any (`S.member` keys w) [Char 'l', Char 'd', SpecialKey KeyRight]
        goJump  = any (`S.member` keys w) [Char 'k', Char 'w', SpecialKey KeyUp, SpecialKey KeySpace]

playerAccelerationTransform :: World -> Acceleration
playerAccelerationTransform w = (0, aY)
  where p = player w
        aY
          | switchGravity = -(getY $ playerAcceleration p)
          | otherwise     = getY $ playerAcceleration p
        switchGravity = any (`S.member` keys w) [Char 'f']

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

----------------------------------------------
-- Initial World
----------------------------------------------

initialWorld :: World
initialWorld = World { gameState = Running
                     , t = 0
                     , keys = S.empty
                     , mouse = (0,0)
                     , terrain = [Block { position = (-100, -50)
                                        , size = (200, 50)
                                        , blockType = Dirt}]
                     , player = Player { playerBlock = Block { position = (0, 50)
                                                             , size = (50, 50)
                                                             , blockType = Actor}
                                       , playerSpeed = 5
                                       , playerJump = 3
                                       , playerVelocity = (0, 0)
                                       , playerAcceleration = (0, -9.8)
                                       , playerSprite = Square
                                       , alive = True
                                       , won = False }
                     , baddies = [] } 
