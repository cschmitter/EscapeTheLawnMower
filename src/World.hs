module World where

----------------------------------------------
-- World
----------------------------------------------
data World = World { gameState :: State
                   , terrain :: Terrain
                   , player :: Player
                   , baddies :: Baddies}

data State = Menu | Running | GameOver Victory

data Victory = Victory | Defeat
----------------------------------------------
-- Terrain
----------------------------------------------
type Terrain = [Block]

data Block = Block { blockPosition :: Position
                   , blockType :: BlockType }

data BlockType = Dirt
               | Spike Direction
               | Water Direction
               | Oil Direction

----------------------------------------------
-- Player
----------------------------------------------

data Player = Player { playerPosition :: Position
                     , playerVelocity :: Velocity 
                     , playerAcceleration :: Acceleration
                     , playerSprite :: PlayerSprite}

data PlayerSprite = Square

----------------------------------------------
-- Baddies
----------------------------------------------

type Baddies = [Baddie]

data Baddie = Baddie { baddiePosition :: Position
                     , baddieVelocity :: Velocity 
                     , baddieAcceleration :: Acceleration
                     , baddieSprite :: BaddieSprite}

data BaddieSprite = Gumba

----------------------------------------------
-- Common
----------------------------------------------
data Direction = Up | Down | Left | Right

type Position = (Float, Float)

data Velocity = Velocity Float Direction
  
data Acceleration = Acceleration Float Direction


initialWorld :: World
initialWorld = undefined

updateWorld :: Float -> World -> World
updateWorld _ = undefined
