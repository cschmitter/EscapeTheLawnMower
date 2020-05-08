module World where

----------------------------------------------
-- World
----------------------------------------------

data World = World { gameState :: State
                   , t :: Float
                   , mouse :: Position
                   , terrain :: Terrain
                   , player :: Player
                   , baddies :: Baddies
                   } deriving (Show, Eq)

data State = Menu | Running | GameOver Victory deriving (Show, Eq)

data Victory = Victory | Defeat deriving (Show, Eq)

----------------------------------------------
-- Terrain
----------------------------------------------

type Terrain = [Block]

data Block = Block { position :: Position
                   , size :: Size
                   , blockType :: BlockType
                   } deriving (Show, Eq)

data BlockType = Actor
               | Dirt
               | Spike
               | Water Direction
               | Oil Direction deriving (Show, Eq)

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
                     , won :: Bool
                     } deriving (Show, Eq)

data PlayerSprite = Square deriving (Show, Eq)

data Action = GoStop
            | GoLeft
            | GoRight
            | GoJump
            | GoFlip
            deriving (Show, Eq)

----------------------------------------------
-- Baddies
----------------------------------------------

type Baddies = [Baddie]

data Baddie = Baddie { baddieBlock :: Block
                     , baddieVelocity :: Velocity 
                     , baddieAcceleration :: Acceleration
                     , baddieSprite :: BaddieSprite
                     } deriving (Show, Eq)

data BaddieSprite = Gumba deriving (Show, Eq)

----------------------------------------------
-- Common
----------------------------------------------

data Direction = Upward | Downward | Leftward | Rightward deriving (Show, Eq)

type Position = (Float, Float)

type Size = (Float, Float)

type Velocity = (Float, Float)
  
type Acceleration = (Float, Float)

getX :: (a, b) -> a
getX = fst

getY :: (a, b) -> b
getY = snd

addV :: (Num a) => (a, a) -> (a, a) -> (a, a)
addV (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

diffV :: (Num a) => (a, a) -> (a, a) -> (a, a)
diffV (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

dotProduct :: (Num a) => [(a, a)] -> a
dotProduct [] = 0
dotProduct vs = (product $ map getX vs) + (product $ map getY vs)

----------------------------------------------
-- Physics
----------------------------------------------

isCollision :: Block -> Block -> Bool
isCollision b1 b2 = if (collision b1 b2) /= Nothing then True else False

collision :: Block -> Block -> Maybe (Direction, Float)
collision b1 b2
  | not col = Nothing
  | smallUp = Just (Upward, upGap)
  | smallDown = Just (Downward, downGap)
  | smallLeft = Just (Leftward, leftGap)
  | smallRight = Just (Rightward, rightGap)
  | otherwise = Nothing
  where smallUp    = col && 5 > abs upGap
        smallDown  = col && 5 > abs downGap
        smallLeft  = col && 5 > abs leftGap
        smallRight = col && 5 > abs rightGap
        col = xOverlap && yOverlap

        upGap =    (up1    - down2 )
        downGap =  (down1  - up2   ) 
        leftGap =  (left1  - right2)  
        rightGap = (right1 - left2 )   

        xOverlap = left1 <= right2 && left2 <= right1
        yOverlap = down1 <= up2    && down2 <= up1
        
        (up1, up2)       = (y1 + h1, y2 + h2)
        (down1, down2)   = (y1 - h1, y2 - h2)
        (left1, left2)   = (x1 - w1, x2 - w2)
        (right1, right2) = (x1 + w1, x2 + w2)
        
        (x1, y1) = position b1
        (x2, y2) = position b2
        (w1, h1) = halve $ size b1
        (w2, h2) = halve $ size b2
        halve (a, b) = (a/2, b/2)

playerTerrainCollisions :: World -> [(Direction, Float)]
playerTerrainCollisions w = map (\(Just a) -> a)
                          $ filter (/= Nothing)
                          $ map (collision b) (terrain w)
  where b = playerBlock $ player w

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

----------------------------------------------
-- Initial World
----------------------------------------------

initialWorld :: World
initialWorld = World { gameState = Running
                     , t = 0
                     , mouse = (0,0)
                     , terrain = [ Block { position = (-100, -50)
                                         , size = (200, 20)
                                         , blockType = Dirt}
                                 , Block { position = (100, 100)
                                         , size = (300, 20)
                                         , blockType = Dirt}
                                 , Block { position = (0, -150)
                                         , size = (500, 20)
                                         , blockType = Dirt}
                                 ]
                     , player = Player { playerBlock = Block { position = (0, 50)
                                                             , size = (50, 50)
                                                             , blockType = Actor}
                                       , playerSpeed = 100
                                       , playerJump = 300
                                       , playerVelocity = (0, 0)
                                       , playerAcceleration = (0, -300)
                                       , playerSprite = Square
                                       , alive = True
                                       , won = False }
                     , baddies = [] } 
