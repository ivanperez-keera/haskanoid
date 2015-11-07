module Constants where

import FRP.Yampa
import Physics.TwoDimensions.Dimensions

width :: Double
width  = 640
height :: Double
height = 600

gameTop    :: Double
gameTop    = 100
gameLeft   :: Double
gameLeft   = 0

gameWidth :: Double
gameWidth = width
gameHeight :: Double
gameHeight = height - gameTop

loadingDelay :: DTime
loadingDelay = 2 -- seconds

paddleWidth, paddleHeight :: Double
paddleWidth  = 104
paddleHeight = 24
paddleMargin :: Double
paddleMargin = 50
ballWidth, ballHeight :: Double
ballWidth    = 10
ballHeight   = 10
ballMargin :: Double
ballMargin   = 30
blockWidth, blockHeight :: Double
blockWidth   = 64
blockHeight  = 32
blockSeparation :: Double
blockSeparation = 10

initialBallVel :: Pos2D
initialBallVel = (300, -300)

collisionErrorMargin :: Double
collisionErrorMargin = 100

stdLives :: Int
stdLives = 3

-- Energy transmission between objects in collisions
velTrans :: Double
velTrans = 0.2

-- Max speed
maxVNorm :: Double
maxVNorm = 300
      
-- Delays
-- restartDelay :: Time
-- restartDelay = 3
-- 
-- wonDelay :: Time
-- wonDelay = 3
