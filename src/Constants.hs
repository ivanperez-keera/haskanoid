module Constants where

import FRP.Yampa
import Physics.TwoDimensions.Dimensions

-- * Main screen proportion
width :: Num a => a
width  = 640

height ::  Num a => a
height = 600

-- * Game area
gameTop    :: Num a => a
gameTop    = 100

gameLeft   :: Num a => a
gameLeft   = 0

gameWidth :: Num a => a
gameWidth = width

gameHeight :: Num a => a
gameHeight = height - gameTop

-- * Block area
gameAreaTopMargin :: Num a => a
gameAreaTopMargin = 10

gameAreaMinLeftMargin :: Num a => a
gameAreaMinLeftMargin = 25

-- * Time \/ Delays
loadingDelay :: DTime
loadingDelay = 2 -- seconds
      
-- restartDelay :: Time
-- restartDelay = 3
-- 
-- wonDelay :: Time
-- wonDelay = 3

-- * Object properties

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
pointsUpWidth, pointsUpHeight :: Double
pointsUpWidth = 30
pointsUpHeight = 24
livesUpWidth, livesUpHeight :: Double
livesUpWidth = 30
livesUpHeight = 27

-- * Lives

stdLives :: Int
stdLives = 3
maxBlockLife :: Int
maxBlockLife = 3
minBlockLife :: Int
minBlockLife = 1

-- * Physics

initialBallVel :: Pos2D
initialBallVel = (300, -300)

collisionErrorMargin :: Double
collisionErrorMargin = 100

-- | Energy transmission between objects in collisions
velTrans :: Double
velTrans = 0.2

-- | Max speed
maxVNorm :: Double
maxVNorm = 300

-- * Debugging

-- | Initial level. Change this in the code to start
-- from a different level.
initialLevel :: Int
initialLevel = 0
