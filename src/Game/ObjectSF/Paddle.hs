{-# LANGUAGE Arrows #-}
module Game.ObjectSF.Paddle where

-- External imports
import Data.Ord.Extra                   (inRange)
import FRP.Yampa                        (integral, returnA, (*^), (^+^), (^-^),
                                         (^<<))
import Physics.CollisionEngine          (isInCollision)
import Physics.TwoDimensions.Dimensions (Pos2D)

-- Internal imports
import Game.Constants (gameHeight, gameWidth, paddleHeight, paddleMargin,
                       paddleWidth)
import Game.Objects   (Object (..), ObjectKind (Paddle),
                       ObjectProperties (PaddleProps))
import Game.ObjectSF  (ObjectInput (ObjectInput), ObjectSF, livingObject)
import UserInput      (Controller (controllerPos))

-- | The paddle tries to be in line with the mouse/pointer/controller.
--
-- It has drag, to make the game a bit harder. Take a look at the code if you
-- want to make it move faster or even instantaneously.
--
objPaddle :: ObjectSF
objPaddle = proc (ObjectInput ci cs os) -> do

  -- Detect collisions
  let name = "paddle"
  let kind = Paddle
  let isHit = isInCollision (name, kind) cs

  -- Try to get to the mouse position, but with a capped
  -- velocity.

  rec
      -- let v = limitNorm (20.0 *^ (refPosPaddle ci ^-^ p)) maxVNorm
      -- let p = refPosPaddle ci -- (initPosPaddle ^+^) ^<< integral -< v
      let v = 100.00 *^ (refPosPaddle ci ^-^ p)
      p <- (initPosPaddle ^+^) ^<< integral -< v
      -- let p = refPosPaddle ci

  --  Use this code if you want instantaneous movement,
  --  particularly cool with the Wiimote, but remember to cap
  --  the balls velocity or you will get incredibly high
  --  velocities when the paddle hits the ball.
  --
  --  let p = refPosPaddle ci
  --  v <- derivative -< p

  returnA -< livingObject
               Object { objectName           = name
                      , objectKind           = Paddle
                      , objectProperties     = PaddleProps (paddleWidth,paddleHeight)
                      , objectPos            = p
                      , objectVel            = (0,0)
                      , objectAcc            = (0,0)
                      , objectDead           = False
                      , objectHit            = isHit
                      , canCauseCollisions   = True
                      , collisionEnergy      = 0
                      }

-- | Follow the controller's horizontal position, keeping a constant
-- vertical position.
refPosPaddle :: Controller -> Pos2D
refPosPaddle c = (x', yPosPaddle)
    where
        (x, _) = controllerPos c
        x'     = inRange (0, gameWidth - paddleWidth) (x - (paddleWidth/2))

-- | The initial position of the paddle, horizontally centered.
initPosPaddle :: Pos2D
initPosPaddle = ((gameWidth - paddleWidth)/2, yPosPaddle)

-- | The paddle's vertical position, at a reasonable distance from the bottom.
yPosPaddle :: Double
yPosPaddle = gameHeight - paddleMargin
