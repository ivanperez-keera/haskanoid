{-# LANGUAGE Arrows #-}
module Game.ObjectSF.Ball where

-- External imports
import Control.Applicative                  ((<$>))
import Data.List                            (find)
import Data.VectorSpace.Extra               (limitNorm)
import FRP.Yampa                            (Event (Event), SF, arr, edge,
                                             integral, lMerge, noEvent, returnA,
                                             switch, tag, (-->), (^+^), (^<<),
                                             (^>>))
import Physics.CollisionEngine              (changedVelocity, isInCollision,
                                             isInCollisionWith)
import Physics.TwoDimensions.Dimensions     (Pos2D, Vel2D)
import Physics.TwoDimensions.PhysicalObject (Collision (..))

-- Internal imports
import Game.Constants (ballHeight, ballWidth, initialBallVel, maxVNorm,
                       paddleWidth)
import Game.Objects   (Collisions, Object (..),
                       ObjectKind (Ball, Paddle, PowerUp, Side),
                       ObjectProperties (BallProps),
                       PowerUpKind (DestroyBallUp, LivesUp),
                       collisionObjectKind, isPaddle)
import Game.ObjectSF  (ObjectInput (..), ObjectOutput (ObjectOutput), ObjectSF,
                       livingObject, outputObject)
import UserInput      (Controller (controllerClick))


-- | Ball
--
-- A ball that follows the paddle until the user fires it
-- ('followPaddleDetectLaunch'), then switches ('switch') over to start
-- bounding around, until it hits the floor ('bounceAroundDetectMiss').
objBall :: ObjectSF
objBall = switch followPaddleDetectLaunch   $ \p ->
          switch (bounceAroundDetectMiss p) $ \_ ->
          objBall
    where
        -- Yampa's edge is used to turn the continuous
        -- signal produced by controllerClick into an
        -- event-carrying signal, only true the instant
        -- the mouse button is clicked.
        followPaddleDetectLaunch = proc oi -> do
            o     <- followPaddle -< oi
            click <- edge         -< controllerClick (userInput oi)
            returnA -< (o, click `tag` objectPos (outputObject o))

        bounceAroundDetectMiss p = proc oi -> do
            o                <- bouncingBall p initialBallVel -< oi
            miss             <- collisionWithBottom           -< collisions oi
            hitDestroyBallUp <- collisionDestroyBallUpPaddle  -< collisions oi

            let hbod = lMerge miss hitDestroyBallUp
            returnA -< (o, hbod)

-- | Position of the ball, starting from p0 with velicity v0, since the time of
-- last switching (that is, collision, or the beginning of time --being fired
-- from the paddle-- if never switched before), provided that no obstacles are
-- encountered.
freeBall :: Pos2D -> Vel2D -> ObjectSF
freeBall p0 v0 = proc (ObjectInput ci cs os) -> do

  -- Detect collisions
  let name = "ball"
  let kind = Ball
  let isHit = isInCollision (name, kind) cs

  -- Cap speed
  let v = limitNorm v0 maxVNorm

  -- Any free moving object behaves like this (but with
  -- acceleration. This should be in some FRP.NewtonianPhysics
  -- module)
  p <- (p0 ^+^) ^<< integral -< v

  let obj = Object { objectName           = name
                   , objectKind           = Ball
                   , objectProperties     = BallProps ballWidth
                   , objectPos            = p
                   , objectVel            = v0
                   , objectAcc            = (0, 0)
                   , objectDead           = False
                   , objectHit            = isHit
                   , canCauseCollisions   = True
                   , collisionEnergy      = 1
                   }

  returnA -< livingObject obj

-- | Ball follows the paddle if there is one, and it's out of the screen
-- otherwise). To avoid reacting to collisions, this ball is non-interactive.
followPaddle :: ObjectSF
followPaddle = arr $ \oi ->
  -- Calculate ball position, midway on top of the the paddle
  --
  -- This code allows for the paddle not to exist (Maybe), although that should
  -- never happen in practice.
  let mbPaddlePos = objectPos <$> find isPaddle (knownObjects oi)
      ballPos     = maybe (outOfScreen, outOfScreen)
                          ((paddleWidth/2, - ballHeight) ^+^)
                          mbPaddlePos
  in ObjectOutput (inertBallAt ballPos) noEvent noEvent
  where outOfScreen = -10
        inertBallAt p = Object { objectName           = "ball"
                               , objectKind           = Ball
                               , objectProperties     = BallProps ballWidth
                               , objectPos            = p
                               , objectVel            = (0, 0)
                               , objectAcc            = (0, 0)
                               , objectDead           = False
                               , objectHit            = False
                               , canCauseCollisions   = False
                               , collisionEnergy      = 0
                               }

-- | Fires an event when the ball *enters in* a collision with the
-- bottom wall.
--
-- NOTE: even if the overlap is not corrected, 'edge' makes
-- the event only take place once per collision.
collisionWithBottom :: SF Collisions (Event ())
collisionWithBottom = isInCollisionWith ("ball", Ball) ("bottomWall", Side) ^>> edge

-- | Fires an event when the ball *enters in* a collision with the
-- bottom wall.
--
-- NOTE: even if the overlap is not corrected, 'edge' makes
-- the event only take place once per collision.
collisionDestroyBallUpPaddle :: SF Collisions (Event ())
collisionDestroyBallUpPaddle = proc cs -> do

  -- Has the powerup been hit?
  let hits :: Collisions
      hits = filter (any (collisionObjectKind (PowerUp DestroyBallUp)) . collisionData) cs

      paddleHits :: Collisions
      paddleHits = filter (any (collisionObjectKind Paddle) . collisionData) hits

      isHit :: Bool
      isHit = not (null $ concatMap collisionData paddleHits)

  dead <- edge -< isHit
  returnA -< dead

-- | Fires an event when a PowerUp LivesUp *enters in* a collision with the
-- paddle.
--
-- NOTE: even if the overlap is not corrected, 'edge' makes
-- the event only take place once per collision.
collisionLivesUpsPaddle :: SF Collisions Int
collisionLivesUpsPaddle = proc cs -> do

  -- Has the powerup been hit?
  let hits :: Collisions
      hits = filter (any (collisionObjectKind (PowerUp LivesUp)) . collisionData) cs

      paddleHits :: Collisions
      paddleHits = filter (any (collisionObjectKind Paddle) . collisionData) hits

      isHit :: Bool
      isHit = not (null $ concatMap collisionData paddleHits)

  dead <- edge -< isHit
  returnA -< length (concatMap collisionData paddleHits)

-- A bouncing ball moves freely until there is a collision, then bounces and
-- goes on and on.
--
-- This SF needs an initial position and velocity. Every time
-- there is a bounce, it takes a snapshot of the point of
-- collision and corrected velocity, and starts again.
--
bouncingBall :: Pos2D -> Vel2D -> ObjectSF
bouncingBall p0 v0 =
  switch progressAndBounce (uncurry bouncingBall) -- Clearer: \(p', v') -> bouncingBall p' v'
 where

   -- Calculate the future tentative position, and bounce if necessary.
   --
   -- The SF freeball p0 v0 gives the position of the ball, starting from
   -- p0 with velicity v0, since the time of last switching (or being fired,
   -- whatever happened last) provided that no obstacles are encountered.
   --
   -- The ballBounce needs the ball SF' input and detects when it's time to
   -- bounce.

   progressAndBounce = proc oi -> do
     ou <- freeBall p0 v0 -< oi
     bb <- ballBounce     -< (oi, ou)
     returnA -< (ou, bb)

-- | Detect if the ball must bounce and, if so, take a snapshot of the object's
-- current position and velocity.
--
-- NOTE: To avoid infinite loops when switching, the initial input is discarded
-- and never causes a bounce. This works in this game and in this particular
-- case because the ball never-ever bounces immediately as fired from the
-- paddle.  This might not be true if a block is extremely close, if you add
-- flying enemies to the game, etc.
ballBounce :: SF (ObjectInput, ObjectOutput) (Event (Pos2D, Vel2D))
ballBounce = noEvent --> ballBounce'

-- | Detect if the ball must bounce and, if so, take a snapshot of the object's
-- current position and velocity.
--
-- This does the core of the work, and does not ignore the initial input.
--
-- It proceeds by detecting whether any collision affects
-- the ball's velocity, and outputs a snapshot of the object
-- position and the corrected velocity if necessary.
ballBounce' :: SF (ObjectInput, ObjectOutput) (Event (Pos2D, Vel2D))
ballBounce' = proc (ObjectInput ci cs os, o) -> do
  -- HN 2014-09-07: With the present strategy, need to be able to
  -- detect an event directly after
  -- ev <- edgeJust -< changedVelocity "ball" cs
  let ev = maybe noEvent Event (changedVelocity ("ball", Ball) cs)
  returnA -< fmap (\v -> (objectPos (outputObject o), v)) ev

