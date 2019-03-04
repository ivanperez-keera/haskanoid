{-# LANGUAGE Arrows #-}
module Game.ObjectSF.PowerUp where

-- External imports
import FRP.Yampa                            (edge, integral, noEvent, returnA,
                                             (^+^), (^<<))
import Physics.TwoDimensions.Dimensions     (Pos2D, Size2D)
import Physics.TwoDimensions.PhysicalObject (Collision (..))

-- Internal imports
import Game.Objects  (Collisions, Object (..), ObjectKind (Paddle, PowerUp),
                      ObjectProperties (PowerUpProps), PowerUpKind,
                      collisionObjectKind, collisionObjectName)
import Game.ObjectSF (ObjectInput (ObjectInput), ObjectOutput (ObjectOutput),
                      ObjectSF)

powerUp :: String -> PowerUpKind -> Pos2D -> Size2D -> ObjectSF
powerUp idprefix puk (x,y) (w,h) = proc (ObjectInput ci cs os) -> do

  let name = idprefix ++ "powerup" ++ show (x,y)

  -- Has the powerup been hit?
  let hits :: Collisions
      hits = filter (any (collisionObjectName name). collisionData) cs

      paddleHits :: Collisions
      paddleHits = filter (any (collisionObjectKind Paddle) . collisionData) hits

      bottomHits :: Collisions
      bottomHits = filter (any (collisionObjectName "bottomWall") . collisionData) hits

      isHit :: Bool
      isHit = not (null $ concatMap collisionData paddleHits)
              || not (null $ concatMap collisionData bottomHits)

  -- Time to eliminate? (dead on collision with paddle)
  let isDead = isHit
  dead <- edge -< isDead

  -- Position (always falling)
  p <- (p0 ^+^) ^<< integral -< v

  returnA -< ObjectOutput
               Object { objectName           = name
                      , objectKind           = PowerUp puk
                      , objectProperties     = PowerUpProps (w, h)
                      , objectPos            = p
                      , objectVel            = v
                      , objectAcc            = (0,0)
                      , objectDead           = isDead
                      , objectHit            = isHit
                      , canCauseCollisions   = False
                      , collisionEnergy      = 0
                      }
               dead
               noEvent

  where p0 = (x', y')
        -- Calculate new position
        x' = x + (w / 2)
        y' = y + (h / 2)
        v  = (0, 100)

