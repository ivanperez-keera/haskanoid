{-# LANGUAGE Arrows #-}
module Game.ObjectSF.Block where

-- External imports
import FRP.Yampa                        (Event, accumHoldBy, edge, localTime,
                                         noEvent, returnA, tag)
import Physics.CollisionEngine          (isInCollision)
import Physics.TwoDimensions.Dimensions (Pos2D, Size2D)

-- Internal imports
import Game.Constants (destroyBallUpHeight, destroyBallUpWidth, livesUpHeight,
                       livesUpWidth, mockUpHeight, mockUpWidth, pointsUpHeight,
                       pointsUpWidth)
import Game.Object    (AlwaysPowerUp, Object (..), ObjectKind (Block),
                       ObjectProperties (BlockProps), PowerUpKind (..),
                       SignalPowerUp)
import Game.ObjectSF  (ObjectInput (ObjectInput), ObjectOutput (ObjectOutput),
                       ObjectSF, PowerUpDef (PowerUpDef))

-- | Block SF generator. It uses the blocks's size, position and a number of
-- lives that the block has. The block's position is used for it's unique ID,
-- which means that two simulatenously existing blocks should never have the
-- same position. This is ok in this case because they are static, but would not
-- work if they could move and be created dynamically.
objBlock :: (Pos2D, Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp) -> Size2D -> ObjectSF
objBlock ((x,y), initlives, mpuk, spu) (w,h) = proc (ObjectInput ci cs os) -> do

  -- Detect collisions
  let name  = "blockat" ++ show (x,y)
      kind  = Block
      isHit = isInCollision (name, kind) cs
  hit   <- edge -< isHit

  -- Must be hit initlives times to disappear
  --
  -- If you want them to "recover" or self-heal with time,
  -- use the following code in place of lives.
  --
  -- recover <- delayEvent 5.0 -< hit
  -- lives <- accumHoldBy (+) 3 -< (hit `tag` (-1) `lMerge` recover `tag` 1)
  lives <- accumHoldBy (+) initlives -< (hit `tag` (-1))
  --
  -- let lives = 3 -- Always perfect

  -- Dead if out of lives.
  let isDead = lives <= 0
  dead <- edge -< isDead
  -- let isDead = False -- immortal blocks

  -- If it's hit or dead, does it create a 'powerup'?

  let pukWidthHeight :: PowerUpKind -> (Double, Double)
      pukWidthHeight PointsUp = (pointsUpWidth, pointsUpHeight)
      pukWidthHeight LivesUp = (livesUpWidth, livesUpHeight)
      pukWidthHeight MockUp = (mockUpWidth, mockUpHeight)
      pukWidthHeight DestroyBallUp = (destroyBallUpWidth, destroyBallUpHeight)

  -- create powerup if event happens (hit or dead)
  let createPowerUpF :: String -> PowerUpKind -> Event () -> Event PowerUpDef
      createPowerUpF idprefix puk event = event `tag` PowerUpDef idprefix puk (x,y) (pukWidthHeight puk)

  -- first event input is hit, second is dead
  let createPowerUpF' :: (String, Maybe (PowerUpKind, AlwaysPowerUp)) -> Event () -> Event () -> Event PowerUpDef
      createPowerUpF' (idprefix, Just (puk, True))  hit _    = createPowerUpF idprefix puk hit
      createPowerUpF' (idprefix, Just (puk, False)) _   dead = createPowerUpF idprefix puk dead
      createPowerUpF' (_, Nothing) _ _ = noEvent

  t <- localTime -< ()

  let createPowerUp = createPowerUpF' (show t, mpuk) hit dead

  returnA -< ObjectOutput
                Object{ objectName           = name
                      , objectKind           = Block
                      , objectProperties     = BlockProps lives spu (w, h)
                      , objectPos            = (x,y)
                      , objectVel            = (0,0)
                      , objectAcc            = (0,0)
                      , objectDead           = isDead
                      , objectHit            = isHit
                      , canCauseCollisions   = False
                      , collisionEnergy      = 0
                      }
               dead
               createPowerUp

