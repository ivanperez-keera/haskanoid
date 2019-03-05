{-# LANGUAGE Arrows #-}
module Game.ObjectSF.Wall  where

-- External imports
import FRP.Yampa                        (noEvent, returnA)
import Physics.CollisionEngine          (isInCollision)
import Physics.TwoDimensions.Dimensions (Pos2D)
import Physics.TwoDimensions.Side       (Side (BottomSide, LeftSide, RightSide, TopSide))

-- Internal imports
import Game.Constants (gameHeight, gameWidth)
import Game.Objects   (Object (..), ObjectKind (Side), ObjectName,
                       ObjectProperties (SideProps))
import Game.ObjectSF  (ObjectInput (ObjectInput), ObjectOutput (ObjectOutput),
                       ObjectSF)

-- | Walls. Each wall has a side and a position.
--
-- NOTE: They are considered game objects instead of having special treatment.
-- The function that turns walls into 'Shape's for collision detection
-- determines how big they really are. In particular, this has implications in
-- ball-through-paper effects (ball going through objects, potentially never
-- coming back), which can be seen if the FPS suddently drops due to CPU load
-- (for instance, if a really major Garbage Collection kicks in.  One potential
-- optimisation is to trigger these with every SF iteration or every rendering,
-- to decrease the workload and thus the likelyhood of BTP effects.
objSideRight  :: ObjectSF
objSideRight  = objWall "rightWall"  RightSide  (gameWidth, 0)

-- | See 'objSideRight'.
objSideLeft   :: ObjectSF
objSideLeft   = objWall "leftWall"   LeftSide   (0, 0)

-- | See 'objSideRight'.
objSideTop    :: ObjectSF
objSideTop    = objWall "topWall"    TopSide    (0, 0)

-- | See 'objSideRight'.
objSideBottom :: ObjectSF
objSideBottom = objWall "bottomWall" BottomSide (0, gameHeight)

-- | Generic wall builder, given a name, a side and its base
-- position.
objWall :: ObjectName -> Side -> Pos2D -> ObjectSF
objWall name side pos = proc (ObjectInput ci cs os) -> do
   let isHit = isInCollision (name, Side) cs
   returnA -< ObjectOutput
                 Object { objectName           = name
                        , objectKind           = Side
                        , objectProperties     = SideProps side
                        , objectPos            = pos
                        , objectVel            = (0,0)
                        , objectAcc            = (0,0)
                        , objectDead           = False
                        , objectHit            = isHit
                        , canCauseCollisions   = False
                        , collisionEnergy      = 0
                        }
                noEvent
                noEvent
