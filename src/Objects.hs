-- | Game objects and collisions.
module Objects where

import FRP.Yampa.VectorSpace

import Data.Extra.Num
import Physics.TwoDimensions.Dimensions
import Physics.TwoDimensions.Collisions
import Physics.TwoDimensions.Physics

import Constants

-- * Objects

type ObjectName = String

-- | Objects have logical properties (ID, kind, dead, hit), shape properties
-- (kind), physical properties (kind, pos, vel, acc) and collision properties
-- (hit, 'canCauseCollisions', energy, displaced).
data Object = Object { objectName           :: ObjectName
                     , objectKind           :: ObjectKind
                     , objectPos            :: Pos2D
                     , objectVel            :: Vel2D
                     , objectAcc            :: Acc2D
                     , objectDead           :: Bool
                     , objectHit            :: Bool
                     , canCauseCollisions   :: Bool
                     , collisionEnergy      :: Double
                     , displacedOnCollision :: Bool       -- Theoretically, setting cE == 0 should suffice
                     }
 deriving (Show)

type Objects   = [Object]

-- | The kind of object and any size properties.
--
-- TODO: Use a GADT to separate these properties in two types and guarantee a
-- proper correspondence in 'Object'.
data ObjectKind = Ball    Double -- radius?
                | Paddle  Size2D 
                | Block   Energy Size2D
                | Side    Side
  deriving (Show,Eq)

type Energy = Int

isBall :: ObjectKind -> Bool
isBall (Ball _) = True
isBall _        = False

isBlock :: ObjectKind -> Bool
isBlock (Block {}) = True
isBlock _          = False

isPaddle :: Object -> Bool
isPaddle o = case objectKind o of
  (Paddle _) -> True
  _          -> False

objShape :: Object -> Shape
objShape obj = case objectKind obj of
  (Ball r)    -> Rectangle (p ^-^ (r,r)) (2*r, 2*r)
  (Paddle s)  -> Rectangle p s
  (Block _ s) -> Rectangle p s
  (Side   s)  -> sideToShape p s
 where p = objectPos obj
       width'  = gameWidth
       height' = gameHeight
       d = collisionErrorMargin
       sideToShape p TopSide    = Rectangle (p ^-^ (d, d)) (width' + 2*d, d)
       sideToShape p LeftSide   = Rectangle (p ^-^ (d, d)) (d, height' + 2*d)
       sideToShape p RightSide  = Rectangle (p ^-^ (0, d)) (d, height' + 2*d)
       sideToShape p BottomSide = Rectangle (p ^-^ (d, 0)) (width' + 2*d, d)

-- * Collisions
type Collisions = [Collision]

-- | A collision is a list of objects that collided, plus their velocities as
-- modified by the collision.
-- 
-- Take into account that the same object could take part in several
-- simultaneous collitions, so these velocities should be added (per object).
data Collision = Collision
  { collisionData :: [(ObjectName, Vel2D)] } -- ObjectId x Velocity
 deriving Show

-- | Detects a collision between one object and another regardless of
-- everything else
--
-- FIXME: should we use the last known positions? Or should velocities suffice?
detectCollision :: Object -> Object -> Maybe Collision
detectCollision obj1 obj2
  | overlap obj1 obj2 = Just (collisionResponseObj obj1 obj2)
  | otherwise         = Nothing

overlap obj1 obj2 = overlapShape (objShape obj1) (objShape obj2)

collisionSide :: Object -> Object -> Side
collisionSide obj1 obj2 = shapeCollisionSide (objShape obj1) (objShape obj2)

collisionResponseObj o1 o2 =
  Collision $
    map objectToCollision [(o1, side, o2), (o2, side', o1)]
  where side  = collisionSide o1 o2
        side' = oppositeSide side
        objectReacts      o             = collisionEnergy o > 0 || displacedOnCollision o
        objectToCollision (o,s,o')      = (objectName o, correctVel (objectVel o ^+^ (velTrans *^ objectVel o')) (collisionEnergy o) s)
        correctVel (vx,vy) e TopSide    = (vx, ensurePos (vy * (-e)))
        correctVel (vx,vy) e BottomSide = (vx, ensureNeg (vy * (-e)))
        correctVel (vx,vy) e LeftSide   = (ensureNeg (vx * (-e)),vy)
        correctVel (vx,vy) e RightSide  = (ensurePos (vx * (-e)),vy)
