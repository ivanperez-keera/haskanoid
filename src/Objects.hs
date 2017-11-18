{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Game objects and collisions.
module Objects where

import           FRP.Yampa.VectorSpace
import           Physics.Shapes.BasicAABBCollisions
import           Physics.TwoDimensions.Collisions
import           Physics.TwoDimensions.Dimensions
import           Physics.TwoDimensions.Physics
import qualified Physics.TwoDimensions.PhysicalObjects as P

import Constants

-- * Objects

-- | Object collection.
type Objects = [Object]

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
                     }
 deriving (Show)

-- | Type for object id.
type ObjectName = String

-- | The kind of object and any size properties.
--
-- TODO: Use a GADT to separate these properties in two types and guarantee a
-- proper correspondence in 'Object'.
data ObjectKind = Ball     Double -- radius?
                | Paddle   Size2D 
                | Block    BlockEnergy Size2D
                | Side     Side
                | PDiamond Size2D -- A diamond (powerup) with a given size
  deriving (Show,Eq)

-- | Block energy level: From minBlockEnergy - 1 to maxBlockEnergy. The former
--   means "dead".
type BlockEnergy = Int

-- ** Distinguish objects by kind.

isBlock :: Object -> Bool
isBlock o = case objectKind o of
  (Block {}) -> True
  _          -> False

isPaddle :: Object -> Bool
isPaddle o = case objectKind o of
  (Paddle _) -> True
  _          -> False

-- Partial function!
objectSize :: Object -> Size2D
objectSize object = case objectKind object of
  (Paddle sz)   -> sz
  (Block _ sz)  -> sz
  (Ball r)      -> let w = 2*r in (w, w)

-- Partial function. Object has size.
objectTopLevelCorner :: Object -> Pos2D
objectTopLevelCorner object = case objectKind object of
  (Paddle {}) -> objectPos object
  (Block  {}) -> objectPos object
  _other      -> objectPos object ^-^ (0.5 *^ (objectSize object))

-- * Physical properties

-- | Physical object definition of an 'Object'. We use AABB for shapes.
instance P.PhysicalObject Object String Shape where
  physObjectPos       = objectPos
  physObjectVel       = objectVel
  physObjectElas      = collisionEnergy
  physObjectShape     = objShape
  physObjectCollides  = canCauseCollisions
  physObjectId x      = objectName x
  physObjectUpdatePos = \o p -> o { objectPos = p }
  physObjectUpdateVel = \o v -> o { objectVel = v }
  physDetectCollision = detectCollision

-- | Collision shape of an object.
objShape :: Object -> Shape
objShape obj = case objectKind obj of
  Ball r          -> Rectangle (pos ^-^ (r,r)) (2*r, 2*r)
  Paddle sz       -> Rectangle pos sz
  Block _ sz      -> Rectangle pos sz
  PDiamond sz     -> Rectangle pos sz
  Side TopSide    -> Rectangle (pos ^-^ (e, e)) (gameW + 2*e, e)
  Side LeftSide   -> Rectangle (pos ^-^ (e, e)) (e,           gameH + 2*e)
  Side RightSide  -> Rectangle (pos ^-^ (0, e)) (e,           gameH + 2*e)
  Side BottomSide -> Rectangle (pos ^-^ (e, 0)) (gameW + 2*e, e)

 where pos = objectPos obj
       e   = collisionErrorMargin

       gameW = gameWidth
       gameH = gameHeight

-- ** Collisions
type Collision  = P.Collision  ObjectName
type Collisions = P.Collisions ObjectName
