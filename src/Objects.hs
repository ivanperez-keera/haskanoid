{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Game objects and collisions.
module Objects where

import           FRP.Yampa.VectorSpace
import           Physics.TwoDimensions.Shapes.BasicAABB
import           Physics.TwoDimensions.Shapes.BasicAABBCollisions
import           Physics.TwoDimensions.Side
import           Physics.TwoDimensions.Dimensions
-- import           Physics.TwoDimensions.Physics
import qualified Physics.TwoDimensions.PhysicalObject as P

import Constants

-- * Objects

-- | Object collection.
type Objects = [Object]

-- | Objects have logical properties (ID, kind, dead, hit), shape properties
-- (kind), physical properties (kind, pos, vel, acc) and collision properties
-- (hit, 'canCauseCollisions', energy, displaced).
--
-- The properties need to agree with the kind. The kind is necessary to
-- avoid using string matching on the name to determine the object kind.
data Object = Object { objectName           :: ObjectName
                     , objectKind           :: ObjectKind
                     , objectProperties     :: ObjectProperties
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

-- | The kind of object.
data ObjectKind = Ball
                | Paddle
                | Block
                | Side
                | PowerUp PowerUpKind
  deriving (Show,Eq)

-- | Properties associated to each kind of object.
data ObjectProperties  = BallProps     Double -- radius?
                       | PaddleProps   Size2D
                       | BlockProps    BlockEnergy SignalPowerUp Size2D
                       | SideProps     Side
                       | PowerUpProps  Size2D
  deriving (Show,Eq)

-- | Block energy level: From minBlockEnergy - 1 to maxBlockEnergy. The former
--   means "dead".
type BlockEnergy = Int

-- | Indicates whether a block signals that it contains a powerup (True)
--   or not (False).
--
--   Note: The signal is independent from the actual creating of powerups.
type SignalPowerUp = Bool


-- | Indicates whether a powerup is created everytime if the ball hits
--   the block (True) or only when the block is "dead" (False).
type AlwaysPowerUp = Bool

-- | The kind of powerup:
--   PointsUp and LivesUp add points and lives, respectively.
--   MockUp does not add anything.
--   DestroyBallUp destroys the ball (and therefore takes one life).
data PowerUpKind = PointsUp | LivesUp | MockUp | DestroyBallUp
  deriving (Show,Eq)

-- ** Distinguish objects by kind.

isBlock :: Object -> Bool
isBlock o = case objectKind o of
  (Block) -> True
  _       -> False

isPaddle :: Object -> Bool
isPaddle o = case objectKind o of
  (Paddle) -> True
  _        -> False

isSide :: Object -> Bool
isSide o = case objectKind o of
  Side -> True
  _    -> False

-- Partial function!
objectSize :: Object -> Size2D
objectSize object = case objectProperties object of
  PaddleProps sz    -> sz
  BlockProps _ _ sz -> sz
  BallProps r       -> let w = 2*r in (w, w)
  PowerUpProps sz   -> sz

-- Partial function. Object has size.
objectTopLevelCorner :: Object -> Pos2D
objectTopLevelCorner object = case objectKind object of
  Paddle     -> objectPos object
  Block      -> objectPos object
  PowerUp {} -> objectPos object
  _other     -> objectPos object ^-^ (0.5 *^ objectSize object)

-- * Physical properties

-- | Physical object definition of an 'Object'. We use AABB for shapes.
instance P.PhysicalObject Object (String, ObjectKind) Shape where
  physObjectPosition       = objectPos
  physObjectVelocity       = objectVel
  physObjectElasticity     = collisionEnergy
  physObjectShape          = objShape
  physObjectCollides       = canCauseCollisions
  physObjectId x           = (objectName x, objectKind x)
  physObjectUpdatePosition = \o p -> o { objectPos = p }
  physObjectUpdateVelocity = \o v -> o { objectVel = v }
  physDetectCollision      = detectCollision

-- | Collision shape of an object.
objShape :: Object -> Shape
objShape obj = case objectProperties obj of
  BallProps r          -> Rectangle (pos ^-^ (r,r)) (2*r, 2*r)
  PaddleProps sz       -> Rectangle pos sz
  BlockProps _ _ sz    -> Rectangle pos sz
  PowerUpProps sz      -> Rectangle pos sz
  SideProps TopSide    -> Rectangle (pos ^-^ (e, e)) (gameW + 2*e, e)
  SideProps LeftSide   -> Rectangle (pos ^-^ (e, e)) (e,           gameH + 2*e)
  SideProps RightSide  -> Rectangle (pos ^-^ (0, e)) (e,           gameH + 2*e)
  SideProps BottomSide -> Rectangle (pos ^-^ (e, 0)) (gameW + 2*e, e)

 where pos = objectPos obj
       e   = collisionErrorMargin

       gameW = gameWidth
       gameH = gameHeight

-- ** Collisions
type Collision  = P.Collision  (ObjectName, ObjectKind)
type Collisions = P.Collisions (ObjectName, ObjectKind)

-- | Check if collision is with a given kind.
collisionObjectKind :: ObjectKind -> ((ObjectName, ObjectKind), Vel2D) -> Bool
collisionObjectKind ok1 ((_, ok2),_) = ok1 == ok2

-- | Check if collision is with a given id.
collisionObjectName :: ObjectName -> ((ObjectName, ObjectKind), Vel2D) -> Bool
collisionObjectName on1 ((on2, _),_) = on1 == on2
