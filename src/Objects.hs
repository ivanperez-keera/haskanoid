{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Game objects and collisions.
module Objects where

import FRP.Yampa.VectorSpace

import Physics.TwoDimensions.Dimensions
import Physics.TwoDimensions.Collisions
import Physics.TwoDimensions.Physics
import Physics.Shapes.BasicAABBCollisions
import qualified Physics.TwoDimensions.PhysicalObjects     as P

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

isBlock :: Object -> Bool
isBlock o = case objectKind o of
  (Block {}) -> True
  _          -> False

isPaddle :: Object -> Bool
isPaddle o = case objectKind o of
  (Paddle _) -> True
  _          -> False

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
type Collision  = P.Collision  ObjectName
type Collisions = P.Collisions ObjectName
