-- | Level definition
--
-- This module includes the definition of the levels.
--
-- Each level has:
--
--   - Logical properties (defined in 'blockPosS').
--
--   - Multimedia properties (a background and audio file).
--
-- Together they form 'levels'.
module Levels where

import Control.Arrow((***))
import Physics.TwoDimensions.Dimensions

import Constants
import Resources

-- * Levels
-- ** Level specification
data LevelSpec = LevelSpec
 { blockPoss  :: [Pos2D]       -- ^ Block positions
 , levelBg    :: ImageResource -- ^ Background image
 , levelMusic :: MusicResource -- ^ Background music
 }

-- * Concrete levels
levels :: [LevelSpec]
levels = [ -- Level 0
           LevelSpec { blockPoss  = blockPosS 0
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level0.mp3"
                     }
         ,
           -- Level 1
           LevelSpec { blockPoss  = blockPosS 1
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 2
           LevelSpec { blockPoss  = blockPosS 2
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ]

-- | Level block specification (positions)

-- Level 0
--   %%%%%%%%
-- % XXXXXXXX
-- % XXXXXXXX
-- % XXXXXXXX
-- % XXXXXXXX
blockPosS :: Int -> [Pos2D]
blockPosS 0 = map (adjustHPos *** adjustVPos) allBlocks

 where allBlocks :: (Enum a, Num a) => [(a,a)]
       allBlocks = [(x,y) | x <- [0..blockColumns-1], y <- [0..blockRows-1]]

       adjustHPos :: Double -> Double
       adjustHPos = (leftMargin +) . ((blockWidth + blockSeparation) *)

       adjustVPos :: Double -> Double
       adjustVPos = (topMargin +) . ((blockHeight + blockSeparation) *)

       blockRows :: Num a => a
       blockRows = 4

       -- Fit as many as possible
       blockColumns :: Num a => a
       blockColumns =
         1 + round' ((gameWidth - blockWidth - 2 * leftMargin) / (blockWidth + blockSeparation))
        where round' = fromIntegral . floor

       leftMargin :: Num a => a
       leftMargin = 25

       topMargin :: Num a => a
       topMargin = 10
       

-- Level 1
--   %%%%%%%%
-- %    XXXX
-- %   XXXXX 
-- %  XXXXXX
-- % XXXXXX
-- % XXXXX
-- % XXXX
--
blockPosS 1 = map (adjustHPos *** adjustVPos) allBlocks
 where
   allBlocks :: (Enum a, Num a, Eq a, Ord a) => [(a,a)]
   allBlocks = [ (x,y) | x <- [0..7], y <- [0..5]
                       , (x + y > 2) && (x + y < 10)
               ]

   adjustHPos :: Double -> Double
   adjustHPos = (leftMargin +) . ((blockWidth + blockSeparation) *)

   adjustVPos :: Double -> Double
   adjustVPos = ((blockHeight + blockSeparation) *)

   leftMargin :: Num a => a
   leftMargin = 20

-- Level 2
--   %%%%%%
-- %  XXXXX
-- % X XXXX
-- % XX XXX
blockPosS 2  = map (adjustHPos *** adjustVPos) allBlocks
 where
   allBlocks :: (Enum a, Num a, Eq a) => [(a,a)]
   allBlocks = [(x,y) | x <- [0..5], y <- [0..2], x /= y]

   adjustHPos :: Double -> Double
   adjustHPos = (blockWidth *)

   adjustVPos :: Double -> Double
   adjustVPos = (100 +) . (blockHeight *)

blockPosS _ = error "No more levels"

-- * Testing constants
--
-- These constans are used by the game, and can be mofidied on order to test
-- different levels.
--
-- TODO: should this be moved to the module Constants?

-- | Initial level. Change this in the code to start
-- from a different level.
initialLevel :: Int
initialLevel = 0

-- | Number of levels. Change this in the code to finish
-- in a different level.
numLevels :: Int
numLevels = length levels
