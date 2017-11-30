-- | Level definition
--
-- This module includes the definition of the levels.
--
-- Each level has:
--
--   - Logical properties (defined in 'blockDescS').
--
--   - Multimedia properties (a background and audio file).
--
-- Together they form 'levels'.
module Levels where

import Control.Arrow                    ((***))
import Data.List                        (nub,zip4)
import Game.AssetManager
import Physics.TwoDimensions.Dimensions

import Constants
import Objects

-- * Levels
-- ** Level specification
data LevelSpec = LevelSpec
 { blockCfgs  :: [(Pos2D, Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)] 
                -- ^ Block positions, block lives, kind of powerups,
                -- and whether the block signals that it contains a powerup.
                -- Note: SignalPowerUp is not logically related to the kind
                -- of powerups and can be used to irritate the player.
                -- For example, signalling that it contains a powerup, but 
                -- the powerup kind is Nothing. Or containing a powerup, but
                -- not signaling it.
 , levelBg    :: ImageResource  -- ^ Background image
 , levelMusic :: MusicResource  -- ^ Background music
 }

-- | Number of levels. Change this in the code to finish
-- in a different level.
numLevels :: Int
numLevels = length levels

-- * Concrete levels
levels :: [LevelSpec]
levels = map (\(d,b,m) -> LevelSpec d (Resource b) (Resource m))
  [ (blockDescS 0,  "data/level1.png", "data/level0.mp3")
  , (blockDescS 1,  "data/level1.png", "data/level1.mp3")
  , (blockDescS 2,  "data/level2.png", "data/level2.mp3")
  , (blockDescS 3,  "data/level1.png", "data/level0.mp3")
  , (blockDescS 4,  "data/level1.png", "data/level1.mp3")
  , (blockDescS 5,  "data/level2.png", "data/level2.mp3")
  , (blockDescS 6,  "data/level1.png", "data/level0.mp3")
  , (blockDescS 7,  "data/level1.png", "data/level1.mp3")
  , (blockDescS 8,  "data/level2.png", "data/level2.mp3")
  , (blockDescS 9,  "data/level0.png", "data/level1.mp3")
  , (blockDescS 10, "data/level1.png", "data/level1.mp3")
  , (blockDescS 11, "data/level2.png", "data/level2.mp3")
  , (blockDescS 12, "data/level0.png", "data/level1.mp3")
  , (blockDescS 13, "data/level1.png", "data/level1.mp3")
  , (blockDescS 14, "data/level2.png", "data/level2.mp3")
  , (blockDescS 15, "data/level1.png", "data/level0.mp3")
  , (blockDescS 16, "data/level1.png", "data/level1.mp3")
  ]

-- | Level block specification (positions,lives of block, maybe powerup)

-- Level 0
--
-- X = maxBlockLife - 1
-- P = powerup points minBlockLife
-- 
--   %%%%%%%%
-- % PXPXPXPX
-- % XPXPXPXP
-- % PXPXPXPX
-- 
-- Every second block is a powerup (PointsUp) with minBlockLife.
-- All other blocks have (maxBlockLife - 1).
-- Blocks containing powerups signal that they do.
blockDescS :: Int -> [(Pos2D, Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
blockDescS 0 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = zip4 blocks lives powerUps signalsPu
         where
           blocks    = blockPoss
           blockn    = length blocks
           lives     = blockLifeCycle blockn
           powerUps  = powerUpList lives
           signalsPu = signals powerUps
 
       blockPoss ::[(Int, Int)]
       blockPoss = [(x,y) | x <- [0..blockColumns - 1]
                          , y <- [0..blockRows - 1]
                   ]

       blockLifeCycle :: Int -> [Int]
       blockLifeCycle n = take n $ cycle oneCycle
         where 
           oneCycle = [minBlockLife, minBlockLife + 1] 

       powerUpList :: [Int] -> [Maybe (PowerUpKind, AlwaysPowerUp)]
       powerUpList [] = []
       powerUpList (n:ns) | n == minBlockLife = (Just (PointsUp, False) : powerUpList ns)
                          | otherwise = (Nothing : powerUpList ns)

       blockRows :: Int
       blockRows = 3


-- Level 1
--   %%%%%%%%
-- %  X P X P
-- % X P X P
-- %  X P X P
-- % X P X P
--
-- If a block with powerup is hit it creates a powerup.
-- Every second block in a row is a powerup (PointsUp).
-- All blocks containing powerups signal that they do.
-- All blocks have maxBlockLife.
blockDescS 1 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = zip4 blocks lives powerUps signalsPu
         where
           blocks    = blockPoss
           blockn    = length blocks
           lives     = blockLifeCycle blockn
           powerUps  = powerUpList blockn 
           signalsPu = signals powerUps
 


       blockPoss :: [(Int, Int)]
       blockPoss = [(x,y) | y <- [0..blockRows - 1] -- first create y then x:
                          , x <- [0..blockColumns - 1] -- order matters
                          , (even x && odd y)  ||      -- for the zipping of 
                            (odd x  && even y)         -- blocks, lives, ...
                   ]

       blockLifeCycle :: Int -> [Int]
       blockLifeCycle n = take n $ cycle [maxBlockLife]

       powerUpList :: Int -> [Maybe (PowerUpKind, AlwaysPowerUp)]
       powerUpList n = take n $ cycle oneCycle
         where
           oneCycle = [Nothing, Just (PointsUp, True)]

       blockRows :: Int
       blockRows = 4


-- Level 2
-- X == maxBlockLife
-- O == maxBlockLife - 1
-- Y == minBlockLife
-- P == minBlockLife LivesUp
--
--   %%%%%%%%
-- % YYYYYYYY
-- % YXXXXXXY
-- % YXOOOOXY
-- % YXOPPOXY
-- % YXOOOOXY
-- % YXXXXXXY
-- % YYYYYYYY
blockDescS 2 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), minBlockLife, Nothing, False) -- Y
                    | x <- [0..blockColumns - 1]
                    , y <- [0..blockRows - 1]
                    , (x == 0)
                    || (y == 0)
                    || (x == blockColumns - 1)
                    || (y == blockRows - 1)]
                   ++ [((x,y), maxBlockLife - 1, Nothing, False) -- O
                       | x <- [1..blockColumns - 2]
                       , y <- [1..blockRows - 2]
                       , (x == 1)
                       || (y == 1)
                       || (x == blockColumns - 2)
                       || (y == blockRows - 2)]
                   ++ [((x,y), maxBlockLife, Nothing, False) -- X
                       | x <- [2..blockColumns - 3]
                       , y <- [2..blockRows - 3]
                       , (x == 2)
                       || (y == 2)
                       || (x == blockColumns - 3)
                       || (y == blockRows - 3)]
                   ++ [((x,y), maxBlockLife, Just (LivesUp, False), True) -- P
                       | x <- [3..blockColumns - 4]
                       , y <- [3..blockRows - 4]
                       , (x == 3)
                       || (y == 3)
                       || (x == blockColumns - 4)
                       || (y == blockRows - 4)]
                   
       blockRows :: Int
       blockRows = 7



-- Level 3 
--
-- X == maxBlockLife
-- O == minBlockLife, destroyUp 
--
--   %%%%%%%%
-- %  XXXXXX
-- % X      X
-- %    OO  
-- %    OO 
-- % X      X
-- %  XXXXXX
blockDescS 3 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), maxBlockLife, Nothing, False) 
                    | x <- [1..blockColumns - 2]
                    , y <- [0, blockRows - 1]]
                   ++ [((x,y), maxBlockLife, Nothing, False) 
                       | x <- [0, blockColumns - 1]
                       , y <- [1, blockRows - 2]]
                   ++ [((x,y), minBlockLife, Just (DestroyUp, True) , True) 
                       | x <- midColumns
                       , y <- [2,3]]

       blockRows :: Int
       blockRows = 6


-- Level 4
--   %%%%%%%%
-- % XXXXXXXX
-- %
-- %  P P P P
-- %
-- % L L L L
-- %
-- % XXXXXXXX
--
-- X == maxBlockLife, NothingUp
-- P == maxBlockLife - 1, PointsUp (always)
-- L == minBlockLife, LivesUp
blockDescS 4 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), maxBlockLife, Just (NothingUp, True), False) -- X 
                    | x <- [0..blockColumns - 1]
                    , y <- [0, blockRows - 1]]
                   ++ [((x,y), maxBlockLife - 1, Just (PointsUp, True), True) -- P 
                       | x <- [0..blockColumns - 1]
                       , y <- [2], odd x]
                   ++ [((x,y), minBlockLife, Just (LivesUp, False), True) -- L
                       | x <- [0..blockColumns - 1]
                       , y <- [4], even x]

       blockRows :: Int
       blockRows = 7


-- Level 5
--   %%%%%%%%
-- % XX XXXXX
-- % XX XXXXX
-- % 
-- % DD DDDDD
-- % XX XXXXX
-- % XX XXXXX
-- % XX XXXXX
-- % XX XXXXX
-- % XX XXXXX
-- % XX XXXXX
-- 
-- X == minBlockLife
-- D == maxBlockLife, DestroyUp (always)
blockDescS 5 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), minBlockLife, Nothing, False) -- X
                    | x <- [0..blockColumns - 1]
                    , y <- [0..blockRows - 1]
                    , x /= 2, y /= 2, y /= 3]
                   ++ [((x,y), maxBlockLife, Just (DestroyUp, True), True) -- D
                    | x <- [0..blockColumns - 1]
                    , y <- [3]
                    , x /= 2]
                    
       blockRows :: Int
       blockRows = 9




-- Level 6 
-- X == maxBlockLife
-- T == maxBlockLife - 1, PointsUp
-- O == minBlockLife
-- L == minBlockLife, LivesUp
-- 
--   %%%%%%%%
-- % XXXXXXXX
-- %  O O O O
-- %  L L L L
-- % T T T T
-- % T T T T
-- %  L L L L
-- %  O O O O
-- % 
-- % XXX XXXX
blockDescS 6 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), maxBlockLife - 1, Just (PointsUp, True), False) -- T
                    | x <- [0..blockColumns - 1]
                    , y <- [3, 4]
                    , odd x]
                  ++ [((x,y), minBlockLife, Nothing, False) -- O
                       | x <- [0..blockColumns - 1]
                       , y <- [1, blockRows - 3]
                       , even x ]
                  ++ [((x,y), minBlockLife, Just (LivesUp, False), True) -- L
                       | x <- [0..blockColumns - 1]
                       , y <- [2, blockRows - 4]
                       , even x ]
                   ++ [((x,y), maxBlockLife, Nothing, False) -- X
                       | x <- [0..blockColumns - 1]
                       , y <- [0, blockRows - 1]
                       , y == 0 || x /= midColumn]
       blockRows :: Int
       blockRows = 9


-- Level 7 
-- X == maxBlockLife
-- O == minBlockLife
-- T == maxBlockLife - 1, PointsUp
-- 
--   %%%%%%%%
-- % XXXOOXXX
-- % XXXOOXXX
-- % 
-- %  T T T T
-- % T T T T
-- % 
-- % XXXOOXXX
-- % XXXOOXXX
blockDescS 7 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), maxBlockLife - 1, Just (PointsUp, False), True)
                    | x <- [0..blockColumns - 1]
                    , y <- midColumns
                    , (even x && y == midColumn)
                    || (odd x && y == midColumn -1)]
                   ++ [((x,y), minBlockLife, Just (NothingUp, True), False) 
                       | x <- [midColumn - 1, midColumn]
                       , y <- [0, 1, blockRows - 2, blockRows - 1]]
                   ++ [((x,y), maxBlockLife, Nothing, False) 
                       | x <- [0..blockColumns - 1]
                       , y <- [0, 1, blockRows - 2, blockRows - 1]
                       , x /= midColumn - 1, x /= midColumn]


       blockRows :: Int
       blockRows = 8



-- Level 8 
--   %%%%%%%%
-- % XXXXXXX
-- %   D D
-- %   D D
-- %
-- %  D   D
-- % D     D
-- 
-- X = maxBlockLife, LivesUp (always)
-- D = maxBlockLife, DestroyUp (always)
blockDescS 8 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), maxBlockLife, Just (LivesUp, True), False) 
                    | x <- [0..blockColumns - 1]
                    , y <- [0]]
                   ++ [((x,y), maxBlockLife, Just (DestroyUp, True), False) 
                       | x <- [2, blockColumns - 3]
                       , y <- [1, 2]]
                   ++ [ ((1,4), maxBlockLife, Just (DestroyUp, True), False)
                      , ((blockColumns - 2,4), maxBlockLife, Just (DestroyUp, True), False)
                      , ((0,5), maxBlockLife, Just (DestroyUp, True), False)
                      , ((blockColumns - 1,5), maxBlockLife, Just (DestroyUp, True), False)
                      ]





-- Level 9
-- X == maxBlockLife
-- P == minBlockLife, PointsUp
-- L == minBlockLife, LivesUp
-- T == maxBlockLife - 1
-- 
--   %%%%%%%%
-- %  X X X
-- %  X X X 
-- %  X X X 
-- %  X X X 
-- % LPLPLPL
-- % T T T 
-- % T T T 
-- % T T T
-- % T T T 
blockDescS 9 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), maxBlockLife, Just (NothingUp, True), False) -- X
                    | x <- [0..blockColumns - 1]
                    , y <- [0..midRow - 1], odd x]
                   ++ [((x,y), minBlockLife, powerUps x, True) -- O
                       | x <- [0..blockColumns - 1]
                       , y <- [midRow]]
                   ++ [((x,y), maxBlockLife - 1, Just (NothingUp, True), False) -- T
                       | x <- [0..blockColumns - 1]
                       , y <- [midRow + 1..blockColumns - 1]
                       , even x]


       powerUps :: Int -> Maybe (PowerUpKind, AlwaysPowerUp)
       powerUps x | even x     = Just (PointsUp, True)
                  | odd x      = Just (LivesUp, True)
 
       blockRows :: Int
       blockRows = 9

       midRow :: Int
       midRow = blockRows `div` 2



-- Level 10 
--   %%%%%%%%
-- %  X X X
-- %  X X X 
-- %  X X X 
-- %  X X X 
-- % PPPPPPP
-- %  X X X 
-- %  X X X 
-- %  X X X
-- %  X X X
--
-- X == maxBlockLife, NothingUp
-- P == maxBlockLife - 1, DestroyUp 
blockDescS 10 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks =  [((x,y), blockLife y, powerUps y, False) 
                     | x <- [0..blockColumns - 1]
                     , y <- [0..blockRows - 1]
                     , odd x || (y == midRow && even x)]

       blockLife :: Int -> Int  
       blockLife y | y == midRow = maxBlockLife - 1
                   | otherwise   = maxBlockLife

       powerUps :: Int -> Maybe (PowerUpKind, AlwaysPowerUp)
       powerUps y | y == midRow = Just (DestroyUp, True)
                  | otherwise   = Just (NothingUp, True) 

       blockRows :: Int
       blockRows = 9

       midRow :: Int
       midRow = blockRows `div` 2


-- BACKUP Levels

-- Level 11
--   %%%%%%%%
-- %    XXXX
-- %   XXXXX 
-- %  XXXXXX
-- % XXXXXX
-- % XXXXX
-- % XXXX
--
blockDescS 11 = map (first4 adjustPos) allBlocks
 where
   allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
   allBlocks = [((x,y), maxBlockLife, Nothing, False) 
                 | x <- [0..blockColumns - 1]
                 , y <- [0..blockRows - 1]
                 , (x + y > 2) && (x + y < 10)
               ]
               
   blockRows :: Int
   blockRows = 6

-- Level 12
--   %%%%%%%%
-- % XXXXXXXX
-- % X XXXX X
-- % XX XX XX
-- % XXX  XXX
blockDescS 12  = map (first4 adjustPos) allBlocks
 where
   allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
   allBlocks = [((x,y), maxBlockLife, Nothing, False) 
                | x <- [0..blockColumns - 1]
                , y <- [0..blockRows - 1]
                , x /= y && (blockColumns - 1 - x) /= y
               ]

   blockRows :: Int
   blockRows = 4
               

-- Level 13
--   %%%%%%%%
-- %    XX
-- %   X  X
-- %  X    X
-- % XXXXXXXX
blockDescS 13 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = nub $
         [((3,0), maxBlockLife, Nothing, False),((blockColumns - 4,0), maxBlockLife, Nothing, False)]
         ++ [((2,1), maxBlockLife, Nothing, False),((blockColumns - 3,1), maxBlockLife, Nothing, False)]
         ++ [((1,2), maxBlockLife, Nothing, False),((blockColumns - 2,2), maxBlockLife, Nothing, False)]
         ++ [((x,y), maxBlockLife, Nothing, False) | x <- [0..blockColumns - 1], y <- [3]]

-- Level 14
--   %%%%%%%%
-- %  XXXXXX
-- % X      X
-- % X      X
-- % X      X
-- %  XXXXXX
blockDescS 14 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), maxBlockLife, Nothing, False) 
                    | x <- [1..blockColumns - 2]
                    , y <- [0, blockRows - 1]]
                   ++ [((x,y), maxBlockLife, Nothing, False) 
                       | x <- [0, blockColumns - 1]
                       , y <- [1..blockRows - 2]]

       blockRows :: Int
       blockRows = 5


-- Level 15
--   %%%%%%%%
-- %    X
-- %   XXX
-- %  X X X
-- % XXXXXXX
-- %  X X X
-- %   XXX
-- %    X
blockDescS 15 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), maxBlockLife, Nothing, False) | x <- midColumns, y <- [0..6]]
                   ++ [((x,y), maxBlockLife, Nothing, False) | x <- [0..blockColumns-1], y <- [3]]
                   ++ [((x,y), maxBlockLife, Nothing, False) | x <- [2,blockColumns-3], y <- [1,5]]
                   ++ [((x,y), maxBlockLife, Nothing, False) | x <- [1,blockColumns-2], y <- [2,4]]


-- Level 16
--   %%%%%%%%
-- % XX     
-- % XXXX     
-- % XXXXXX  
-- % XXXXXXXX
-- % XXXXXX  
-- % XXXX     
-- % XX     
blockDescS 16 = map (first4 adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp)]
       allBlocks = [((x,y), maxBlockLife, Nothing, False)
                   | y <- [0..blockRows-1]
                   , x <- [0..(blockColumns-1) - (2 * abs (y - midRow))]
                   ]

       blockRows :: Int
       blockRows = 7

       midRow :: Int
       midRow = blockRows `div` 2


blockDescS _ = error "No more levels"


-- Dynamic positioning/level size

adjustPos :: (Int, Int) -> (Double, Double)
adjustPos = (adjustHPos . fI) *** (adjustVPos . fI)
  where
    adjustVPos = (gameAreaTopMargin +) . ((blockHeight + blockSeparation) *)
    adjustHPos = (leftMargin +) . ((blockWidth + blockSeparation) *)
    fI         = fromIntegral

-- Fit as many as possible
blockColumns :: Int
blockColumns =
    1 + floor ( (gameWidth - blockWidth - 2 * gameAreaMinLeftMargin)
              / (blockWidth + blockSeparation)
              )

midColumn :: Int
midColumn = blockColumns `div` 2

midColumns :: [Int]
midColumns | odd blockColumns = [midColumn]
           | otherwise        = [midColumn - 1, midColumn]

leftMargin :: Double
leftMargin = round' $ (gameWidth - maxBlocksWidth) / 2
  where
    round' :: Double -> Double
    round' x = fromIntegral (floor x :: Int)
    maxBlocksWidth =
      blockWidth + (blockWidth + blockSeparation) * (fromIntegral blockColumns - 1)

-- Matching list of powerups with a list that signals that powerup
signals :: [Maybe (PowerUpKind, AlwaysPowerUp)] -> [SignalPowerUp]
signals [] = []
signals (Nothing :puks) = (False : signals puks)
signals (_:puks)            = (True  : signals puks)   

-- Auxiliary functions

first4 :: (a -> a') -> (a, b, c, d) -> (a', b, c, d)
first4 f (a, b, c, d) = (f a, b, c, d)
