{-# LANGUAGE CPP #-}
module ResourceManager where

import Data.IORef
import Game.Resource.Manager.IORef ()
import Game.Resource.Spec

import ResourceSpecs

#if defined(sdl) || defined (sdl2)
import Game.Resource.Manager.SDL as SDLResourceMgr
#endif

data ResourceId = IdGameFont
                | IdGameFontColor
                | IdBgMusic
                | IdBlockHitFX
                | IdBgImg
                | IdBallImg
                | IdBlock1Img
                | IdBlock2Img
                | IdBlock3Img
                | IdBlockPuImg
                | IdPaddleImg
                | IdPointsUpImg
                | IdLivesUpImg
                | IdMockUpImg
                | IdDestroyBallUpImg
  deriving (Ord, Eq)

gameResourceSpec :: ResourceSpec ResourceId
gameResourceSpec = ResourceSpec
  { fonts  = [ (IdGameFont,         gameFontSpec) ]
  , images = [ (IdBgImg,            initialBG)
             , (IdBallImg,          ballImage)
             , (IdBlock1Img,        block1Image)
             , (IdBlock2Img,        block2Image)
             , (IdBlock3Img,        block3Image)
             , (IdBlockPuImg,       blockPuImage)
             , (IdPaddleImg,        paddleImage)
             , (IdPointsUpImg,      pointsUpImage)
             , (IdLivesUpImg,       livesUpImage)
             , (IdMockUpImg,        mockUpImage)
             , (IdDestroyBallUpImg, destroyBallUpImage)
             ]
  , sounds = [ (IdBlockHitFX,       blockHitSFX) ]
  , colors = [ (IdGameFontColor,    fontColor) ]
  , music  = [ (IdBgMusic,          backgroundMusic) ]
  }

type ResourceMgr = SDLResourceMgr.ResourceManager IORef ResourceId

-- * Resource management
-- loadNewResources :: ResourceMgr ->  GameState -> IO Resources
-- loadNewResources mgr state = do
--   manager <- readIORef (unResMgr mgr)
--   let newState     = gameStatus (gameInfo state)
--       oldResources = resources manager
--   return oldResources

--   newResources <- updateAllResources oldResources newState
-- 
--   let manager' = ResourceManager { resources = newResources }
-- 
--   writeIORef (unResMgr mgr) manager'
--   return newResources
-- 
-- updateAllResources :: Resources -> GameStatus -> IO Resources
-- updateAllResources res (GameLoading n) = do
--   -- Load new music
--   let newMusicFP' = _resourceFP $ levelMusic $ levels !! n
--   newMusicFP <- fst <$> localizeResource (newMusicFP', ())
-- 
--   let oldMusic   = bgMusic res
--       oldMusicFP = maybe "" musicName oldMusic
--   newMusic <- if oldMusicFP == newMusicFP
--               then return oldMusic
--               else do -- Loading can fail, in which case we continue
--                       -- with the old music
--                       bgM <- loadMusic newMusicFP
--                       if isNothing bgM
--                        then do putStrLn $ "Could not load resource " ++ newMusicFP
--                                return oldMusic
--                        else do stopMusic
--                                playMusic (fromJust bgM)
--                                return bgM
-- 
--   -- Load new background
--   let newBgFP' = _resourceFP $ levelBg $ levels !! n
--   newBgFP <- fst <$> localizeResource (newBgFP', Nothing)
-- 
--   -- Load the next background image, if available and necessary
--   let oldBg   = bgImage res
--       oldBgFP = maybe "" imgName oldBg
-- 
--   newBg <- if oldBgFP == newBgFP
--              then return oldBg
--              else loadImage (newBgFP, Nothing)
-- 
--   return (res { bgImage = newBg, bgMusic = newMusic })
-- 
-- updateAllResources res _ = return res

-- localizeResource :: (FilePath, a) -> IO (FilePath, a)
-- localizeResource (fp, extra) = do
--   fp' <- getDataFileName fp
--   return (fp', extra)
