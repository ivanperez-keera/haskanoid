module ResourceManagerSDL2 where

import           Control.Monad
import           Data.IORef
import           Data.Maybe
import           Data.IORef.Extra
import           Game.Audio.SDL2
import           Game.AssetManager.SDL2     hiding (loadImage)
import qualified Game.AssetManager.SDL2     as Res
import           Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.TTF        as TTF

import Resources
import ResourcesSDL2

newtype ResourceMgr = ResourceMgr { unResMgr :: IORef ResourceManager }

data ResourceManager = ResourceManager
  { resources       :: Resources
  }

-- | Ad-hoc resource loading
-- FIXME: Make it more generic, and also the resource manager.
loadResources :: IO (Maybe ResourceMgr)
loadResources = do
  -- Initialize needed subsystems
  TTF.init
  initAudio

  -- Load the resources we need
  font     <- loadFont gameFontSpec
  
  bgM      <- loadMusic   backgroundMusic
  blockHit <- loadSoundFX blockHitSFX
  img      <- loadImage   initialBG
  ball     <- loadImage   ballImage
  b1       <- loadImage   block1Image
  b2       <- loadImage   block2Image
  b3       <- loadImage   block3Image
  paddle   <- loadImage   paddleImage
  pointsUp <- loadImage   pointsUpImage
  livesUp  <- loadImage   livesUpImage

  -- FIXME: This sould not be here: start playing music
  when (isJust bgM) $ playMusic (fromJust bgM)

  -- Return Nothing or embed in Resources
  let res = Resources <$> font
                      <*> blockHit
                      <*> pure img
                      <*> ball
                      <*> b1
                      <*> b2
                      <*> b3
                      <*> paddle
                      <*> pointsUp
                      <*> livesUp
                      <*> pure bgM

   -- Some resources did not load
  case res of
    Nothing   -> do putStrLn "Something failed to load"
                    return Nothing
    Just res' -> do newMgr <- newIORef (ResourceManager res')
                    return $ Just $ ResourceMgr newMgr

preloadResources :: ResourceMgr -> Renderer -> IO ()
preloadResources mgr rdr = void $ do
  modifyIORefM (unResMgr mgr) $ \mgr' -> do

    let res = resources mgr'

    bgImage'     <- maybe (return Nothing) ((Just <$>) . maybePreloadImage rdr) (bgImage res)
    ballImg'     <- maybePreloadImage rdr (ballImg    res)
    block1Img'   <- maybePreloadImage rdr (block1Img  res)
    block2Img'   <- maybePreloadImage rdr (block2Img  res)
    block3Img'   <- maybePreloadImage rdr (block3Img  res)
    paddleImg'   <- maybePreloadImage rdr (paddleImg  res)
    pointsUpImg' <- maybePreloadImage rdr (pointsUpImg res)
    livesUpImg'  <- maybePreloadImage rdr (livesUpImg res)

    let res' = res { bgImage     = bgImage'
                   , ballImg     = ballImg'
                   , block1Img   = block1Img'
                   , block2Img   = block2Img'
                   , block3Img   = block3Img'
                   , paddleImg   = paddleImg'
                   , pointsUpImg = pointsUpImg'
                   , livesUpImg  = livesUpImg'
                   }
    return (mgr' { resources = res' })

-- loadNewResources (GamePlaying (prefs, state)) = do
--   mgr     <- unResMgr <$> getResourceManagerDM
--   manager <- lift $ readIORef mgr
--   let oldState     = lastKnownStatus manager
--       newState     = gameStatus (gameInfo state)
--       oldResources = resources manager
-- 
--   newResources <- case newState of
--                     GameLoading n | newState /= oldState
--                                   -> updateAllResources oldResources newState
--                     _             -> return oldResources
-- 
--   let manager' = ResourceManager { lastKnownStatus = newState
--                                  , resources       = newResources
--                                  }
-- 
--   lift $ writeIORef mgr manager'
-- 
--   return newResources
-- 
-- updateAllResources :: Resources -> GameStatus -> DisplayMonad Resources
-- updateAllResources res (GameLoading n) = do
-- 
--   let newBgFP    = _resourceFP $ levelBg    $ levels !! n
--   let newMusicFP = _resourceFP $ levelMusic $ levels !! n
-- 
--   -- Load the next background music, if available and necessary
--   let oldMusic = bgMusic $ gameRes res
--   newMusic <- lift $
--                 if musicName oldMusic == newMusicFP
--                    then return oldMusic
--                    else do bgM <- loadMusic newMusicFP
--                            case bgM of
--                              Nothing -> do return oldMusic
--                              Just m  -> do stopMusic
--                                            playMusic m
--                                            return m
-- 
--   -- Load the next background image, if available and necessary
--   let oldBg = bgImage $ gameRes res
--   newBg <- if imgName oldBg == newBgFP
--              then return oldBg
--              else (lift . tryLoadImage newBgFP Nothing . Just) =<< getRendererDM
-- 
--   let gameRes' = (gameRes res) { bgMusic = newMusic, bgImage = newBg }
--       res'     = res { gameRes = gameRes'}
-- 
--   return res'

-- ** Resources

loadSoundFX :: SoundFXSpec -> IO (Maybe Audio)
loadSoundFX (fp, dur) = loadAudio fp dur

loadImage :: ImageSpec -> IO (Maybe Image)
loadImage (fp, Nothing) = Just <$> Res.loadImage fp
loadImage (fp, mask)    = Just <$> tryLoadImage fp mask Nothing

loadFont :: FontSpec -> IO (Maybe Font)
loadFont (fp, lh) = Just . (\ttf -> Font fp ttf) <$> TTF.openFont fp lh

