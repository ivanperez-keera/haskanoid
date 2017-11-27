{-# LANGUAGE CPP #-}
module ResourceManagerSDL2 where

import           Control.Applicative    ((<$>))
import           Control.Monad
import           Data.IORef
import           Data.Maybe
import           Game.AssetManager      hiding (audio)
import qualified Graphics.UI.SDL.TTF    as TTF

import GameState
import Levels
import Paths_haskanoid
import Resources
import ResourcesSDL2

#ifdef sdl
import           Game.AssetManager.SDL1 hiding (loadImage)
import qualified Game.AssetManager.SDL1 as Res
import           Game.Audio.SDL
#elif sdl2
import           Data.IORef.Extra
import           Game.AssetManager.SDL2 hiding (loadImage)
import qualified Game.AssetManager.SDL2 as Res
import           Game.Audio.SDL2
import           Graphics.UI.SDL        as SDL
#endif

-- * Resource management

newtype ResourceMgr = ResourceMgr { unResMgr :: IORef ResourceManager }

data ResourceManager = ResourceManager
  { lastKnownStatus :: GameStatus
  , resources       :: Resources
  }

-- | Ad-hoc resource loading
-- FIXME: Make it more generic, and also the resource manager.
loadResources :: IO (Maybe ResourceMgr)
loadResources = do
  -- Initialize needed subsystems
  TTF.init
  initAudio

  -- Load the resources we need
  font     <- loadFont    =<< localizeResource gameFontSpec
  bgM      <- loadMusic   =<< fst <$> localizeResource (backgroundMusic, ())
  blockHit <- loadSoundFX =<< localizeResource blockHitSFX
  img      <- loadImage   =<< localizeResource initialBG
  ball     <- loadImage   =<< localizeResource ballImage
  b1       <- loadImage   =<< localizeResource block1Image
  b2       <- loadImage   =<< localizeResource block2Image
  b3       <- loadImage   =<< localizeResource block3Image
  paddle   <- loadImage   =<< localizeResource paddleImage
  pointsUp <- loadImage   =<< localizeResource pointsUpImage
  livesUp  <- loadImage   =<< localizeResource livesUpImage

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
    Just res' -> (Just . ResourceMgr)
                    <$> newIORef (ResourceManager GameStarted res')

#ifdef sdl2
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
#endif

loadNewResources :: ResourceMgr ->  GameState -> IO Resources
loadNewResources mgr state = do
  manager <- readIORef (unResMgr mgr)
  let oldState = lastKnownStatus manager
      newState = gameStatus (gameInfo state)
      oldResources = resources manager

  newResources <- case newState of
                    (GameLoading _) | newState /= oldState
                                    -> updateAllResources oldResources newState
                    _               -> return oldResources

  let manager' = ResourceManager { lastKnownStatus = newState
                                 , resources       = newResources
                                 }

  writeIORef (unResMgr mgr) manager'
  return newResources

updateAllResources :: Resources -> GameStatus -> IO Resources
updateAllResources res (GameLoading n) = do
  -- Load new music
  let newMusicFP' = _resourceFP $ levelMusic $ levels !! n
  newMusicFP <- fst <$> localizeResource (newMusicFP', ())

  let oldMusic   = bgMusic res
      oldMusicFP = maybe "" musicName oldMusic
  newMusic <- if oldMusicFP == newMusicFP
              then return oldMusic
              else do -- Loading can fail, in which case we continue
                      -- with the old music
                      bgM <- loadMusic newMusicFP
                      if isNothing bgM
                       then do putStrLn $ "Could not load resource " ++ newMusicFP
                               return oldMusic
                       else do stopMusic
                               playMusic (fromJust bgM)
                               return bgM

  -- Load new background
  let newBgFP' = _resourceFP $ levelBg $ levels !! n
  newBgFP <- fst <$> localizeResource (newBgFP', Nothing)

  -- Load the next background image, if available and necessary
  let oldBg   = bgImage res
      oldBgFP = maybe "" imgName oldBg

  newBg <- if oldBgFP == newBgFP
             then return oldBg
             else loadImage (newBgFP, Nothing)

  return (res { bgImage = newBg, bgMusic = newMusic })

-- ** Resources

loadSoundFX :: SoundFXSpec -> IO (Maybe Audio)
loadSoundFX (fp, dur) = loadAudio fp dur

loadImage :: ImageSpec -> IO (Maybe Image)
loadImage (fp, Nothing) = Just <$> Res.loadImage fp
#ifdef sdl
loadImage (fp, mask)    = Just <$> tryLoadImage fp mask
#elif sdl2
loadImage (fp, mask)    = Just <$> tryLoadImage fp mask Nothing
#endif

loadFont :: FontSpec -> IO (Maybe Font)
loadFont (fp, lh) = Just . (\ttf -> Font fp ttf) <$> TTF.openFont fp lh

localizeResource :: (FilePath, a) -> IO (FilePath, a)
localizeResource (fp, extra) = do
  fp' <- getDataFileName fp
  return (fp', extra)
