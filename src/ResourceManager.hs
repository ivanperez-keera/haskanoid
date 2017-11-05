module ResourceManager where

import           Control.Applicative       ((<$>))
import           Control.Monad
import           Control.Monad.IfElse
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.IORef
import           Data.Maybe
import           Game.AssetManager         hiding (audio)
import           Game.AssetManager.SDL1
import           Game.Audio.SDL
import           Graphics.UI.SDL           as SDL
import           Graphics.UI.SDL.Image     as Image
import qualified Graphics.UI.SDL.TTF       as TTF

import Constants
import GameState
import Levels
import Objects
import Paths_haskanoid
import Resources

-- * Resource management

newtype ResourceMgr = ResourceMgr { unResMgr :: IORef ResourceManager }

data ResourceManager = ResourceManager
  { lastKnownStatus :: GameStatus
  , resources       :: Resources
  }

-- | Ad-hoc resource loading
-- This function is ad-hoc in two senses: first, because it
-- has the paths to the files hard-coded inside. And second,
-- because it loads the specific resources that are needed,
-- not a general
--
loadResources :: IO (Maybe ResourceMgr)
loadResources = runMaybeT $ do
  -- Font initialization
  ttfOk <- lift TTF.init

  gameFont <- liftIO $ getDataFileName "data/lacuna.ttf"
  -- Load the fonts we need
  font  <- liftIO $ TTF.tryOpenFont gameFont 32 -- What does the 32 do?
  let myFont = fmap (Font gameFont) font

  blockHit <- liftIO $ loadAudio =<< getDataFileName "data/196106_aiwha_ding-cc-by.wav"

  -- bgM <- liftIO $ loadMusic "Ckotty_-_Game_Loop_11.ogg"
  -- bgM <- liftIO $ loadMusic "data/level0.mp3"

  -- let levelBg = "data/level0.png"
  -- img <- lift $ fmap (Image levelBg) $ load levelBg

  ballImg <- liftIO $ getDataFileName "data/ball2.png"
  ball <- lift $ Image ballImg <$> load ballImg

  b1Img <- liftIO $ getDataFileName "data/block1.png"
  b1 <- lift $ Image b1Img <$> load b1Img

  b2Img <- liftIO $ getDataFileName "data/block2.png"
  b2 <- lift $ Image b2Img <$> load b2Img

  b3Img <- liftIO $ getDataFileName "data/block3.png"
  b3 <- lift $ Image b3Img <$> load b3Img

  paddleImg <- liftIO $ getDataFileName "data/paddleBlu.png"
  paddle <- lift $ Image paddleImg <$> load paddleImg

  -- Start playing music
  -- when (isJust bgM) $ lift (playMusic (fromJust bgM))

  -- Return Nothing or embed in Resources
  res <- case (myFont, blockHit) of
           (Just f, Just b) -> let
                               in return (Resources f b Nothing ball b1 b2 b3 paddle Nothing)
           _                        -> do liftIO $ putStrLn "Some resources could not be loaded"
                                          mzero

  liftIO $ ResourceMgr <$>
    newIORef (ResourceManager GameStarted res)


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
  newMusicFP <- getDataFileName newMusicFP'

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
                               return bgM

  -- Load new background
  let newBgFP' = _resourceFP $ levelBg $ levels !! n

  newBgFP <- getDataFileName newBgFP'

  let oldBg   = bgImage res
      oldBgFP = maybe "" imgName oldBg

  newBg <- if oldBgFP == newBgFP
             then return oldBg
             else do img' <- load newBgFP
                     return $ Just (Image newBgFP img')

  return (res { bgImage = newBg, bgMusic = newMusic })
