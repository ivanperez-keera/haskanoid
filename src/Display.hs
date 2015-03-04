module Display where

import Control.Monad
import Control.Monad.IfElse
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import Graphics.UI.SDL       as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as Image

import Audio
import Constants
import GameState
import Objects
import Resources hiding (audio)
import Levels
import Paths_haskanoid

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
  ball <- lift $ fmap (Image ballImg) $ load ballImg

  b1Img <- liftIO $ getDataFileName "data/block1.png"
  b1 <- lift $ fmap (Image b1Img) $ load b1Img

  b2Img <- liftIO $ getDataFileName "data/block2.png"
  b2 <- lift $ fmap (Image b2Img) $ load b2Img

  b3Img <- liftIO $ getDataFileName "data/block3.png"
  b3 <- lift $ fmap (Image b3Img) $ load b3Img

  paddleImg <- liftIO $ getDataFileName "data/paddleBlu.png"
  paddle <- lift $ fmap (Image paddleImg) $ load paddleImg

  -- Start playing music
  -- when (isJust bgM) $ lift (playMusic (fromJust bgM))

  -- Return Nothing or embed in Resources
  res <- case (myFont, blockHit) of
           (Just f, Just b) -> let 
                               in return (Resources f b Nothing ball b1 b2 b3 paddle Nothing)
           _                        -> do liftIO $ putStrLn "Some resources could not be loaded"
                                          mzero

  liftIO $ fmap ResourceMgr $
    newIORef (ResourceManager (GameStarted) (res))

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

  initAudio

initGraphs :: IO ()
initGraphs = do
  -- Create window
  screen <- SDL.setVideoMode (round width) (round height) 32 [SWSurface]
  SDL.setCaption "Test" ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor False


render :: ResourceMgr -> GameState -> IO()
render resourceManager shownState = do
  resources <- loadNewResources resourceManager shownState
  audio   resources shownState
  display resources shownState

audio :: Resources -> GameState -> IO()
audio resources shownState = do
  -- Start bg music if necessary
  playing <- musicPlaying
  unless playing $ awhen (bgMusic resources) playMusic 

  -- Play object hits
  mapM_ (audioObject resources) $ gameObjects shownState

audioObject resources object = when (objectHit object) $
  case objectKind object of
    (Block _ _) -> playFile (blockHitSnd resources) 3000
    _           -> return ()

display :: Resources -> GameState -> IO()
display resources shownState = do 
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen

  -- Paint background

  awhen (bgImage resources) $ \bg' -> void $ do
    let bg     = imgSurface bg'
    let rectBg = SDL.Rect 0 0 (round width) (round height)
    SDL.blitSurface bg Nothing screen $ Just rectBg

  hud <- createRGBSurface [SWSurface]
             (round width) (round gameTop)
             32 0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
  paintGeneral hud resources (gameInfo shownState)
  let rectHud = SDL.Rect 0 0 (round width) (round gameTop)
  SDL.blitSurface hud Nothing screen $ Just rectHud

  -- The following line is BIG_ENDIAN specific
  -- The 32 is important because we are using Word32 for the masks
  -- FIXME: Should I use HWSurface and possibly other flags (alpha?)?
  surface <- createRGBSurface [SWSurface]
             (round gameWidth) (round gameHeight)
             32 0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
  paintGeneralMsg surface resources (gameStatus (gameInfo shownState))
  mapM_ (paintObject resources surface) $ gameObjects shownState
  let rect = SDL.Rect (round gameLeft) (round gameTop) (round gameWidth) (round gameHeight)
  SDL.blitSurface surface Nothing screen $ Just rect

  -- Double buffering
  SDL.flip screen

paintGeneral screen resources over = void $ do
  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format 0x11 0x22 0x33
  fillRect screen Nothing bgColor
  paintGeneralHUD screen resources over

paintGeneralMsg screen resources GamePlaying     = return ()
paintGeneralMsg screen resources GamePaused      = paintGeneralMsg' screen resources "Paused"
paintGeneralMsg screen resources (GameLoading n) = paintGeneralMsg' screen resources ("Level " ++ show n)
paintGeneralMsg screen resources GameOver        = paintGeneralMsg' screen resources "GAME OVER!!!"
paintGeneralMsg screen resources GameFinished    = paintGeneralMsg' screen resources "You won!!! Well done :)"

paintGeneralMsg' screen resources msg = void $ do
  let font = resFont resources
  message <- TTF.renderTextSolid (unFont font) msg (SDL.Color 128 128 128)
  let x = (SDL.surfaceGetWidth  screen - w) `div` 2
      y = (SDL.surfaceGetHeight screen - h) `div` 2
      w = SDL.surfaceGetWidth  message
      h = SDL.surfaceGetHeight message
  SDL.blitSurface message Nothing screen $ Just (SDL.Rect x y w h)

paintGeneralHUD screen resources over = void $ do
  let font = unFont $ resFont resources
  message1 <- TTF.renderTextSolid font ("Level: " ++ show (gameLevel over)) (SDL.Color 128 128 128)
  let w1 = SDL.surfaceGetWidth  message1
      h1 = SDL.surfaceGetHeight message1
  SDL.blitSurface message1 Nothing screen $ Just (SDL.Rect 10 10 w1 h1)
  message2 <- TTF.renderTextSolid font ("Points: " ++ show (gamePoints over)) (SDL.Color 128 128 128)
  let w2 = SDL.surfaceGetWidth  message2
      h2 = SDL.surfaceGetHeight message2
  SDL.blitSurface message2 Nothing screen $ Just (SDL.Rect 10 (10 + h2 + 5) w2 h2)
  message3 <- TTF.renderTextSolid font ("Lives: " ++ show (gameLives over)) (SDL.Color 128 128 128)
  let rightMargin = SDL.surfaceGetWidth screen
      w2 = SDL.surfaceGetWidth  message3
      h2 = SDL.surfaceGetHeight message3
  SDL.blitSurface message3 Nothing screen $ Just (SDL.Rect (rightMargin - 10 - w2) 10 w2 h2)

paintObject resources screen object = do
  red <- mapRGB format 0xFF 0 0
  case objectKind object of
    (Paddle (w,h))  -> void $ do let bI = imgSurface $ paddleImg resources
                                 t <- mapRGB (surfaceGetPixelFormat bI) 0 255 0 
                                 setColorKey bI [SrcColorKey, RLEAccel] t 
                                 SDL.blitSurface bI Nothing screen $ Just (SDL.Rect x y (round w) (round h))
    (Block e (w,h)) -> void $ do let bI = imgSurface $ blockImage e
                                 SDL.blitSurface bI Nothing screen $ Just (SDL.Rect x y (round w) (round h))
    (Ball r)        -> void $ do let x' = x - round r
                                     y' = y - round r
                                     sz = round (2*r)
                                 -- b <- convertSurface (imgSurface $ ballImg resources) (format) []
				 let bI = imgSurface $ ballImg resources
                                 t <- mapRGB (surfaceGetPixelFormat bI) 0 255 0 
                                 setColorKey bI [SrcColorKey, RLEAccel] t 
                                 SDL.blitSurface bI Nothing screen $ Just (SDL.Rect x' y' sz sz)
    _              -> return ()
  where format = surfaceGetPixelFormat screen
        p      = objectPos object
        x      = round (fst p)
        y      = round (snd p)
        blockImage 3 = block1Img resources
        blockImage 2 = block2Img resources
        blockImage n = block3Img resources

newtype ResourceMgr = ResourceMgr { unResMgr :: IORef ResourceManager }

data ResourceManager = ResourceManager
  { lastKnownStatus :: GameStatus
  , resources       :: Resources
  }

data Resources = Resources
  { resFont     :: Font
  , blockHitSnd :: Audio
  , bgImage     :: Maybe Image
  , ballImg     :: Image
  , block1Img   :: Image
  , block2Img   :: Image
  , block3Img   :: Image
  , paddleImg   :: Image
  , bgMusic     :: Maybe Music
  }

data Image = Image { imgName  :: String, imgSurface :: Surface }
data Font  = Font  { fontName :: String, unFont :: TTF.Font }

loadNewResources :: ResourceMgr ->  GameState -> IO Resources
loadNewResources mgr state = do
  manager <- readIORef (unResMgr mgr)
  let oldState = lastKnownStatus manager
      newState = gameStatus (gameInfo state)
      oldResources = resources manager

  newResources <- case newState of
                    (GameLoading _) | (newState /= oldState)
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

  newMusic <- if (oldMusicFP == newMusicFP)
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
