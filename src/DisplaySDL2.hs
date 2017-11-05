{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module DisplaySDL2 where

import           Control.Applicative
import           Control.Arrow              ((***))
import           Control.Monad
import           Control.Extra.Monad
import           Control.Monad.IfElse
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.IORef
import           Data.List                  (find)
import           Data.Maybe
import           Data.Word
import           Data.IORef.Extra
import           FRP.Yampa.VectorSpace
import           Game.Audio.SDL2
import           Game.Render.SDL2
import           Game.AssetManager.SDL2     hiding (loadImage)
import qualified Game.AssetManager.SDL2     as Res
import           Graphics.UI.SDL            as SDL
import           Graphics.UI.SDL.Mouse      as SDL
import           Graphics.UI.SDL.Surface    as SDL
import qualified Graphics.UI.SDL.TTF        as TTF
import qualified Graphics.UI.SDL.TTF.Types  as TTF
import           System.IO.Unsafe
import           Text.Printf

-- import Audio
import Constants
import GameState
import Objects
import Resources hiding (audio)
import Levels
import Physics.TwoDimensions.Dimensions

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

  -- initAudio

initGraphs :: ResourceMgr -> IO (Renderer, Window)
initGraphs mgr = do
  -- Create window
  (window,renderer) <- SDL.createWindowAndRenderer (Size (round width) (round height)) [WindowShown ]
  renderSetLogicalSize renderer (round width)  (round height)

  preloadResources mgr renderer

  return (renderer, window)

render :: ResourceMgr -> GameState -> (Renderer, Window) -> IO()
render resourceManager shownState (renderer, window) = do
  SDL.showWindow window
  -- resources <- loadNewResources resourceManager shownState renderer
  res <- resources <$> readIORef (unResMgr resourceManager)
  audio   res shownState
  display res shownState (renderer, window)

audio :: Resources -> GameState -> IO()
audio resources shownState = do
  return ()
  -- Start bg music if necessary
  -- playing <- musicPlaying
  -- unless playing $ awhen (bgMusic resources) playMusic

  -- Play object hits
  -- mapM_ (audioObject resources) $ gameObjects shownState

-- audioObject resources object = when (objectHit object) $
--   case objectKind object of
--     (Block _ _) -> playFile (blockHitSnd resources) 3000
--     _           -> return ()

-- preloadResources :: ResourceMgr -> Renderer -> IO()
-- preloadResources mgr rdr = do
--   return ()
--   -- mgr' <- readIORef $ unResMgr mgr
--   -- let res = resources mgr'
--   -- bgImage'    <- maybePreload (bgImage res)
--   -- let res' = res { bgImage    = bgImage' }
--   -- writeIORef (unResMgr mgr) $ mgr' { resources = res' }
--  -- where maybePreload img =
--  --         if isJust (imgTexture img) then return img else preloadImage rdr img

-- preloadImage :: Renderer -> Image -> IO Image
-- preloadImage rdr (Image fp surface _) = do
--   text <- createTextureFromSurface rdr surface
--   return $ Image fp surface (Just text)

-- ** Visual rendering
display :: Resources -> GameState -> (Renderer, Window) -> IO()
display resources shownState (rdr,window) = do 
  SDL.showWindow window

  setRenderDrawColor rdr 0xFF 0 0 0
  renderClear rdr

  displayBackground rdr resources

  displayHUD rdr resources shownState

  let base = (round gameLeft, round gameTop)

  -- Paint overlay message, if any
  paintGeneralMsg base rdr resources (gameStatus (gameInfo shownState))

  -- Paint objects
  -- mapM_ (\o -> render rdr (resources, o) base) (showObjects shownState)
  mapM_ (paintObject (gameLeft, gameTop) rdr resources ) $ gameObjects shownState

  -- Double buffering
  SDL.renderPresent rdr

displayBackground :: Renderer -> Resources -> IO ()
displayBackground screen resources =
  when (isJust (bgImage resources)) $ do
    renderAlignLeft screen (fromJust $ bgImage resources) (0,0)

displayHUD :: Renderer -> Resources -> GameState -> IO()
displayHUD screen resources = paintGeneral screen resources . gameInfo

paintGeneral :: Renderer -> Resources -> GameInfo -> IO ()
paintGeneral screen resources over = void $
  paintGeneralHUD screen resources over

paintGeneralMsg :: (Int, Int) -> Renderer -> Resources -> GameStatus -> IO ()
paintGeneralMsg base screen resources status = awhen (statusMsg status) $ \msg ->
  renderAlignCenter screen (resources, msg, fontColor) base
 where statusMsg :: GameStatus -> Maybe String
       statusMsg GamePlaying     = Nothing
       statusMsg GamePaused      = Just "Paused"
       statusMsg (GameLoading n) = Just ("Level " ++ show n)
       statusMsg GameOver        = Just "GAME OVER!!!"
       statusMsg GameFinished    = Just "You won!!! Well done :)"

paintGeneralHUD :: Renderer -> Resources -> GameInfo -> IO ()
paintGeneralHUD screen resources over = void $ do
  printAlignLeft  screen resources ("Level:  " ++ show (gameLevel  over)) (10,10)
  printAlignLeft  screen resources ("Points: " ++ show (gamePoints over)) (10,50)
  printAlignRight screen resources ("Lives:  " ++ show (gameLives  over)) (10,10)
  -- printAlignRight screen resources ("Time left: " ++ printf "%.2f" (gameTime over)) (10,50)

  -- printLives      screen resources (gameLives over)

-- printLives :: Renderer -> Resources -> Int -> IO ()
-- printLives screen resources lvs = do
--   (w,_) <- renderGetLogicalSize screen
--   printLives' resources (w, 10) screen stdLives lvs
--
-- printLives' resources (px,py) screen 0 _ = return ()
-- printLives' resources (px,py) screen maxLives 0 = do
--   let img = imgSurface $ lifeImg resources
--   let x = px - w - 10
--       y = py
--       w = SDL.surfaceGetWidth  img
--       h = SDL.surfaceGetHeight img
--   renderCopy screen (fromJust $ imgTexture $ lifeImg resources) Nothing (Just (SDL.Rect x y w h))
--   printLives' resources (x, y) screen (maxLives - 1) 0
--
-- printLives' resources (px,py) screen maxLives n = do
--   let img = imgSurface $ ballImg resources
--       x = px - w - 10
--       y = py
--       w = SDL.surfaceGetWidth  img
--       h = SDL.surfaceGetHeight img
--   renderCopy screen (fromJust $ imgTexture $ ballImg resources) Nothing (Just (SDL.Rect x y w h))
--   printLives' resources (x, y) screen (maxLives -1) (n-1)
  -- printAlignRight screen resources ("Lives: " ++ show lvs)   (10,10)

printCentered :: Pos2D -> Renderer -> Resources -> String -> IO ()
printCentered base screen resources msg = void $ do
  (wid,hei) <- SDL.renderGetLogicalSize screen
  -- print (wid, hei)
  let font = resFont resources
  let message = unsafePerformIO $ TTF.renderTextSolid (unFont font) msg fontColor
  -- message' <- return message -- Gfx.zoom message 0.5 0.5 True
  let (dx, dy) = (round *** round) base
      x = dx + (wid - w) `div` 2
      y = dy + (hei - h) `div` 2
      w = SDL.surfaceGetWidth  message - dx
      h = SDL.surfaceGetHeight message - dy
  let txt = unsafePerformIO $ createTextureFromSurface screen message
  -- print (base,x,y,wid,hei, w, h, dx, dy)
  renderCopy screen txt Nothing (Just (SDL.Rect x y w h))

printAlignLeft :: Renderer -> Resources -> String -> (Int, Int) -> IO ()
printAlignLeft screen resources msg (x,y) = void $ do
  let font = unFont $ resFont resources
  let message = unsafeDupablePerformIO $ TTF.renderTextSolid font msg fontColor
  let w = SDL.surfaceGetWidth  message
      h = SDL.surfaceGetHeight message
  let txt = unsafeDupablePerformIO $ createTextureFromSurface screen message
  renderCopy screen txt Nothing (Just (SDL.Rect x y w h))

printAlignRight :: Renderer -> Resources -> String -> (Int, Int) -> IO ()
printAlignRight screen resources msg (x,y) = void $ do
  let font = unFont $ resFont resources
  message <- TTF.renderTextSolid font msg fontColor
  (rightMargin,_) <- renderGetLogicalSize screen
  let w           = SDL.surfaceGetWidth  message
      h           = SDL.surfaceGetHeight message
      rect        = SDL.Rect (rightMargin - x - w) y w h
  txt <- createTextureFromSurface screen message
  renderCopy screen txt Nothing (Just rect)

paintObject :: Pos2D -> Renderer -> Resources -> Object -> IO ()
paintObject base screen resources object =
  case objectKind object of
    (Side {}) -> return ()
    other     -> void $ do let (x,y) = (round *** round) (objectPos object ^+^ base)
                           -- void $ SDLP.filledCircle screen x y ballSize (SDL.Pixel enemyColor)

                           -- Show velocity (not for now):
                           let (vx, vy) = objectVel object
                               (x',y')  = (round *** round) (base ^+^ objectPos object ^+^ (vx, vy))
                           -- void $ SDLP.line screen x y x' y' (SDL.Pixel velColor)
                           return ()


newtype ResourceMgr = ResourceMgr { unResMgr :: IORef ResourceManager }

data ResourceManager = ResourceManager
  { resources       :: Resources
  }

-- printLives :: Renderer -> Resources -> Int -> IO ()
-- printLives screen resources lvs = do
--   (w,_) <- renderGetLogicalSize screen
--   printLives' resources screen (w, 10) stdLives lvs
-- 
-- printLives' resources screen (px,py) 0        _    = return ()
-- printLives' resources screen (px,py) maxLives n    = do
--   let img = (if n < 0 then ballImg else lifeImg) resources
--   let x = px - renderWidth img - 10
--       y = py
--   renderAlignLeft screen img (x, y)
--   printLives' resources screen (x, y) (maxLives - 1) (n-1)

instance Renderizable (Resources, String, Color) where
  renderTexture r = renderTexture r . renderSurface
  renderWidth     = renderWidth  . renderSurface
  renderHeight    = renderHeight . renderSurface

renderSurface :: (Resources, String, Color) -> Surface
renderSurface (resources, msg, color) = unsafePerformIO $
   TTF.renderTextSolid font msg color
 where font = unFont $ resFont resources

-- instance Renderizable (Resources, Object) where
--   renderTexture r  = renderTexture r . objectImage
--   renderSize       = (round *** round) . objectSize . snd
--   render screen (resources, object) base =
--     case objectKind object of
--       (Side {}) -> return ()
--       other     -> renderAlignLeft screen (resources, object) (x,y)
-- 
--     where (x,y) = (round *** round) (objectTopLevelCorner object ^+^ base')
--           base' = (fromIntegral *** fromIntegral) base

-- Partial function. Object has image.
objectImage :: (Resources, Object) -> Image
objectImage (resources, object) = case objectKind object of
  (Paddle {})   -> paddleImg resources
  (Block e _)   -> case e of 
                     3 -> block1Img resources
                     2 -> block2Img resources
                     n -> block3Img resources
  (Ball {})     -> ballImg resources

fontColor :: SDL.Color
fontColor = SDL.Color 228 228 228 255

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

  img      <- loadImage initialBG
  ball     <- loadImage ballImage
  b1       <- loadImage block1Image
  b2       <- loadImage block2Image
  b3       <- loadImage block3Image
  paddle   <- loadImage paddleImage

  -- FIXME: This sould not be here: start playing music
  when (isJust bgM) $ do
    playMusic (fromJust bgM)

  -- Check that all resources have been loaded

  -- Return Nothing or embed in Resources
  let res = do f   <- font
               b   <- blockHit
               i   <- img
               bl  <- ball
               bi1 <- b1
               bi2 <- b2
               bi3 <- b3
               p   <- paddle
               return (Resources f b img bl bi1 bi2 bi3 p bgM)

   -- Some resources did not load
  case res of
    Nothing   -> do return Nothing
    Just res' -> do newMgr <- newIORef (ResourceManager res')
                    return $ Just $ ResourceMgr newMgr

-- preloadResources :: ResourceMgr -> Renderer -> IO ()
preloadResources mgr rdr = void $ do
  modifyIORefM (unResMgr mgr) $ \mgr' -> do

    let res = resources mgr'

    bgImage'    <- maybe (return Nothing) ((Just <$>) . maybePreloadImage rdr) (bgImage res)
    ballImg'    <- maybePreloadImage rdr (ballImg   res)
    block1Img'  <- maybePreloadImage rdr (block1Img res)
    block2Img'  <- maybePreloadImage rdr (block2Img res)
    block3Img'  <- maybePreloadImage rdr (block3Img res)
    paddleImg'  <- maybePreloadImage rdr (paddleImg res)

    let res' = res { bgImage    = bgImage'
                   , ballImg    = ballImg'
                   , block1Img  = block1Img'
                   , block2Img  = block2Img'
                   , block3Img  = block3Img'
                   , paddleImg  = paddleImg'
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

type ImageSpec   = (FilePath, Maybe (Word8, Word8, Word8))
type MusicSpec   = FilePath
type SoundFXSpec = (FilePath, Int)
type FontSpec    = (FilePath, Int)

loadSoundFX :: SoundFXSpec -> IO (Maybe Audio)
loadSoundFX (fp, dur) = loadAudio fp dur

loadImage :: ImageSpec -> IO (Maybe Image)
loadImage (fp, Nothing) = Just <$> Res.loadImage fp
loadImage (fp, mask)    = Just <$> tryLoadImage fp mask Nothing

loadFont :: FontSpec -> IO (Maybe Font)
loadFont (fp, lh) = Just . (\ttf -> Font fp ttf) <$> TTF.openFont fp lh

-- ** Game assets

-- Images
initialBG       :: ImageSpec
initialBG       = ("data/level0.png", Nothing)

ballImage       :: ImageSpec
ballImage       = ("data/ball.png", Nothing)

block1Image     :: ImageSpec
block1Image     = ("data/block1.png", Nothing)

block2Image     :: ImageSpec
block2Image     = ("data/block2.png", Nothing)

block3Image     :: ImageSpec
block3Image     = ("data/block3.png", Nothing)

paddleImage     :: ImageSpec
paddleImage     = ("data/paddleBlu.png", Just (0, 255, 0))

-- Fonts
gameFontSpec    :: FontSpec
gameFontSpec    = ("data/lacuna.ttf", 32)

-- Audio
backgroundMusic :: MusicSpec
backgroundMusic = "data/menu.mp3"

blockHitSFX     :: SoundFXSpec
blockHitSFX     = ("data/196106__aiwha__ding-copyrighted2.wav", 2000)
