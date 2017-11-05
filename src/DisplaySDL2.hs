{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module DisplaySDL2
  ( module DisplaySDL2
  , module ResourceManagerSDL2
  )
  where

import           Control.Arrow              ((***))
import           Control.Monad
import           Control.Monad.IfElse
import           Data.IORef
import           Data.Maybe
import           FRP.Yampa.VectorSpace
import           Game.AssetManager.SDL2     hiding (loadImage)
import           Game.Render.SDL2
import           Graphics.UI.SDL            as SDL
import           Graphics.UI.SDL.Surface    as SDL
import qualified Graphics.UI.SDL.TTF        as TTF
import           System.IO.Unsafe

-- import Audio
import Constants
import GameState
import Objects
import Resources
import ResourceManagerSDL2
import Physics.TwoDimensions.Dimensions

type RenderingCtx = (Renderer, Window)

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

  -- initAudio

initGraphs :: ResourceMgr -> IO RenderingCtx
initGraphs mgr = do
  -- Create window
  (window,renderer) <- SDL.createWindowAndRenderer (Size width height) [WindowShown]
  renderSetLogicalSize renderer width height

  preloadResources mgr renderer

  return (renderer, window)

-- * Rendering and Sound

render :: ResourceMgr -> GameState -> RenderingCtx -> IO ()
render resourceManager shownState (renderer, window) = do
  SDL.showWindow window
  -- resources <- loadNewResources resourceManager shownState renderer
  res <- resources <$> readIORef (unResMgr resourceManager)
  audio   res shownState
  display res shownState (renderer, window)

-- * Audio

audio :: Resources -> GameState -> IO ()
audio resources shownState = do
  -- Start bg music if necessary
  -- playing <- musicPlaying
  -- unless playing $ awhen (bgMusic resources) playMusic

  -- Play object hits
  mapM_ (audioObject resources) $ gameObjects shownState

audioObject :: Resources -> Object -> IO ()
audioObject resources object = return ()
--   case objectKind object of
--     (Block _ _) -> playFile (blockHitSnd resources) 3000
--     _           -> return ()

-- preloadResources :: ResourceMgr -> Renderer -> IO ()
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
display :: Resources -> GameState -> RenderingCtx -> IO ()
display resources shownState (rdr,window) = do 
  SDL.showWindow window

  setRenderDrawColor rdr 0xFF 0 0 0
  renderClear rdr

  paintBackground rdr resources

  paintInfo rdr resources shownState

  let base = (gameLeft, gameTop)

  -- Paint overlay message, if any
  paintMessage base rdr resources (gameStatus (gameInfo shownState))

  -- Paint objects
  -- mapM_ (\o -> render rdr (resources, o) base) (showObjects shownState)
  mapM_ (paintObject (gameLeft, gameTop) rdr resources ) $ gameObjects shownState

  -- Double buffering
  SDL.renderPresent rdr

paintBackground :: Renderer -> Resources -> IO ()
paintBackground screen resources =
  when (isJust (bgImage resources)) $
    renderAlignLeft screen (fromJust $ bgImage resources) (0,0)

paintInfo :: Renderer -> Resources -> GameState -> IO ()
paintInfo screen resources state = void $ do
  (txt, message) <- printSolid screen resources ("Level:  " ++ show (gameLevel  $ gameInfo state))
  renderAlignLeft screen txt (10,10)

  (txt, message) <- printSolid screen resources ("Points: " ++ show (gamePoints $ gameInfo state))
  renderAlignLeft screen txt (10,50)

  (txt, message) <- printSolid screen resources ("Lives:  " ++ show (gameLives  $ gameInfo state))
  renderAlignRight screen txt (10,10)

  -- printAlignRight screen resources ("Time left: " ++ printf "%.2f" (gameTime over)) (10,50)
  -- printLives      screen resources (gameLives over)

paintMessage :: (Int, Int) -> Renderer -> Resources -> GameStatus -> IO ()
paintMessage base screen resources status = awhen (statusMsg status) $ \msg ->
  renderAlignCenter screen (resources, msg, fontColor) base
 where statusMsg :: GameStatus -> Maybe String
       statusMsg GamePlaying     = Nothing
       statusMsg GamePaused      = Just "Paused"
       statusMsg (GameLoading n) = Just ("Level " ++ show n)
       statusMsg GameOver        = Just "GAME OVER!!!"
       statusMsg GameFinished    = Just "You won!!! Well done :)"
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

-- -- Partial function. Object has image.
-- objectImage :: (Resources, Object) -> Image
-- objectImage (resources, object) = case objectKind object of
--   (Paddle {})   -> paddleImg resources
--   (Block e _)   -> case e of 
--                      3 -> block1Img resources
--                      2 -> block2Img resources
--                      n -> block3Img resources
--   (Ball {})     -> ballImg resources

-- * Auxiliary function
printSolid :: Renderer -> Resources -> String -> IO (Texture, Surface)
printSolid screen resources msg = do
  let font = unFont $ resFont resources
  message <- TTF.renderTextSolid font msg fontColor
  txt     <- createTextureFromSurface screen message
  return (txt, message)

fontColor :: SDL.Color
fontColor = SDL.Color 228 228 228 255
