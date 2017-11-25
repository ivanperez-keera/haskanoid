{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DisplaySDL2
  ( initializeDisplay
  , RenderingCtx
  , initGraphs
  , DisplaySDL2.render
  , module ResourceManagerSDL2
  )
  where

import Control.Arrow             ((***))
import Control.Monad
import Control.Monad.IfElse
import Data.IORef
import Data.Maybe
import FRP.Yampa.VectorSpace
import Game.AssetManager.SDL2    hiding (loadImage)
import Game.Audio.SDL2
import Game.Render.Renderer.SDL2 as Render
import Graphics.UI.SDL           as SDL
import Graphics.UI.SDL.TTF       as TTF

import Constants
import GameState
import Objects
import Resources
import ResourceManagerSDL2

type RenderingCtx = (Renderer, Window)

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

  initAudio

initGraphs :: ResourceMgr -> IO RenderingCtx
initGraphs mgr = do
  -- Create window
  (window,renderer) <- SDL.createWindowAndRenderer (Size width height) [WindowShown, WindowOpengl]
  renderSetLogicalSize renderer width height

  preloadResources mgr renderer

  return (renderer, window)

-- * Rendering and Sound
render :: ResourceMgr -> GameState -> RenderingCtx -> IO ()
render resourceManager shownState (renderer, window) = do
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
audioObject resources object = when (objectHit object) $
  case objectKind object of
    Block -> playFile (blockHitSnd resources)
    _     -> return ()

-- ** Visual rendering
display :: Resources -> GameState -> RenderingCtx -> IO ()
display resources shownState (rdr,window) = do 
  SDL.showWindow window

  setRenderDrawColor rdr 0xFF 0xFF 0 0
  renderClear rdr

  paintBackground rdr resources

  paintInfo rdr resources shownState

  let base = (gameLeft, gameTop)

  -- Paint overlay message, if any
  paintMessage (0, 0) rdr resources (gameStatus (gameInfo shownState))

  -- Paint objects
  mapM_ (\o -> Render.render rdr (resources, o) base) (gameObjects shownState)

  -- Double buffering
  SDL.renderPresent rdr

paintBackground :: Renderer -> Resources -> IO ()
paintBackground screen resources = do
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

paintMessage :: (Int, Int) -> Renderer -> Resources -> GameStatus -> IO ()
paintMessage base screen resources status = do
  awhen (statusMsg status) $ \msg -> do
    (t,s) <- printSolid screen resources msg
    renderAlignCenter screen t base
 where statusMsg :: GameStatus -> Maybe String
       statusMsg GamePlaying     = Nothing
       statusMsg GamePaused      = Just "Paused"
       statusMsg (GameLoading n) = Just ("Level " ++ show n)
       statusMsg GameOver        = Just "GAME OVER!!!"
       statusMsg GameFinished    = Just "You won!!! Well done :)"

instance Renderizable (Resources, Object) Renderer where
  renderTexture r  = renderTexture r . objectImage
  renderSize       = return . (round *** round) . objectSize . snd
  render screen (resources, object) base =
    case objectKind object of
      (Side {}) -> return ()
      other     -> do tex <- renderTexture screen (resources, object)
                      Render.render screen tex (x,y)

    where (x,y) = (round *** round) (objectTopLevelCorner object ^+^ base')
          base' = (fromIntegral *** fromIntegral) base

-- Partial function. Object has image.
objectImage :: (Resources, Object) -> Image
objectImage (resources, object) = case objectKind object of
  Paddle           -> paddleImg resources
  Block            -> let (BlockProps e _) = objectProperties object 
                      in case e of 
                           3 -> block1Img resources
                           2 -> block2Img resources
                           n -> block3Img resources
  Ball             -> ballImg resources
  PowerUp PointsUp -> pointsUpImg resources
  PowerUp LivesUp  -> livesUpImg  resources

-- * Auxiliary function
printSolid :: Renderer -> Resources -> String -> IO (Texture, Surface)
printSolid screen resources msg = do
  let font = unFont $ resFont resources
  message <- TTF.renderTextSolid font msg fontColor
  txt     <- createTextureFromSurface screen message
  return (txt, message)

fontColor :: SDL.Color
fontColor = SDL.Color 228 228 228 255
