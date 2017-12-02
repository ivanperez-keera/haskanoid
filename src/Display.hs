{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP                         #-}
module Display
  ( module Display
  , module ResourceManager
  )
  where

import Control.Arrow              ((***))
import Control.Monad
import Control.Monad.IfElse       (awhen)
import Control.Monad.Trans.Reader
import Data.Maybe
import FRP.Yampa.VectorSpace
import Game.Render.Monad
import Game.Resource.Manager.Ref  (prepareAllResources, tryGetResourceAudio)
import Game.VisualElem
import Game.VisualElem.Render
import Graphics.UI.Align
import Graphics.UI.Collage
import Graphics.UI.SDL            as SDL hiding (flip)

import Constants
import GameState
import Objects
import ResourceManager

#ifdef sdl

import Game.Audio.SDL

import RenderSDL1

#elif sdl2
import Game.Audio.SDL2

import Game.Render.Renderer.SDL2 ()
import Game.Render.Monad.SDL2    ()

type RealRenderingCtx = (Renderer, Window)
type RenderingCtx     = (Renderer, Window)
#endif

-- * Initialization

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

  initAudio

initGraphs :: ResourceManager.ResourceMgr -> IO RenderingCtx
#ifdef sdl
initGraphs _mgr = do
  -- Create window
  screen <- SDL.setVideoMode width height 32 [SWSurface]
  SDL.setCaption "Test" ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor False

#elif sdl2

initGraphs mgr = do
  -- Create window
  (window, renderer) <- SDL.createWindowAndRenderer (Size width height) [WindowShown, WindowOpengl]
  renderSetLogicalSize renderer width height

  prepareAllResources mgr renderer

  return (renderer, window)
#endif

-- * Rendering and Sound

-- | Loads new resources, renders the game state using SDL, and adjusts music.
render :: ResourceMgr -> GameState -> RenderingCtx -> IO ()
render resourceManager shownState ctx = do
  audio   resourceManager shownState
  display resourceManager shownState =<< getRealRenderingCtx ctx

-- ** Audio

audio :: ResourceMgr -> GameState -> IO ()
audio resourceManager shownState = do
  -- Start bg music if necessary
  -- playing <- musicPlaying
  -- unless playing $ awhen (bgMusic resources) playMusic

  -- Play object hits
  mapM_ (audioObject resourceManager) $ gameObjects shownState

audioObject :: ResourceMgr -> Object -> IO ()
audioObject resourceManager object = when (objectHit object) $
  case objectKind object of
    Block -> do bhit <- tryGetResourceAudio resourceManager IdBlockHitFX undefined
                awhen bhit playFile
    _     -> return ()

-- ** Visual rendering
-- TODO: Uses undefined for rendering context, should get from Main
display :: ResourceMgr -> GameState -> RealRenderingCtx -> IO ()
display resourceManager shownState = onRenderingCtx $ \ctx -> 
  flip runReaderT (resourceManager, undefined, ctx) $ renderVE $ CollageItems $
    [ bgItem, levelTxt, pointsTxt, livesTxt ] ++ mStatusTxt ++ objItems
  where
    -- Background
    bgItem     = CollageItem (VisualImage IdBgImg)
                             (const ((0, 0), Align HLeft VTop))
    -- HUD
    levelTxt   = CollageItem (VisualText IdGameFont IdGameFontColor ("Level: "  ++ show (gameLevel over)))
                             (const ((10, 10), Align HLeft  VTop))
    pointsTxt  = CollageItem (VisualText IdGameFont IdGameFontColor ("Points: " ++ show (gamePoints over)))
                             (const ((10, 40), Align HLeft  VTop))
    livesTxt   = CollageItem (VisualText IdGameFont IdGameFontColor ("Lives: "  ++ show (gameLives over)))
                             (const ((10, 40), Align HRight VTop))
    -- Game status
    mStatusTxt = [ CollageItem (VisualText IdGameFont IdGameFontColor msg)
                               (const ((0, 0), Align HCenter VCenter))
                 | isJust (statusMsg status), let msg = fromJust (statusMsg status) ]

    -- Game objectsj
    objItems   = map objItem (gameObjects shownState)
    objItem object =
      case objectKind object of
        (Side {}) -> CollageItems []
        other     -> let objPos = objectTopLevelCorner object 
                         pos    = (round *** round) (objPos^+^ (gameLeft, gameTop))
                     in  CollageItem (VisualImage (objectImage object))
                                     (const (pos, Align HLeft VTop))

    over   = gameInfo shownState
    status = gameStatus over


statusMsg :: GameStatus -> Maybe String
statusMsg GamePlaying          = Nothing
statusMsg GamePaused           = Just "Paused"
statusMsg (GameLoading n name) = Just ("Level " ++ name)
statusMsg GameOver             = Just "GAME OVER!!!"
statusMsg GameFinished         = Just "You won!!! Well done :)"
statusMsg GameStarted          = Nothing

-- Partial function. Object has image.
objectImage :: Object -> ResourceId
objectImage object = case objectKind object of
  Paddle                -> IdPaddleImg
  Block                 -> let (BlockProps e pu _) = objectProperties object 
                           in case pu of
                                True  -> IdBlockPuImg -- signals powerup
                                False -> case e of 
                                           3 -> IdBlock1Img
                                           2 -> IdBlock2Img
                                           n -> IdBlock3Img
  Ball                  -> IdBallImg
  PowerUp PointsUp      -> IdPointsUpImg
  PowerUp LivesUp       -> IdLivesUpImg
  PowerUp MockUp        -> IdMockUpImg
  PowerUp DestroyBallUp -> IdDestroyBallUpImg
