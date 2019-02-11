{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP                         #-}
module Display
  ( module Display
  , module ResourceManager
  )
  where

import Control.Monad
import Control.Monad.IfElse       (awhen)
import Control.Monad.Trans.Reader
import Game.Render.Monad
import Game.Render.Renderer.SDL ()
import Game.Resource.Manager.Ref  (prepareAllResources, tryGetResourceSound,
                                   tryGetResourceMusic)
import Game.Audio
import Game.VisualElem
import Game.VisualElem.Render
import Graphics.UI.Align
import Graphics.UI.Collage
import Graphics.UI.SDL        as SDL hiding (flip)
import Playground             (Settings (height, width))
import Playground.SDL         (RenderingCtx)

import Constants
import GameState
import Objects
import ResourceManager

#ifdef sdl2
import Game.Render.Monad.SDL    ()
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
  _screen <- SDL.setVideoMode (width settings) (height settings) 32 [SWSurface]
  SDL.setCaption "Test" ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor False
  SDL.getVideoSurface

#elif sdl2

initGraphs mgr = do
  -- Create window
  (window, renderer) <- SDL.createWindowAndRenderer (Size (width settings) (height settings)) [WindowShown, WindowOpengl]
  renderSetLogicalSize renderer (width settings) (height settings)

  prepareAllResources mgr renderer

  return (renderer, window)
#endif

-- * Rendering and Sound

-- | Loads new resources, renders the game state using SDL, and adjusts music.
render :: ResourceMgr -> GameState -> RenderingCtx -> IO ()
render resourceManager shownState ctx = do
  audio   resourceManager shownState
  display resourceManager shownState ctx

-- ** Audio

audio :: ResourceMgr -> GameState -> IO ()
audio resourceManager shownState = do
  -- Start bg music if necessary
  playing <- musicIsPlaying
  unless playing $ do
    m <- tryGetResourceMusic resourceManager IdBgMusic undefined
    awhen m playMusic

  -- Play object hits
  mapM_ (audioObject resourceManager) $ gameObjects shownState

audioObject :: ResourceMgr -> Object -> IO ()
audioObject resourceManager object = when (objectHit object) $
  case objectKind object of
    Block -> do bhit <- tryGetResourceSound resourceManager IdBlockHitFX undefined
                awhen bhit playSoundFX
    _     -> return ()

-- ** Visual rendering
-- TODO: Uses undefined for rendering context, should get from Main
display :: ResourceMgr -> GameState -> RenderingCtx -> IO ()
display resourceManager shownState = onRenderingCtx $ \ctx ->
  flip runReaderT (resourceManager, undefined, ctx) $ renderVE $ CollageItems $
    concat [ [ bgItem, levelTxt, pointsTxt, livesTxt ], mStatusTxt, objItems ]
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
                 | Just msg <- [ statusMsg status ] ]

    -- Game objects
    objItems = map objItem $ filter (not . isSide) $ gameObjects shownState
    objItem object = CollageItem (VisualImage (objectImage object))
                                 (const (pos, Align HLeft VTop))
      where
        pos      = (round (ox + gameLeft), round (oy + gameTop))
        (ox, oy) = objectTopLevelCorner object

    over   = gameInfo shownState
    status = gameStatus over

statusMsg :: GameStatus -> Maybe String
statusMsg GamePlaying               = Nothing
statusMsg GamePaused                = Just "Paused"
statusMsg (GameLoading _ levelName) = Just ("Level " ++ levelName)
statusMsg GameOver                  = Just "GAME OVER!!!"
statusMsg GameFinished              = Just "You won!!! Well done :)"
statusMsg GameStarted               = Nothing

-- Partial function. Object has image.
objectImage :: Object -> ResourceId
objectImage object = case objectKind object of
  Paddle                -> IdPaddleImg
  Block                 -> let (BlockProps e pu _) = objectProperties object
                           in if pu
                                then IdBlockPuImg -- signals powerup
                                else case e of
                                       3 -> IdBlock1Img
                                       2 -> IdBlock2Img
                                       _ -> IdBlock3Img
  Ball                  -> IdBallImg
  PowerUp PointsUp      -> IdPointsUpImg
  PowerUp LivesUp       -> IdLivesUpImg
  PowerUp MockUp        -> IdMockUpImg
  PowerUp DestroyBallUp -> IdDestroyBallUpImg
