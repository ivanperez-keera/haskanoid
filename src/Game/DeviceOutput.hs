{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | The device output handles the rendering of the game state.
module Game.DeviceOutput where

-- External imports
import Control.Monad             (unless, when)
import Control.Monad.IfElse      (awhen)
import Game.Audio                (musicIsPlaying, playMusic, playSoundFX)
import Game.Resource.Manager.Ref (tryGetResourceMusic, tryGetResourceSound)
import Game.VisualElem           (VisualElem (VisualImage, VisualText))
import Graphics.UI.Align         (Align (Align),
                                  HAlign (HCenter, HLeft, HRight),
                                  VAlign (VCenter, VTop))
import Graphics.UI.Collage       (Collage (CollageItem), collageMapM)
import Playground                (DAlign, displayWithBGColorImage')
import Playground.SDL            (dAlignToAbsPos')

-- Internal imports
import DeviceOutput     (RenderEnv)
import Game.Constants   (gameLeft, gameTop)
import Game.Levels      (bgColor, levels, mBgImage, mBgMusic)
import Game.Object      (Object, ObjectKind (Ball, Block, Paddle, PowerUp),
                         ObjectProperties (BlockProps), PowerUpKind (..),
                         isSide, objectHit, objectKind, objectProperties,
                         objectTopLevelCorner)
import Game.State       (GameState, GameStatus (..), gameInfo, gameLevel,
                         gameLevel, gameLevel, gameLives, gameObjects,
                         gamePoints, gameStatus)
import Resource.Manager (ResourceId (..))

#ifdef sdl
import Graphics.UI.SDL as SDL (enableUnicode, showCursor)
#endif

#ifdef sdl2
import Graphics.UI.SDL.Mouse as SDL (showCursor, CursorToggle (Hide))
import Game.Render.Monad.SDL ()
#endif

-- * Rendering of the game state

-- | Render the game state.
render :: GameState -> RenderEnv -> IO ()
render shownState env = do
  audio   shownState env
  display shownState env

-- ** Audible

audio :: GameState -> RenderEnv -> IO ()
audio shownState env = do

  audioMusic  shownState env
  audioSounds shownState env

-- ** Visual

-- | Display the game state.
display :: GameState -> RenderEnv -> IO ()
display shownState env = do
  -- Hide mouse
#ifdef sdl
  SDL.showCursor False
#elif sdl2
  SDL.showCursor Hide
#endif

  let (cCol, mBgImg, clg) = game shownState
  -- TODO dAlignToAbsPos' ignores the rendering context
  clg' <- collageMapM (dAlignToAbsPos' env) clg
  displayWithBGColorImage' (cCol, mBgImg, clg') env True

-- * Game specific elements

-- ** Audible

-- *** Music
audioMusic :: GameState -> RenderEnv -> IO ()
audioMusic shownState env@(resourceManager, rtCtx, _) = do
  -- Start bg music if necessary
  playing <- musicIsPlaying
  unless playing $ do
    m <- tryGetMusic
    awhen m playMusic
  where
    tryGetMusic | (Just bgM) <- mBgMusic $ levels !! (gameLevel $ gameInfo shownState)
                = tryGetResourceMusic resourceManager bgM rtCtx
                | otherwise
                = return Nothing

-- *** Sounds

audioSounds :: GameState -> RenderEnv -> IO ()
audioSounds shownState env = do
  -- Play object hits
  mapM_ (audioObject env) $ gameObjects shownState

audioObject :: RenderEnv -> Object -> IO ()
audioObject (resourceManager, rtCtx, _) object = when (objectHit object) $
  case objectKind object of
    Block -> do bhit <- tryGetResourceSound resourceManager IdBlockHitFX rtCtx
                awhen bhit playSoundFX
    _     -> return ()

-- ** Visual

game :: GameState -> (ResourceId, Maybe ResourceId, Collage (VisualElem ResourceId) DAlign)
game shownState = (bgColor lvlSpec, mBgImage lvlSpec, mconcat [levelTxt, pointsTxt, livesTxt, mconcat mStatusTxt, mconcat objItems] )
  where
    -- HUD
    levelTxt   = CollageItem (VisualText IdGameFont IdGameFontColor ("Level: "  ++ show (gameLevel over)))
                             ((10, 10), Align HLeft  VTop)
    pointsTxt  = CollageItem (VisualText IdGameFont IdGameFontColor ("Points: " ++ show (gamePoints over)))
                             ((10, 40), Align HLeft  VTop)
    livesTxt   = CollageItem (VisualText IdGameFont IdGameFontColor ("Lives: "  ++ show (gameLives over)))
                             ((10, 40), Align HRight VTop)
    -- Game status
    mStatusTxt = [ CollageItem (VisualText IdGameFont IdGameFontColor msg)
                               ((0, 0), Align HCenter VCenter)
                 | Just msg <- [ statusMsg status ] ]

    -- Game objects
    objItems = map objItem $ filter (not . isSide) $ gameObjects shownState
    objItem object = CollageItem (VisualImage (objectImage object))
                                 (pos, Align HLeft VTop)
      where
        pos      = (round (ox + gameLeft), round (oy + gameTop))
        (ox, oy) = objectTopLevelCorner object

    over    = gameInfo shownState
    status  = gameStatus over
    lvl     = gameLevel over
    lvlSpec = levels !! lvl

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
