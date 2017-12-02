{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances        #-}
module RenderSDL1 where

import Control.Monad
import Data.Word
import Game.AssetManager.SDL1
import Game.Render.Renderer   as Renderer
import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.TTF    as TTF
import Game.Render.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader

type RenderingCtx     = ()
type RealRenderingCtx = Surface

getRealRenderingCtx :: RenderingCtx -> IO RealRenderingCtx
getRealRenderingCtx () = getVideoSurface

instance Renderizable x Surface => Renderizable (Maybe x) Surface where
  renderTexture surface Nothing  = return Nothing
  renderTexture surface (Just x) = renderTexture surface x
  renderSize Nothing  = return (0, 0)
  renderSize (Just x) = renderSize x


instance Renderizable (TTF.Font, Color, String) Surface where

  renderTexture _surface (font, color, msg) = do
    message <- TTF.renderTextSolid font msg color
    return $ Just message

  renderSize (font, color, msg) = do
    message <- TTF.renderTextSolid font msg color
    let w = surfaceGetWidth message
        h = surfaceGetHeight message
    return (w, h)

instance Renderizable Surface Surface where
  renderTexture _surface surface = return $ Just surface
  renderSize surface = do
    screen <- getVideoSurface
    renderingSize screen (Just surface)

instance RenderingContext Surface where
  type RenderingUnit Surface = Maybe Surface
  renderingWidth  screen screen' = return $ maybe 0 surfaceGetWidth screen'
  renderingHeight screen screen' = return $ maybe 0 surfaceGetHeight screen'
  renderingScreenSize screen = renderingSize screen (Just screen)
  renderUnit screen Nothing        (x,y) = return ()
  renderUnit screen (Just surface) (x,y) = void $
    SDL.blitSurface surface Nothing screen $ Just (SDL.Rect x y (-1) (-1))

clearScreen screen (r,g,b) = do
  -- Clear background
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format r g b
  fillRect screen Nothing bgColor

instance Renderizable Image Surface where
  renderTexture ctx img = renderTexture ctx (imgSurface img)
  renderSize img = renderSize (imgSurface img)

instance RenderingContextC Surface where
  type ColorT Surface = Color
  type ImageT Surface = Surface
  type FontT  Surface = TTF.Font
  renderWith f = do
    screen <- ask
    f
    lift $ SDL.flip screen

instance Renderizable (Game.AssetManager.SDL1.Font, Color, String) Surface where

  renderTexture ctx (font, color, msg) = do
    let font' = unFont font
    renderTexture ctx (font', color, msg)

  renderSize (font, color, msg) = do
    let font' = unFont font
    renderSize (font', color, msg)
