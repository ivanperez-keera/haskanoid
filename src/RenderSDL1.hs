{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances        #-}
module RenderSDL1
  where

import Control.Monad
import Data.Word
import Game.AssetManager.SDL1
import Game.Render.Renderer   as Renderer
import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.TTF    as TTF

type RenderingCtx     = ()
type RealRenderingCtx = Surface

getRealRenderingCtx :: RenderingCtx -> IO RealRenderingCtx
getRealRenderingCtx () = getVideoSurface

onRenderingCtx :: (RealRenderingCtx -> IO ()) -> RenderingCtx -> IO ()
onRenderingCtx f ctx = do
  screen <- getRealRenderingCtx ctx
  f screen
  SDL.flip screen

instance Renderizable x Surface => Renderizable (Maybe x) Surface where
  renderTexture surface Nothing  = return Nothing
  renderTexture surface (Just x) = renderTexture surface x
  renderSize Nothing  = return (0, 0)
  renderSize (Just x) = renderSize x

instance Renderizable (res, a) Surface => Renderizable (res, Maybe a) Surface where

  renderTexture _surface (res, Nothing) = return Nothing
  renderTexture surface  (res, Just x)  = renderTexture surface (res, x)
  renderSize (resources, Nothing) = return (0, 0)
  renderSize (resources, Just x)  = renderSize (resources, x)

instance Renderizable (a, Image) Surface where
  renderTexture ctx (resources, img) = renderTexture ctx img
  renderSize (resources, img) = renderSize img

instance Renderizable (TTF.Font, String, (Word8, Word8, Word8)) Surface where

  renderTexture _surface (font, msg, (r,g,b)) = do
    message <- TTF.renderTextSolid font msg (SDL.Color r g b)
    return $ Just message

  renderSize (font, msg, (r, g, b)) = do
    message <- TTF.renderTextSolid font msg (SDL.Color r g b)
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
