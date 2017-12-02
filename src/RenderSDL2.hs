{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module RenderSDL2
  where

import Data.Word
import Game.AssetManager.SDL2    hiding (loadImage)
import Game.Render.Renderer.SDL2 as Render
import Graphics.UI.SDL           as SDL
import Graphics.UI.SDL.Surface   as SDL
import Graphics.UI.SDL.TTF       as TTF
import Graphics.UI.SDL.TTF.Types as TTF

type RenderingCtx     = (Renderer, Window)
type RealRenderingCtx = RenderingCtx

onRenderingCtx :: (RenderingCtx -> IO ()) -> RenderingCtx -> IO ()
onRenderingCtx f (rdr, window) = do
  SDL.showWindow window

  f (rdr, window)

  -- Double buffering
  SDL.renderPresent rdr

instance Renderizable (res, a) (Renderer, Window) => Renderizable (res, Maybe a) (Renderer, Window) where

  renderTexture _surface (res, Nothing) = return Nothing
  renderTexture surface  (res, Just x)  = renderTexture surface (res, x)
  renderSize (resources, Nothing) = return (0, 0)
  renderSize (resources, Just x)  = renderSize (resources, x)

instance Renderizable (a, Image) (Renderer, Window) where
  renderTexture ctx (resources, img) = renderTexture ctx img
  renderSize (resources, img) = renderSize img

-- instance Renderizable (TTF.TTFFont, String, (Word8, Word8, Word8)) (Renderer, Window) where
-- 
--   renderTexture renderer (font, msg, (r,g,b)) = do
--     message <- TTF.renderTextSolid font msg (SDL.Color r g b 255)
--     txt     <- createTextureFromSurface renderer message
--     return $ Just txt
-- 
--   renderSize (font, msg, (r, g, b)) = do
--     message <- TTF.renderTextSolid font msg (SDL.Color r g b 255)
--     let w = surfaceGetWidth message
--         h = surfaceGetHeight message
--     return (w, h)

instance Renderizable (Texture, Surface) (Renderer, Window) where
  renderTexture r  = renderTexture r . fst
  renderSize       = renderSize . fst
  render screen (texture, _surface) base =
    Render.render screen texture base
