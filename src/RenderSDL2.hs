module RenderSDL2 where

import Graphics.UI.SDL (Renderer, Window)

type RenderingCtx     = (Renderer, Window)
type RealRenderingCtx = RenderingCtx
