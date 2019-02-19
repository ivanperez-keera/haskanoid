{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DeviceOutput where

-- External imports
import App.Context      (RuntimeContext)
import Control.DeepSeq  (NFData, rnf)
import Playground.SDL   (RenderingCtx)
import Graphics.UI.SDL

-- Internal imports
import Resource.Manager (ResourceMgr)

-- * Environment handling

-- | The rendering environment given by the resource manager,
-- runtime context, and rendering context.
--
-- This is a valid 'GRenderingEnv' which has functions like
-- renderingEnvResourceMgr, renderingEnvRuntimeCtx and
-- renderingEnvRuntimeCtx.
type RenderEnv = (ResourceMgr, RuntimeContext, RenderingCtx)

-- Orphan instaces

-- | NFData instance for Surface.
instance NFData Surface where
  rnf s = s `seq` ()

#ifdef sdl2
-- | NFData instances for Texture.
instance NFData Texture where
  rnf s = s `seq` ()
#endif
