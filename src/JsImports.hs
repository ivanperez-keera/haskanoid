{-# LANGUAGE JavaScriptFFI, CPP #-}

-- | Misc FFI imports from JS.

module JsImports where

import GHCJS.Types

#ifdef __GHCJS__
foreign import javascript unsafe "$r = Date.now();" now :: IO Double
#else
now = error "now: only available from JavaScript"
#endif
