module Resource.Values where

-- External imports
import App.Context                      (noResourceContext)
import Data.Char                        (toLower)
import Playground                       (Settings (..))

-- | General settings of the application.
settings :: (Num a) => Settings a
settings = Settings
  { debugTag      = "uk.co.keera.game." ++ fmap toLower (gameName (settings :: Settings Double) noResourceContext)
  , debugMain     = False
  , debugInput    = False
  , debugStats    = False
  , debugMedia    = False
  , debugAudio    = False
  , debugOS       = False
  , debugLogic    = False
  , debugRenderer = False
  , debugVerbose  = False
  , width         = 640
  , height        = 600
  , gameName      = const "Haskanoid"
  , confFilePath = fmap toLower (gameName (settings :: Settings Double) noResourceContext) ++ ".config"
  }
