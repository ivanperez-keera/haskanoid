-- | The state of the game during execution. It has two
-- parts: general info (level, points, etc.) and
-- the actual gameplay info (objects).
--
-- Because the game is always in some running state
-- (there are no menus, etc.) we assume that there's
-- always some gameplay info, even though it can be
-- empty.
module Game.State where

-- Internal imports
import Game.Constants (Level, Lives, Points, initialLevel, initialPoints,
                       stdLives)
import Game.Object    (Objects)

-- | The running state is given by a bunch of 'Objects' and the current general
-- 'GameInfo'. The latter contains info regarding the current level, the number
-- of points, etc.
--
-- Different parts of the game deal with these data structures.  It is
-- therefore convenient to group them in subtrees, even if there's no
-- substantial difference betweem them.
data GameState = GameState
  { gameObjects :: Objects
  , gameInfo    :: GameInfo
  }

-- | The gameinfo tells us the current game state (running, paused, etc.)
-- and general information, in this case, the number of lives, the level
-- and the points.
--
-- Since this info is then presented together to the users in a top panel, it
-- is convenient to give this product of values a proper name.
data GameInfo = GameInfo
  { gameStatus :: GameStatus
  , gameLives  :: Lives
  , gameLevel  :: Level
  , gamePoints :: Points
  }

data GameInfoMini = GameInfoMini
  { lives  :: Lives
  , level  :: Level
  , points :: Points
  }

-- | Possible actual game statuses. The game is always in one of these.
-- Interaction and presentation depend on this. Yampa switches are
-- used to jump from one to another, and the display module
-- changes presentation depending on the status.
data GameStatus = GamePlaying
                | GamePaused
                | GameLoading Level String
                | GameOver
                | GameFinished
                | GameStarted
 deriving Eq

-- | Initial (default) game state.
neutralGameState :: GameState
neutralGameState = GameState
  { gameObjects = []
  , gameInfo    = neutralGameInfo
  }

-- | Initial (default) game info (no points, no lives, no level).
neutralGameInfo :: GameInfo
neutralGameInfo = GameInfo
  { gameStatus = GameStarted
  , gameLevel  = 0
  , gameLives  = 0
  , gamePoints = 0
  }

neutralGameInfoMini :: GameInfoMini
neutralGameInfoMini = GameInfoMini
  { lives  = stdLives
  , level  = initialLevel
  , points = initialPoints
  }
