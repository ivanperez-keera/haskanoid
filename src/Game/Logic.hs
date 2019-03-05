{-# LANGUAGE Arrows #-}
-- | This module defines the game as a big Signal Function that transforms a
-- Signal carrying a UserInput 'Controller' information into a Signal carrying
-- 'GameState'.
--
-- There is no randomness in the game, the only input is the user's.
-- 'Controller' is an abstract representation of a basic input device with
-- position information and a /fire/ button.
--
-- The output is defined in 'GameState', and consists of basic information
-- (points, current level, etc.) and a universe of objects.
--
-- Objects are represented as Signal Functions as well ('ObjectSF'). This
-- allows them to react to user input and change with time.  Each object is
-- responsible for itself, but it cannot affect others: objects can watch
-- others, depend on others and react to them, but they cannot /send a
-- message/ or eliminate other objects. However, if you would like to
-- dynamically introduce new elements in the game (for instance, falling
-- powerups that the player must collect before they hit the ground) then it
-- might be a good idea to allow objects not only to /kill themselves/ but
-- also to spawn new object.
--
-- This module contains three sections:
--
--   - A collection of general game SFs. These determine how the game
--   transitions from one state to another based on different internal events
--   (running out of lives, finishing a level, etc.)
--
--   - A collection of gameplay SFs, which control the core game loop, carry
--   out collision detection, add points, etc.
--
--   - One SF per game object. These define the elements in the game universe,
--   which can observe other elements, depend on user input, on previous
--   collisions, etc.
--
-- You may want to read the basic definition of 'GameState', 'Controller' and
-- 'ObjectSF' before you attempt to go through this module.
--
module Game.Logic where -- (wholeGame) where

-- External imports
import Data.IdentityList                    (IL, assocsIL, deleteIL, elemsIL,
                                             insertIL_)
import FRP.Yampa                            (DTime, Event (NoEvent), SF, after,
                                             arr, dSwitch, delay, dpSwitchB,
                                             edge, lMerge, loopPre, mergeBy,
                                             noEvent, returnA, switch, tag,
                                             (&&&), (-->), (>>>), (^>>))
import FRP.Yampa.Extra                      (futureDSwitch)
import Physics.CollisionEngine              (detectCollisions)
import Physics.TwoDimensions.PhysicalObject (Collision (..))

-- Internal imports
import Game.Constants        (initialLevel, levelFinishedDelay, loadingDelay,
                              stdLives)
import Game.Levels           (initialState, levelName, levels, numLevels)
import Game.Objects          (Collisions, ObjectKind (Block, PowerUp),
                              PowerUpKind (..), collisionObjectKind,
                              collisionObjectName, isBlock)
import Game.ObjectSF         (ObjectInput (ObjectInput),
                              ObjectOutput (births, harakiri), ObjectOutputs,
                              ObjectSF, ObjectSFs, PowerUpDef (PowerUpDef),
                              extractObjects, outputObject)
import Game.ObjectSF.Ball    (collisionDestroyBallUpPaddle,
                              collisionLivesUpsPaddle, collisionWithBottom)
import Game.ObjectSF.PowerUp (powerUp)
import Game.State            (GameInfo (..), GameState (..), GameStatus (..),
                              neutralGameInfo, neutralGameState)
import UserInput             (Controller)

-- * General state transitions

-- | Run the game that the player can lose at ('canLose'), until ('switch')
-- there are no more levels ('outOfLevels'), in which case the player has won
-- ('wonGame').
wholeGame :: SF Controller GameState
wholeGame = switch
   -- restart normal behaviour every time I'm out of lives
   (canLose >>> (arr id &&& outOfLevels))
   (\_ -> wonGame)

-- | Detect when the last level is finished.
outOfLevels :: SF GameState (Event ())
outOfLevels = arr ((>= numLevels) . gameLevel . gameInfo) >>> edge

-- | Run the game in which the player is alive, until she runs out of lives
-- ('outOfLives'), in which case the game must be restarted ('restartGame').
canLose :: SF Controller GameState
canLose = switch
   -- retart normal behaviour every time I'm out of lives
   (gameAlive >>> (arr id &&& outOfLives))
   (\_ -> restartGame)

-- | Detect when the last life is lost.
outOfLives :: SF GameState (Event ())
outOfLives = arr ((< 0) . gameLives . gameInfo) >>> edge

-- | The game state is over for 3 seconds, then the game is run again
-- ('wholeGame').
restartGame :: SF Controller GameState
restartGame = switch
  (gameOver &&& after 3 ()) (\_ -> wholeGame)

-- | Produces a neutral 'GameOver' 'GameState'.
gameOver :: SF a GameState
gameOver = arr $ const $
 neutralGameState { gameInfo = neutralGameInfo { gameStatus = GameOver } }

-- | The game state is finished for 4 seconds, then the game is run again
-- ('wholeGame').
wonGame :: SF Controller GameState
wonGame = switch
  (gameFinished &&& after 4 ()) (\_ -> wholeGame)

-- | Produces a neutral 'GameFinished' 'GameState'.
gameFinished :: SF a GameState
gameFinished = arr $ const $
 neutralGameState { gameInfo = neutralGameInfo { gameStatus = GameFinished } }

-- | Run the game from the beginning (no points, max lives, etc.).
--
-- Load the first level.
--
gameAlive :: SF Controller GameState
gameAlive = runLevel stdLives initialLevel 0
  -- loadLevel stdLives initialLevel loadingDelay
  -- (gameWithLives stdLives initialLevel)

-- ** Level loading

-- | Set the game state as loading for a few seconds, then start the actual
-- game. Uses 'loadLevel', passing the game SF ('gameWithLives') as
-- continuation.
runLevel :: Int -> Int -> Int -> SF Controller GameState
runLevel lives level pts = loadLevel lives level pts loadingDelay
 (gameWithLives lives level pts)

-- | Unconditionally output the game in loading state ('levelLoading') for some
-- time, and then ('after') switch over to the given continuation.
--
-- The given arguments are the lives, the level, the points, the time to stay
-- loading the game and the continuation.
loadLevel :: Int -> Int -> Int -> DTime -> SF a GameState -> SF a GameState
loadLevel lives level pts time next = switch
  --
  (levelLoading lives level pts &&& after time ())
  (\_ -> next)

-- | Unconditionally output a neutral game state with the 'GameLoading' status,
-- forever.
levelLoading :: Int -> Int -> Int -> SF a GameState
levelLoading lvs lvl pts = arr $ const $
  neutralGameState { gameInfo = GameInfo { gameStatus = GameLoading lvl (levelName (levels !! lvl))
                                         , gameLevel  = lvl
                                         , gameLives  = lvs
                                         , gamePoints = pts
                                         }
                   }

-- | Start the game at a given level, with a given number of lives.
--
-- It executes the normal gameplay until the level is completed.
-- It then switches to the next level (remembering the current
-- lives and points).
--
-- Conditions like finishing the game or running out of lives are
-- detected in 'wholeGame' and 'canLose', respectively.
--
gameWithLives :: Int -> Int -> Int -> SF Controller GameState
gameWithLives numLives level pts = dSwitch
  -- Run normal game until level is completed
  (proc ctrl -> do
     gState     <- gamePlayOrPause numLives level pts -< ctrl
     gCompleted <- isLevelCompleted'                  -< gState
     returnA -< (gState, gCompleted `tag` gState)
  )

  -- Take last game state, extract basic info, and load the next level
  (\g -> let level' = level + 1
             lives' = gameLives  $ gameInfo g
             pts    = gamePoints $ gameInfo g
         in runLevel lives' level' pts)
  where
    isLevelCompleted' = isLevelCompleted >>> delay levelFinishedDelay NoEvent

-- | Detect if the level is completed (ie. if there are no more blocks).
isLevelCompleted :: SF GameState (Event ())
isLevelCompleted = (not . any isBlock . gameObjects) ^>> edge

-- ** Pausing

-- | Run the normal game.
--
-- NOTE: The code includes a commented piece that detects
-- a request to pause the game. Check out the code to learn how to
-- implement pausing.
gamePlayOrPause :: Int -> Int -> Int -> SF Controller GameState
gamePlayOrPause lives level pts = gamePlay lives level pts
--  ((arr id) &&& (pause undefined (False --> isPaused) (mainLoop lives level)))
--  >>> pauseGeneral
--
-- isPaused :: SF Controller Bool
-- isPaused = arr controllerPause
--
-- pauseGeneral :: SF (Controller, GameState) GameState
-- pauseGeneral = proc (c, g) -> do
--   let isPause = controllerPause c
--   let o       = gameInfo g
--   returnA -< if isPause
--                 then g { gameInfo = o { gameStatus = GamePaused } }
--                 else g

-- * Gameplay

-- | Run the game, obtain the internal game's running state, and compose it
-- with the more general 'GameState' using the known number of lives and
-- points.
gamePlay :: Int -> Int -> Int -> SF Controller GameState
gamePlay lives level pts =
  gamePlay' (initialState levelSpec) >>> composeGameState lives level pts
  where
    levelSpec = levels !! level

-- | Based on the internal gameplay info, compose the main game state and
-- detect when a live is lost. When that happens, restart this SF
-- with one less life available.
--
-- NOTE: it will be some other SF's responsibility to determine if the player's
-- run out of lives.

-- NOTE (about the code): We need to delay the initial event (if it happened to
-- occur) because, at the moment of switching, it will definitely occur and we
-- will fall in an infinite loop.  Therefore, this dswitch only switches for
-- non-start events.
composeGameState :: Int -> Int -> Int
                 -> SF (ObjectOutputs, Int, Event (), Int) GameState
composeGameState lives level pts = futureDSwitch
  (composeGameState' lives level pts)
  (\_ -> composeGameState (lives-1) level pts)

-- | Based on the internal gameplay info, compose the main game state and
-- detect when a live is lost. When that happens, keep the last known game
-- state.
composeGameState' :: Int -> Int -> Int
                  -> SF (ObjectOutputs, Int, Event (), Int) (GameState, Event GameState)
composeGameState' lives level pts = proc (oos, lives', dead, points) -> do
  -- Compose game state
  objects <- extractObjects -< oos
  let lives'' = lives + lives'
  let general = GameState objects
                          (GameInfo GamePlaying lives'' level (pts+points))

  -- Detect death
  let lastGeneral = dead `tag` general

  returnA -< (general, lastGeneral)


-- ** Game with partial state information

-- | Given an initial list of objects, it runs the game, presenting the output
-- from those objects at all times, notifying any time the ball hits the floor,
-- and and of any additional points made.
--
-- This works as a game loop with a post-processing step. It uses
-- a well-defined initial accumulator and a traditional feedback
-- loop.
--
-- The internal accumulator holds:
--
--    - The last known object outputs (discarded at every iteration).
--
--    - The last known collisions (discarded at every iteration).
--
--    - The last known points (added to the new ones in every loop iteration).
--
gamePlay' :: ObjectSFs -> SF Controller (ObjectOutputs, Int, Event (), Int)
gamePlay' objs = loopPre ([], [], 0) $ proc (userInput, (objs, cols, pts)) -> do

   let inputs = ObjectInput userInput cols (map outputObject objs)

   outputs           <- processMovement objs         -< inputs
   cols'             <- detectObjectCollisions       -< outputs

   hitBottom         <- collisionWithBottom          -< cols'
   hitDestroyBallUp  <- collisionDestroyBallUpPaddle -< cols'
   let hbod = lMerge hitBottom hitDestroyBallUp

   hitLivesUps       <- collisionLivesUpsPaddle      -< cols'
   lvs'              <- loopPre 0 (arr (\(x,y)-> (x + y, x + y) )) -< hitLivesUps

   let objs' = elemsIL outputs
       pts'  = pts + countPoints cols'

   returnA -< ((objs', lvs', hbod, pts'), (objs', cols', pts'))

 where

       -- Parallely apply all object functions
       processMovement :: ObjectSFs -> SF ObjectInput (IL ObjectOutput)
       processMovement objs = dpSwitchB
         objs                                  -- Signal functions
         (noEvent --> arr suicidalSect)        -- When necessary, remove all elements that must be removed
         (\sfs' f -> processMovement (f sfs')) -- Move along! Move along! (with new state, aka. sfs)

       suicidalSect :: (a, IL ObjectOutput) -> Event (IL ObjectSF -> IL ObjectSF)
       suicidalSect (_,oos) =
         -- Turn every event carrying a function that transforms the
         -- object signal function list into one function that performs
         -- all the efects in sequence
         foldl (mergeBy (.)) noEvent (es ++ is)

         -- Turn every object that wants to kill itself into
         -- a function that removes it from the list
         where es :: [Event (IL ObjectSF -> IL ObjectSF)]
               es = [ harakiri oo `tag` deleteIL k
                    | (k,oo) <- assocsIL oos ]

               is :: [Event (IL ObjectSF -> IL ObjectSF)]
               is = [fmap (insertIL_ . createPowerUp) (births oo)
                    | (k,oo) <- assocsIL oos]

       -- From the actual objects, detect which ones collide
       detectObjectCollisions :: SF (IL ObjectOutput) Collisions
       detectObjectCollisions = extractObjects >>> arr detectCollisions

       -- Count-points
       countPoints :: Collisions -> Int
       countPoints = sum . map numPoints
         where numPoints (Collision cd)
                  | hasBall cd    = countBlocks cd
                  | hasPaddle cd  = 100 * countPointsUp cd
                  | otherwise     = 0
               hasBall       = any (collisionObjectName "ball")
               countBlocks   = length . filter (collisionObjectKind Block)
               hasPaddle     = any (collisionObjectName "paddle")
               countPointsUp = length . filter (collisionObjectKind (PowerUp PointsUp))

       -- Create powerup
       createPowerUp :: PowerUpDef -> ObjectSF
       createPowerUp (PowerUpDef string puk pos sz) = powerUp string puk pos sz
