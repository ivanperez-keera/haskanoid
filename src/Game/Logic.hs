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
import FRP.Yampa                            (DTime, Event (Event,NoEvent), SF, after, (&&&),
                                             arr, constant, dSwitch, delay,
                                             dpSwitchB, edge, lMerge, loopPre,
                                             mergeBy, noEvent, returnA, switch,
                                             tag, (-->), (>>>), (^>>))
import FRP.Yampa.Extra                      (futureDSwitch, (>?), (||>))
import Physics.CollisionEngine              (detectCollisions)
import Physics.TwoDimensions.PhysicalObject (Collision (..))

-- Internal imports
import Game.Constants        (gameFinishedDelay, initialLevel, Lives, Points,
                              levelFinishedDelay, loadingDelay, restartDelay,
                              stdLives)
import Game.Levels           (levelInfo, levelName, levels, numLevels,
                              objectSFs)
import Game.Object           (Collisions, ObjectKind (Block, PowerUp),
                              PowerUpKind (..), collisionObjectKind,
                              collisionObjectName, isBlock)
import Game.ObjectSF         (ObjectInput (ObjectInput),
                              ObjectOutput (births, harakiri), ObjectOutputs,
                              ObjectSF, ObjectSFs, PowerUpDef (PowerUpDef),
                              extractObjects, outputObject)
import Game.ObjectSF.Ball    (collisionDestroyBallUpPaddle,
                              collisionLivesUpsPaddle, collisionWithBottom)
import Game.ObjectSF.PowerUp (powerUp)
import Game.State            (GameInfo (..), GameInfoMini (..), GameState (..),
                              GameStatus (..), neutralGameInfo,
                              neutralGameInfoMini, neutralGameState)
import UserInput             (Controller)

-- * General state transitions

-- | Run the game that the player can lose at ('canLose'), until ('switch')
-- there are no more levels ('outOfLevels'), in which case the player has won
-- ('wonGame').
wholeGame :: SF Controller GameState
wholeGame =
   -- restart normal behaviour every time I'm out of levels
   (canLose >? outOfLevels)
   ||>  wonGame

-- | Run the game in which the player is alive, until she runs out of lives
-- ('outOfLives'), in which case the game must be restarted ('restartGame').
canLose :: SF Controller GameState
canLose =
   -- retart normal behaviour every time I'm out of lives
   (runLevel neutralGameInfoMini >? outOfLives)
   ||> restartGame

-- | The game state is over for 3 seconds, then the game is run again
-- ('wholeGame').
restartGame :: SF Controller GameState
restartGame =
  gameOver
  ||> wholeGame

-- | The game state is finished for 4 seconds, then the game is run again
-- ('wholeGame').
wonGame :: SF Controller GameState
wonGame =
  gameFinished
  ||> wholeGame

-- | Set the game state as loading, then executes the level until it is
-- completed. It then switches to the next level (remembering the current lives
-- and points).
--
-- Conditions like finishing the game or running out of lives are detected in
-- 'wholeGame' and 'canLose', respectively.
runLevel :: GameInfoMini -> SF Controller GameState
runLevel gim =
  levelLoading gim
  ||> runLevelLoop
  where
    runLevelLoop = dSwitch
       -- Run level until it is completed
      (playLevelOrPause gim >? isLevelCompleted)
      (\gs ->  switch
                 -- then finish level
                 (levelFinished gs )
                 -- and run the next level
                 (runNextLevel)
      )

-- -- | Detect if the level is completed (ie. if there are no more blocks).
-- isLevelCompleted :: SF GameState (Event ())
-- isLevelCompleted = (not . any isBlock . gameObjects) ^>> edge
-- | Create a 'GameInfoMini' from the 'GameState' while increasing the level
-- number by one (to run the next level). Then call 'runLevel' with the new
-- 'GameInfoMini'.
runNextLevel :: GameState -> SF Controller GameState
runNextLevel gs = runLevel (GameInfoMini lvs lvl pts)
  where
    lvs = gameLives  gi
    lvl = gameLevel  gi + 1
    pts = gamePoints gi

    gi = gameInfo gs

-- * States

-- ** Game Finished

-- | Produces a neutral 'GameFinished' 'GameState'.
gameFinished :: SF a (GameState, Event ())
gameFinished = (gameFinished' >? after gameFinishedDelay ())
  where
    gameFinished' :: SF a GameState
    gameFinished' = constant $
     neutralGameState { gameInfo = neutralGameInfo { gameStatus = GameFinished } }

-- ** Game Over

-- | Produces a neutral 'GameOver' 'GameState'.
gameOver :: SF a (GameState, Event ())
gameOver = (gameOver' >? after restartDelay ())
  where
    gameOver' :: SF a GameState
    gameOver' = constant $
     neutralGameState { gameInfo = neutralGameInfo { gameStatus = GameOver } }

-- ** Level finished

levelFinished :: GameState -> SF a (GameState, Event GameState)
levelFinished gs = levelFinished' >? after levelFinishedDelay gs
  where
    levelFinished' = constant gs

-- ** Level loading

-- | Unconditionally output the game in loading state ('levelLoading') for some
-- time, and then ('after') switch over to unconditionally output a neutral
-- game state with the 'GameLoading' status, forever.
levelLoading :: GameInfoMini -> SF a (GameState, Event ())
levelLoading gim = levelLoading' >? after loadingDelay ()
  where
    levelLoading' = constant $
     neutralGameState { gameInfo = GameInfo { gameStatus = GameLoading (level gim) (levelName $ levelInfo (levels !! (level gim)))
                                            , gameLevel  = level  gim
                                            , gameLives  = lives  gim
                                            , gamePoints = points gim
                                            }
                      }

-- ** Playing and Pausing

-- | Run the normal game.
--
-- NOTE: The code includes a commented piece that detects
-- a request to pause the game. Check out the code to learn how to
-- implement pausing.
playLevelOrPause :: GameInfoMini -> SF Controller GameState
playLevelOrPause gim = playLevel gim
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


-- * Termination criteria

-- | Detect when the last level is finished.
outOfLevels :: SF GameState (Event ())
outOfLevels = arr ((>= numLevels) . gameLevel . gameInfo) >>> edge

-- | Detect when the last life is lost.
outOfLives :: SF GameState (Event ())
outOfLives = arr ((< 0) . gameLives . gameInfo) >>> edge

-- | Detect if the level is completed (ie. if there are no more blocks).
isLevelCompleted :: SF GameState (Event GameState)
isLevelCompleted = proc gs -> do
  over <- edge -< (not . any isBlock . gameObjects ) gs
  let snapshot = over `tag` gs
  returnA -< snapshot

-- * Level playing

-- | Bonus are additional lives and additional points.
type Bonus       = (Int, Int)
-- | Indicates the loss of a live.
type Dead        = Event ()

-- | Run the game, obtain the internal game's running state, and compose it
-- with the more general 'GameState' using the known number of lives and
-- points.
playLevel :: GameInfoMini -> SF Controller GameState
playLevel gim  = play >>> (composeGameState gim)
  where
    -- | Given an initial list of objects, it runs the game, presenting the
    -- output from those objects at all times, notifying any time the ball hits
    -- the floor or catches a power up that destroyes the ball, and of any
    -- additional points and lives gained.
    --
    -- This works as a game loop with a post-processing step. It uses
    -- a well-defined initial accumulator and a traditional feedback
    -- loop.
    --
    -- The internal accumulator holds:
    --    - The last known object outputs (discarded at every iteration).
    --    - The last known collisions (discarded at every iteration).
    --    - The last known points (added to the new ones in every loop iteration).
    play :: SF Controller (ObjectOutputs, Bonus, Dead)
    play = loopPre ([], [], 0) $ proc (userInput, (objs, cols, pts)) -> do

       let inputs = ObjectInput userInput cols (map outputObject objs)

       outputs      <- processMovement lvlIobjs -< inputs
       cols'        <- detectObjectCollisions   -< outputs

       dead         <- checkForDead             -< cols'
       (lvsB, ptsB) <- checkForBonus            -< (cols', pts)

       let objs' = elemsIL outputs

       returnA -< ((objs', (lvsB, ptsB), dead), (objs', cols', ptsB))

     where
       lvlSpec  = levels !! (level gim)
       lvlIobjs = objectSFs $ levelInfo lvlSpec

       checkForDead :: SF Collisions Dead
       checkForDead = proc cols -> do
         hitBottom         <- collisionWithBottom          -< cols
         hitDestroyBallUp  <- collisionDestroyBallUpPaddle -< cols
         returnA -< (lMerge hitBottom hitDestroyBallUp)

       checkForBonus :: SF (Collisions, Int) Bonus
       checkForBonus = proc (cols, pts) -> do
         -- Lives
         hitLivesUps <- collisionLivesUpsPaddle  -< cols
         lvs'        <- loopPre 0 (arr (\(x,y)-> (x + y, x + y) )) -< hitLivesUps
         -- Points
         let pts' = pts + countPoints cols

         returnA -< (lvs', pts')

       -- Parallely apply all object functions
       processMovement :: ObjectSFs -> SF ObjectInput (IL ObjectOutput)
       processMovement objs = dpSwitchB
         objs                                  -- Signal functions
         (noEvent --> arr updateObjectSFs)     -- When necessary, add and remove elements
         (\sfs' f -> processMovement (f sfs')) -- Move along! Move along! (with new state, aka. sfs)
         where
           -- Turn every event carrying a function that transforms the
           -- object signal function list into one function that performs
           -- all the efects in sequence
           updateObjectSFs :: (a, IL ObjectOutput) -> Event (IL ObjectSF -> IL ObjectSF)
           updateObjectSFs (_,oos) =
             foldl (mergeBy (.)) noEvent (es ++ is)
             where
               -- Turn every object that wants to kill itself into
               -- a function that removes it from the list
               es :: [Event (IL ObjectSF -> IL ObjectSF)]
               es = [ harakiri oo `tag` deleteIL k
                    | (k,oo) <- assocsIL oos ]

               -- Turn every object that wants to add itself into
               -- a function that adds it to the list
               is :: [Event (IL ObjectSF -> IL ObjectSF)]
               is = [fmap (insertIL_ . createPowerUp) (births oo)
                    | (k,oo) <- assocsIL oos]

-- | Based on the internal play info, compose the main game state and detect
-- when a live is lost. When that happens, restart this SF with one less life
-- available.
--
-- NOTE: it will be some other SF's responsibility to determine if the player's
-- run out of lives.

-- NOTE (about the code): We need to delay the initial event (if it happened to
-- occur) because, at the moment of switching, it will definitely occur and we
-- will fall in an infinite loop.  Therefore, this dswitch only switches for
-- non-start events.
composeGameState :: GameInfoMini
                 -> SF (ObjectOutputs, Bonus, Dead) GameState
composeGameState gim@(GameInfoMini lvs lvl pts) = futureDSwitch
  (composeGameState' gim)
  (\_ -> composeGameState (GameInfoMini (lvs-1) lvl pts))
  where
    -- | Based on the internal play info, compose the main game state and
    -- propagate whether a live is lost.
    composeGameState' :: GameInfoMini
                      -> SF (ObjectOutputs, Bonus, Dead) (GameState, Dead)
    composeGameState' (GameInfoMini lvs lvl pts) = proc (oos, (lvsB, ptsB), dead) -> do
      -- Compose game state
      objects <- extractObjects -< oos
      let gs = GameState objects (GameInfo GamePlaying (lvs+lvsB) lvl (pts+ptsB))
      returnA -< (gs, dead)

-- ** Put in other modules?

-- From the actual objects, detect which ones collide
detectObjectCollisions :: SF (IL ObjectOutput) Collisions
detectObjectCollisions = extractObjects >>> arr detectCollisions

createPowerUp :: PowerUpDef -> ObjectSF
createPowerUp (PowerUpDef name puk pos sz) = powerUp name puk pos sz

-- * Auxiliary functions

countPoints :: Collisions -> Int
countPoints = sum . map numPoints
  where
    numPoints (Collision cd)
      | hasBall cd    = countBlocks cd
      | hasPaddle cd  = 100 * countPointsUp cd
      | otherwise     = 0
    hasBall       = any (collisionObjectName "ball")
    countBlocks   = length . filter (collisionObjectKind Block)
    hasPaddle     = any (collisionObjectName "paddle")
    countPointsUp = length . filter (collisionObjectKind (PowerUp PointsUp))
