{-# LANGUAGE Arrows #-}
-- | This module defines the game as a big Signal Function that transforms a
-- Signal carrying a Input 'Controller' information into a Signal carrying
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
module GamePlay (wholeGame) where

-- External imports
import Control.Applicative ((<$>))
import Data.IdentityList
import Data.List
import Data.Ord.Extra
import Data.Tuple.Utils
import Data.VectorSpace.Extra
import FRP.Yampa
import FRP.Yampa.Extra
import Physics.CollisionEngine
import Physics.TwoDimensions.Collisions
import Physics.TwoDimensions.Dimensions
import Physics.TwoDimensions.PhysicalObjects(Collision(..))
import Debug.Trace

-- Internal imports
import Constants
import GameState
import Input
import Levels
import Objects
import ObjectSF

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
  gamePlay' (initialObjects level) >>> composeGameState lives level pts

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

-- * Game objects
--
-- | Objects initially present: the walls, the ball, the paddle and the blocks.
initialObjects :: Int -> ObjectSFs
initialObjects level = listToIL $
    [ objSideRight 
    , objSideTop
    , objSideLeft
    , objSideBottom
    , objPaddle   
    , objBall
    ]
    ++ map (\p -> objBlock p (blockWidth, blockHeight)) (blockCfgs $ levels!!level)

    
-- *** Ball

-- | Ball
--
-- A ball that follows the paddle until the user fires it
-- ('followPaddleDetectLaunch'), then switches ('switch') over to start
-- bounding around, until it hits the floor ('bounceAroundDetectMiss').
--
objBall :: ObjectSF
objBall = switch followPaddleDetectLaunch   $ \p -> 
          switch (bounceAroundDetectMiss p) $ \_ ->
          objBall
    where
        -- Yampa's edge is used to turn the continuous
        -- signal produced by controllerClick into an
        -- event-carrying signal, only true the instant
        -- the mouse button is clicked.
        followPaddleDetectLaunch = proc oi -> do
            o     <- followPaddle -< oi
            click <- edge         -< controllerClick (userInput oi) 
            returnA -< (o, click `tag` objectPos (outputObject o))

        bounceAroundDetectMiss p = proc oi -> do
            o                <- bouncingBall p initialBallVel -< oi
            miss             <- collisionWithBottom           -< collisions oi
            hitDestroyBallUp <- collisionDestroyBallUpPaddle  -< collisions oi

            let hbod = lMerge miss hitDestroyBallUp
            returnA -< (o, hbod) 

-- | Fires an event when the ball *enters in* a collision with the
-- bottom wall.
--
-- NOTE: even if the overlap is not corrected, 'edge' makes
-- the event only take place once per collision.
collisionWithBottom :: SF Collisions (Event ())
collisionWithBottom = inCollisionWith ("ball", Ball) ("bottomWall", Side) ^>> edge

-- | Fires an event when the ball *enters in* a collision with the
-- bottom wall.
--
-- NOTE: even if the overlap is not corrected, 'edge' makes
-- the event only take place once per collision.
collisionDestroyBallUpPaddle :: SF Collisions (Event ())
collisionDestroyBallUpPaddle = proc cs -> do

  -- Has the powerup been hit?
  let hits :: Collisions
      hits = filter (any (collisionObjectKind (PowerUp DestroyBallUp)) . collisionData) cs

      paddleHits :: Collisions
      paddleHits = filter (any (collisionObjectKind Paddle) . collisionData) hits

      isHit :: Bool
      isHit = not (null $ concatMap collisionData paddleHits)

  dead <- edge -< isHit
  returnA -< dead

-- | Fires an event when a PowerUp LivesUp *enters in* a collision with the
-- paddle.
--
-- NOTE: even if the overlap is not corrected, 'edge' makes
-- the event only take place once per collision.
collisionLivesUpsPaddle :: SF Collisions Int 
collisionLivesUpsPaddle = proc cs -> do

  -- Has the powerup been hit?
  let hits :: Collisions
      hits = filter (any (collisionObjectKind (PowerUp LivesUp)) . collisionData) cs

      paddleHits :: Collisions
      paddleHits = filter (any (collisionObjectKind Paddle) . collisionData) hits

      isHit :: Bool
      isHit = not (null $ concatMap collisionData paddleHits)

  dead <- edge -< isHit
  returnA -< length (concatMap collisionData paddleHits) 

-- | Ball follows the paddle if there is one, and it's out of the screen
-- otherwise). To avoid reacting to collisions, this ball is non-interactive.
followPaddle :: ObjectSF
followPaddle = arr $ \oi ->
  -- Calculate ball position, midway on top of the the paddle
  --
  -- This code allows for the paddle not to exist (Maybe), although that should
  -- never happen in practice.
  let mbPaddlePos = objectPos <$> find isPaddle (knownObjects oi)
      ballPos     = maybe (outOfScreen, outOfScreen)
                          ((paddleWidth/2, - ballHeight) ^+^)
                          mbPaddlePos
  in ObjectOutput (inertBallAt ballPos) noEvent noEvent
  where outOfScreen = -10
        inertBallAt p = Object { objectName           = "ball"
                               , objectKind           = Ball
                               , objectProperties     = BallProps ballWidth
                               , objectPos            = p
                               , objectVel            = (0, 0)
                               , objectAcc            = (0, 0)
                               , objectDead           = False
                               , objectHit            = False
                               , canCauseCollisions   = False
                               , collisionEnergy      = 0
                               }

-- A bouncing ball moves freely until there is a collision, then bounces and
-- goes on and on.
--
-- This SF needs an initial position and velocity. Every time
-- there is a bounce, it takes a snapshot of the point of
-- collision and corrected velocity, and starts again.
--
bouncingBall :: Pos2D -> Vel2D -> ObjectSF
bouncingBall p0 v0 =
  switch progressAndBounce
         (uncurry bouncingBall) -- Somehow it would be clearer like this:
                                -- \(p', v') -> bouncingBall p' v')
 where

       -- Calculate the future tentative position, and
       -- bounce if necessary.
       --
       -- The ballBounce needs the ball SF' input (which has knowledge of
       -- collisions), so we carry it parallely to the tentative new positions,
       -- and then use it to detect when it's time to bounce

       --      ==========================    ============================
       --     -==--------------------->==--->==-   ------------------->==
       --    / ==                      ==    == \ /                    ==
       --  --  ==                      ==    ==  X                     ==
       --    \ ==                      ==    == / \                    ==
       --     -==----> freeBall' ----->==--->==--------> ballBounce -->==
       --      ==========================    ============================
       progressAndBounce = (arr id &&& freeBall') >>> (arr snd &&& ballBounce)

       -- Position of the ball, starting from p0 with velicity v0, since the
       -- time of last switching (or being fired, whatever happened last)
       -- provided that no obstacles are encountered.
       freeBall' = freeBall p0 v0

-- | Detect if the ball must bounce and, if so, take a snapshot of the object's
-- current position and velocity.
--
-- NOTE: To avoid infinite loops when switching, the initial input is discarded
-- and never causes a bounce. This works in this game and in this particular
-- case because the ball never-ever bounces immediately as fired from the
-- paddle.  This might not be true if a block is extremely close, if you add
-- flying enemies to the game, etc.
ballBounce :: SF (ObjectInput, ObjectOutput) (Event (Pos2D, Vel2D))
ballBounce = noEvent --> ballBounce'

-- | Detect if the ball must bounce and, if so, take a snapshot of the object's
-- current position and velocity.
--
-- This does the core of the work, and does not ignore the initial input.
--
-- It proceeds by detecting whether any collision affects
-- the ball's velocity, and outputs a snapshot of the object
-- position and the corrected velocity if necessary.
ballBounce' :: SF (ObjectInput, ObjectOutput) (Event (Pos2D, Vel2D))
ballBounce' = proc (ObjectInput ci cs os, o) -> do
  -- HN 2014-09-07: With the present strategy, need to be able to
  -- detect an event directly after 
  -- ev <- edgeJust -< changedVelocity "ball" cs 
  let ev = maybe noEvent Event (changedVelocity ("ball", Ball) cs)
  returnA -< fmap (\v -> (objectPos (outputObject o), v)) ev

-- | Position of the ball, starting from p0 with velicity v0, since the time of
-- last switching (that is, collision, or the beginning of time --being fired
-- from the paddle-- if never switched before), provided that no obstacles are
-- encountered.
freeBall :: Pos2D -> Vel2D -> ObjectSF
freeBall p0 v0 = proc (ObjectInput ci cs os) -> do

  -- Detect collisions
  let name = "ball"
  let kind = Ball
  let isHit = inCollision (name, kind) cs

  -- Cap speed
  let v = limitNorm v0 maxVNorm

  -- Any free moving object behaves like this (but with
  -- acceleration. This should be in some FRP.NewtonianPhysics
  -- module)
  p <- (p0 ^+^) ^<< integral -< v

  let obj = Object { objectName           = name
                   , objectKind           = Ball 
                   , objectProperties     = BallProps ballWidth
                   , objectPos            = p
                   , objectVel            = v0
                   , objectAcc            = (0, 0)
                   , objectDead           = False
                   , objectHit            = isHit
                   , canCauseCollisions   = True
                   , collisionEnergy      = 1
                   }
  
  returnA -< livingObject obj

-- *** Player paddle

-- | The paddle tries to be in line with the mouse/pointer/controller.
--
-- It has drag, to make the game a bit harder. Take a look at the code if you
-- want to make it move faster or even instantaneously.
--
objPaddle :: ObjectSF
objPaddle = proc (ObjectInput ci cs os) -> do

  -- Detect collisions
  let name = "paddle"
  let kind = Paddle
  let isHit = inCollision (name, kind) cs

  -- Try to get to the mouse position, but with a capped
  -- velocity.

  rec
      -- let v = limitNorm (20.0 *^ (refPosPaddle ci ^-^ p)) maxVNorm
      -- let p = refPosPaddle ci -- (initPosPaddle ^+^) ^<< integral -< v
      let v = 100.00 *^ (refPosPaddle ci ^-^ p)
      p <- (initPosPaddle ^+^) ^<< integral -< v
      -- let p = refPosPaddle ci

  --  Use this code if you want instantaneous movement,
  --  particularly cool with the Wiimote, but remember to cap
  --  the balls velocity or you will get incredibly high
  --  velocities when the paddle hits the ball.
  --
  --  let p = refPosPaddle ci
  --  v <- derivative -< p

  returnA -< livingObject
               Object { objectName           = name
                      , objectKind           = Paddle
                      , objectProperties     = PaddleProps (paddleWidth,paddleHeight)
                      , objectPos            = p
                      , objectVel            = (0,0)
                      , objectAcc            = (0,0)
                      , objectDead           = False
                      , objectHit            = isHit
                      , canCauseCollisions   = True
                      , collisionEnergy      = 0
                      }

-- | Follow the controller's horizontal position, keeping a constant
-- vertical position.
refPosPaddle :: Controller -> Pos2D
refPosPaddle c = (x', yPosPaddle)
    where
        (x, _) = controllerPos c
        x'     = inRange (0, gameWidth - paddleWidth) (x - (paddleWidth/2))

-- | The initial position of the paddle, horizontally centered.
initPosPaddle :: Pos2D
initPosPaddle = ((gameWidth - paddleWidth)/2, yPosPaddle)

-- | The paddle's vertical position, at a reasonable distance from the bottom.
yPosPaddle :: Double
yPosPaddle = gameHeight - paddleMargin

-- *** Blocks

-- | Block SF generator. It uses the blocks's size, position and a number of
-- lives that the block has. The block's position is used for it's unique ID,
-- which means that two simulatenously existing blocks should never have the
-- same position. This is ok in this case because they are static, but would not
-- work if they could move and be created dynamically.
objBlock :: (Pos2D, Int, Maybe (PowerUpKind, AlwaysPowerUp), SignalPowerUp) -> Size2D -> ObjectSF
objBlock ((x,y), initlives, mpuk, spu) (w,h) = proc (ObjectInput ci cs os) -> do

  -- Detect collisions
  let name  = "blockat" ++ show (x,y)
      kind  = Block
      isHit = inCollision (name, kind) cs
  hit   <- edge -< isHit

  -- Must be hit initlives times to disappear
  --
  -- If you want them to "recover" or self-heal with time,
  -- use the following code in place of lives.
  --
  -- recover <- delayEvent 5.0 -< hit
  -- lives <- accumHoldBy (+) 3 -< (hit `tag` (-1) `lMerge` recover `tag` 1) 
  lives <- accumHoldBy (+) initlives -< (hit `tag` (-1)) 
  -- 
  -- let lives = 3 -- Always perfect

  -- Dead if out of lives.
  let isDead = lives <= 0
  dead <- edge -< isDead
  -- let isDead = False -- immortal blocks

  -- If it's hit or dead, does it create a 'powerup'?

  let pukWidthHeight :: PowerUpKind -> (Double, Double)
      pukWidthHeight PointsUp = (pointsUpWidth, pointsUpHeight)
      pukWidthHeight LivesUp = (livesUpWidth, livesUpHeight)
      pukWidthHeight MockUp = (mockUpWidth, mockUpHeight)
      pukWidthHeight DestroyBallUp = (destroyBallUpWidth, destroyBallUpHeight)

  -- create powerup if event happens (hit or dead)
  let createPowerUpF :: String -> PowerUpKind -> Event () -> Event PowerUpDef 
      createPowerUpF idprefix puk event = event `tag` PowerUpDef idprefix puk (x,y) (pukWidthHeight puk)

  -- first event input is hit, second is dead
  let createPowerUpF' :: (String, Maybe (PowerUpKind, AlwaysPowerUp)) -> Event () -> Event () -> Event PowerUpDef
      createPowerUpF' (idprefix, (Just (puk, True)))  hit _    = createPowerUpF idprefix puk hit
      createPowerUpF' (idprefix, (Just (puk, False))) _   dead = createPowerUpF idprefix puk dead
      createPowerUpF' (_, Nothing) _ _ = noEvent

  t <- localTime -< ()

  let createPowerUp = createPowerUpF' (show t, mpuk) hit dead

  returnA -< ObjectOutput 
                Object{ objectName           = name
                      , objectKind           = Block
                      , objectProperties     = BlockProps lives spu (w, h)
                      , objectPos            = (x,y)
                      , objectVel            = (0,0)
                      , objectAcc            = (0,0)
                      , objectDead           = isDead
                      , objectHit            = isHit
                      , canCauseCollisions   = False
                      , collisionEnergy      = 0
                      }
               dead
               createPowerUp

-- *** Powerups
powerUp :: String -> PowerUpKind -> Pos2D -> Size2D -> ObjectSF
powerUp idprefix puk (x,y) (w,h) = proc (ObjectInput ci cs os) -> do

  let name = idprefix ++ "powerup" ++ show (x,y)

  -- Has the powerup been hit?
  let hits :: Collisions
      hits = filter (any (collisionObjectName name). collisionData) cs

      paddleHits :: Collisions
      paddleHits = filter (any (collisionObjectKind Paddle) . collisionData) hits

      bottomHits :: Collisions
      bottomHits = filter (any (collisionObjectName "bottomWall") . collisionData) hits

      isHit :: Bool
      isHit = not (null $ concatMap collisionData paddleHits)
              || not (null $ concatMap collisionData bottomHits)

  -- Time to eliminate? (dead on collision with paddle)
  let isDead = isHit
  dead <- edge -< isDead

  -- Position (always falling)
  p <- (p0 ^+^) ^<< integral -< v

  returnA -< ObjectOutput
               Object { objectName           = name
                      , objectKind           = PowerUp puk
                      , objectProperties     = PowerUpProps (w, h)
                      , objectPos            = p
                      , objectVel            = v
                      , objectAcc            = (0,0)
                      , objectDead           = isDead
                      , objectHit            = isHit
                      , canCauseCollisions   = False
                      , collisionEnergy      = 0
                      }
               dead
               noEvent

  where p0 = (x', y')
        -- Calculate new position
        x' = x + (w / 2)
        y' = y + (h / 2)
        v  = (0, 100)

-- *** Walls

-- | Walls. Each wall has a side and a position.
--
-- NOTE: They are considered game objects instead of having special treatment.
-- The function that turns walls into 'Shape's for collision detection
-- determines how big they really are. In particular, this has implications in
-- ball-through-paper effects (ball going through objects, potentially never
-- coming back), which can be seen if the FPS suddently drops due to CPU load
-- (for instance, if a really major Garbage Collection kicks in.  One potential
-- optimisation is to trigger these with every SF iteration or every rendering,
-- to decrease the workload and thus the likelyhood of BTP effects.
objSideRight  :: ObjectSF
objSideRight  = objWall "rightWall"  RightSide  (gameWidth, 0) 

-- | See 'objSideRight'.
objSideLeft   :: ObjectSF
objSideLeft   = objWall "leftWall"   LeftSide   (0, 0) 

-- | See 'objSideRight'.
objSideTop    :: ObjectSF
objSideTop    = objWall "topWall"    TopSide    (0, 0) 

-- | See 'objSideRight'.
objSideBottom :: ObjectSF
objSideBottom = objWall "bottomWall" BottomSide (0, gameHeight) 

-- | Generic wall builder, given a name, a side and its base
-- position.
objWall :: ObjectName -> Side -> Pos2D -> ObjectSF
objWall name side pos = proc (ObjectInput ci cs os) -> do
   let isHit = inCollision (name, Side) cs
   returnA -< ObjectOutput
                 Object { objectName           = name
                        , objectKind           = Side
                        , objectProperties     = SideProps side 
                        , objectPos            = pos
                        , objectVel            = (0,0)
                        , objectAcc            = (0,0)
                        , objectDead           = False
                        , objectHit            = isHit
                        , canCauseCollisions   = False
                        , collisionEnergy      = 0
                        }
                noEvent
                noEvent
