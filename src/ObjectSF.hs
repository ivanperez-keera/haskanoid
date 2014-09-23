-- | Objects as signal functions.
--
-- Live objects in the game take user input and the game universe
-- and define their state in terms of that. They can remember what
-- happened (see Yampa's Arrow combinators, which hide continuations),
-- change their behaviour (see switches in Yampa).
--
-- They cannot affect other objects, but they can kill themselves (see
-- 'harakiri'). Should you need to spawn new game elements upon
-- events, you might want to change 'harakiri' to something more
-- general.
module ObjectSF where

import FRP.Yampa

import Objects
import Input
import Data.IdentityList

-- | Objects are defined as transformations that take 'ObjectInput' signals and
-- return 'ObjectOutput' signals.
type ObjectSF = SF ObjectInput ObjectOutput

-- | In order to determine its new output, an object needs to know the user's
-- desires ('userInput'), whether there have been any collisions
-- ('collisions'), and the presence of any pre-existing objects
-- ('knownObjects').
--
-- The reason for depending on 'Collisions' is that objects may ``die''
-- when hit.
--
-- The reason for depending on 'Objects' is that objects may choose to
-- follow other objects.
--
-- TODO: Would it be possible to depend on the specific object sfs internally
-- and remove the explicit 'knownObjects'? I guess so, so long as it's possible
-- to always provide the same input to those SFs that they will have in the
-- game: because they are different instances, we need the exact same input to
-- guarantee the exact same behaviour.
data ObjectInput = ObjectInput
  { userInput    :: Controller
  , collisions   :: Collisions
  , knownObjects :: Objects
  }

-- | What we can see about each live object at each time. It's a
-- snapshot of the object.
data ObjectOutput = ObjectOutput
  { outputObject :: Object   -- ^ The object's state (position, shape, etc.).
  , harakiri     :: Event () -- ^ Whether the object has died (killed itself).
  } 


-- | Handy function to create an object that is currently alive.
livingObject :: Object -> ObjectOutput
livingObject o = ObjectOutput o noEvent

-- | List of identifiable objects. Used to work with dynamic object
-- collections.
type ObjectSFs = IL ObjectSF

extractObjects :: Functor f => SF (f ObjectOutput) (f Object)
extractObjects = arr (fmap outputObject)

-- | A list of object outputs
type ObjectOutputs = [ObjectOutput]

