module FRP.Extra.Yampa where

import Control.Arrow
import FRP.Yampa
import FRP.Yampa.Event

-- Auxiliary Yampa stuff

-- holdWhen behaves normally, outputting only the b, when the second value
-- is false, and it holds the last known value when the value is True. 
holdWhen :: b -> SF a (b,Bool) -> SF a b
holdWhen b_init sf = sf >>> holdOutput >>> hold b_init
 where holdOutput = arr (\(b,discard) -> if discard then noEvent else Event b)

-- Given an occasional producer of functions
-- and a source of info, apply the functions when they
-- exist
mergeApply :: SF a b -> SF a (Event (b -> b)) -> SF a b
mergeApply sf1 sf2 =
  (sf1 &&& sf2) >>> (arr (\(b,ef) -> event b ($ b) ef))

mergeApply' :: SF a (b, Event (b -> b)) -> SF a b
mergeApply' sf1 = sf1 >>> (arr (\(b,ef) -> event b ($ b) ef))

rRestart :: SF a (b, Event c) -> SF a b
rRestart sf = r
  where r = switch sf (const r)

futureSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
futureSwitch sf cont = switch (sf >>> (arr id *** notYet)) cont

futureDSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
futureDSwitch sf cont = dSwitch (sf >>> (arr id *** notYet)) cont

boolToEvent :: Bool -> a -> Event a
boolToEvent True = Event
boolToEvent _    = \_ -> noEvent
{-# INLINE boolToEvent #-}

