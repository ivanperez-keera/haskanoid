[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=ivanperez-keera&url=https://github.com/ivanperez-keera/haskanoid&title=Haskanoid&language=&tags=github&category=software) 

This is a Haskell breakout game implemented using the Functional
Reactive Programming library Yampa.

The game has been created for educational purposes, but tries to feature a
substantial amount of the complexity often found in real arcade games. In
particular:

* SDL 1.2 graphics and sound.

* Multiple input devices (keyboard, mouse, Wiimote IR, Kinect).

* Differentiated subsystems for physics/collisions, input,
  rendering/multimedia, logic, etc.

![GitHub Logo](screenshots/desktop.png?raw=true)

We would like to call on Haskell programmers, game developers and anyone with
an interest in Functional Reactive Programming and/or Game Programming to
review the code, ask for clarification when the code is not clear enough, and
help us improve the game, and the state of FRP/Yampa programming as well.

This game was used to present a Declarative Game Programming tutorial at PPDP
14 (see
http://fplab.bitbucket.org/posts/2014-09-23-declarative-game-programm.html for
details). Slides are available on that website.

# Installation

The game will also be available on hackage. All the media resources are
included with the distribution (see LICENCE for redistribution terms).  I
personally recommend using sandboxes (either with cabal or with cabal-dev)*:

```
$ cabal update
$ cabal sandbox init                # skip if you are using cabal-dev
$ cabal unpack haskanoid            # or git clone http://github.com/ivanperez-keera/haskanoid
$ cd haskanoid-*                    # Game resources are here
$ cabal install                     # ...or cabal-dev install
$ ./dist/build/haskanoid/haskanoid
```

To play it with the wiimote, you need to run the program with the special
arguments +RTS -V0. See http://github.com/ivanperez-keera/hcwiid for an
explanation.

*__Two additional notes__:

 * Users of GHC 7.8 need to run additional steps. See issue [#2](../../issues/2) for instructions.
 * MacOSX users (or anyone without a wiimote) might want to disable wiimote and kinect support. You can do so with the cabal flags wiimote and kinect, by running cabal install --flags="-kinect -wiimote".

# Documentation

To try and make things as clear as possible, the code includes a much haddock
documentation and comments as we could reasonably fit. You can compile
those with:

```
$ cabal unpack haskanoid     ## Or git clone this-repo
$ cd haskanoid-*
$ cabal init
$ cabal install --only-dependencies
$ cabal configure && cabal haddock --executables --internal
```

# Related projects

* Yampa (http://github.com/ivanperez-keera/Yampa), the Arrowized Functional
Reactive Programming implementation created by Antony Courtney and Henrik Nilsson.

* hcwiid (http://github.com/ivanperez-keera/hcwiid), a wrapper around
the cwiid library to communicate with Wiimotes.

* freenect (https://hackage.haskell.org/package/freenect), bindings to
communicate with kinect devices.

* FRP Collisions (https://github.com/keera-studios/haskell-frp-yampa-physics),
an introductory example of how to do sphere collisions in Yampa.

* Magic Cookies (https://github.com/keera-studios/magic-cookies), a commercial
FRP game written in Haskell using Yampa that's available on Google Play.

# Other links

Keera Studios is developing a complete version of this game that runs on
Android. This version is already available for selected beta testers on Google
Play. It is **also written in Haskell**. You can see their announcement
[here](http://keera.co.uk/blog/2014/11/24/haskell-android-games-adventure-engine-beta-testing/) and
[here](http://keera.co.uk/blog/?p=690),
and followup on their progress on facebook (http://facebook.com/keerastudios)
and twitter (http://twitter.com/KeeraStudios).

# Homework

There are a few obvious ways to improve this code:

* If you are interested in FRP: The physics/collision subsystem,
  which works in three steps (simulation, detection and correction),
  is too naive. Because there is no control no point-of-collision or
  time-of-collision detection, this results in a collision engine that depends
  greatly on the number of frames per second. In FRP there is little control over
  the delta times used for the simulation. Implement a better system.

  * NOTE: Maths are used for physics and collisions all the time. Matrices are
    your friends. Don't reinvent the wheel, just recall freshman algebra.

  * NOTE: I recommend the following introductory book, which also
    has really good pointers: Jason Gregory's Game Engine Architecture.

  * NOTE: when programming such a system, it's incredibly easy to get
    caught up in tiny details that provoke huge increases in the complexity of
    the problem. The kinds of shapes you allow matter. Whether they are concave or
    convex matter. Whether you allow rotation matters. Don't underestimate this
    problem: professional engines such as AndEngine get this wrong as well (see a
    pinball video on Keera Studios' facebook page for details).
	Start small, try to simplify, and grow steady. A don't, don't, don't
    even think of optimising until you have, at least, solved the problem.

* If you are interested in performance: you'll notice that this game has
  growing memory demands.  There's a trivial change to one module that makes
  the game run in constant memory per level (using only 3M).

  See also: http://keera.co.uk/blog/2014/10/15/from-60-fps-to-500/

* If you are interested in game design: this game is quite simplistic,
  consisting only of blocks that must be hit. Some ideas for improvements:
  things could fall when a block is hit; some blocks might be "poisonous";
  the ball could go faster or slower with time; each level might have
  a timer to complete it; the paddle might tilt to the sides as it moves,
  the inclination depending on the acceleration (or the Wiimote roll).

* If you are interested in FP concepts and game programming, you might want
  to try and think about the following: The resource manager is very
  simplistic: resouce loading happens at level transition, resources are
  unloaded only by the garbage collector as references to them are removed from
  memory. This overly-simplistic work might not scale well for complex
  games. The problem, once again, of implementing such a subsystem is that it's
  an IO component that may fail and must work asynchronously, yet synchronise
  with a pure game component. The following are suggested:

  * Implement a loading message, and make the game wait until resources
    are loaded.

  * Make the resource manager asynchronous, so that resources can
    be loaded/unloaded in the background, even before the next
    level starts, and making the display responsive (again).

* If you are interested in Human-Computer Interaction and Input/Output:
  adding support for new devices is really simple. Using a kinect,
  for instance, requires minimal changes (we have code that works with
  the freenect library). But maybe you can connect it to more interesting
  devices, such as mobile phones, or brain sensors, webcams, or the new
  (upcoming) Kinect, which will do eye tracking!

# Educators

If you find this game attractive and would like to use it to teach functional
programming or other subjects, we'd be very happy to know about it. We can
provide extra material that you can show to students (videos, screenshots,
etc.).
