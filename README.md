This is a Haskell breakout game implemented using the Functional
Reactive Programming library Yampa.

The game has been created for educational purposes, but tries to feature a
substantial amount of the complexity often found in real arcade games. In
particular:

* SDL 1.2 graphics and sound.

* Multiple input devices (keyboard, mouse, Wiimote Ir).

* Differentiated subsystems for physics/collisions, input,
  rendering/multimedia, logic, etc.

We would like to call on Haskell programmers, game developers and anyone with
an interest in Functional Reactive Programming and/or Game Programming to
review the code, ask for clarification when the code is not clear enough, and
help us improve the game, and the state of FRP/Yampa programming as well.

This game was used to present a Declarative Game Programming tutorial at PPDP
14 (see
http://fplab.bitbucket.org/posts/2014-09-23-declarative-game-programm.html for
details). Slides are available on that website.

# Installation

The game is also available on hackage. All the media resources are included
with the distribution (see LICENCE for redistribution terms).

```
$ cabal update
$ cabal install haskanoid
$ haskanoid
```

I personally recommend using sandboxes (either with cabal or with cabal-dev);

$ cabal update
$ cabal init
$ cabal install haskanoid

To run it with the wiimote, you need to run the program with the special
arguments +RTS -V0. See http://github.com/ivanperez-keera/hcwiid for an
explanation.

# Documentation

To try and make things as clear as possible, the code includes a much haddock
documentation and comments as we could reasonably fit. You can compile
those with:

$ cabal unpack haskanoid     ## Or git clone this-repo
$ cd haskanoid-*
$ cabal init
$ cabal install --only-dependencies
$ cabal configure && cabal haddock --executables --internal

# Related projects

* Yampa (http://github.com/ivanperez-keera/Yampa), the Arrowized Functional
Reactive Programming implementation created by Antony Courtney and Henrik Nilsson.

* hcwiid (http://github.com/ivanperez-keera/hcwiid), a wrapper around
the cwiid library to communicate with Wiimotes.

# Other links

Keera Studios is developing a complete version of this game that runs
on Android. It is **also written in Haskell**.

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

* If you are interested in game design: this game is quite simplistic,
  consisting only of blocks that must be hit. Some ideas for improvements:
  things could fall when a block is hit; some blocks might be ``poisonous'';
  the ball could go faster or slower with time; each level might have
  a timer to complete it; the paddle might tilt to the sides as it moves,
  the inclination depending on the acceleration (or the Wiimote roll).

* If you are interested in FP concepts and game programming, you might want
  to try and think about the following: The resource manager is very
  simplistic: resouce loading happens at level transition, resources are
  unloaded only by the garbage collector as references to them are removed from
  memory. This overly-simplistic work might not scale well for more complex
  games. The problem, once again, of implementing such a subsystem is that it's
  an IO component that may fail and must work asynchronously, yet synchronise
  with a pure game component. 
