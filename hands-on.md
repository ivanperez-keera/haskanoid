# Hands-on

In this file you find ideas to improve haskanoid while focussing on (game)
programming related areas that you might want to dive in deeper. The areas are:
functional (reactive) programming, performance, human-computer interaction and
input/output, and game design.

## How to:

If you want to work on one of the suggestions stated in this file (or something
else that will improve haskanoid), check whether there is an open issue related
to this feature, if so, check what is the status of this feature, if not, open
an issue related to this feature. Then start contributing by asking questions
or solving the issue and creating a pull request while always referencing this
issue.

## Ideas and Suggestions

### Functional Reactive Programming (FRP)

If you are interested in FRP: The physics/collision subsystem,
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

### Functional Programming (FP) and Game Programming

If you are interested in FP concepts and game programming, you might want
to try and think about the following: The resource manager is very
simplistic: resource loading happens at level transition, resources are
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

### Performance

If you are interested in performance: you'll notice that this game has
growing memory demands.  There's a trivial change to one module that makes
the game run in constant memory per level (using only 3M).

See also: http://keera.co.uk/blog/2014/10/15/from-60-fps-to-500/

### Human-Computer Interaction and Input/Output

* If you are interested in Human-Computer Interaction and Input/Output:
  adding support for new devices is really simple. Using a kinect,
  for instance, requires minimal changes (we have code that works with
  the freenect library). But maybe you can connect it to more interesting
  devices, such as mobile phones, or brain sensors, webcams, or the new
  (upcoming) Kinect, which will do eye tracking!

### Game Design

#### General Thoughts

If you are interested in game design: this game is quite simplistic,
consisting only of blocks that must be hit. Some ideas for improvements:
things could fall when a block is hit; some blocks might be "poisonous";
the ball could go faster or slower with time; each level might have
a timer to complete it; the paddle might tilt to the sides as it moves,
the inclination depending on the acceleration (or the Wiimote roll).


#### Feature Ideas created for ZuriHac2018
by Christina Zeller

* NOTE: The unsorted tasks vary in complexity and the tasks are not at all described in detail.
  Don't get (too) frustrated but ask me for hints or how to start.

- Feature New Levels:
  Create new levels that are beautiful to watch and fun to play.
- Feature Obstacles:
  Add a new level that includes obstacles.
  Obstacles can be seen as a variation of blocks that cannot be destroyed.
  Make sure that the level is still finished when all destroyable blocks are removed.
- Feature Changing Block Behaviour:
  - Store the catch of a power up in the game state.
  - Change the behaviour of blocks after catching a power up
    (e.g., blocks die at next contact/hit with ball)
- Feature Finish:
  Catching this power up let you finish the level without clearing the screen.
- Feature Restart:
  Catching this power up will returns all blocks of the level to the screen.
- Feature Paddle Size:
  Catching this power up will enlarge and/or shrink the paddle.
- Feature Catch Words (homage: [Bubble Bobble 'EXTEND'](https://en.wikipedia.org/wiki/Bubble_Bobble)):
  Catch power ups in a specific order and earn points.
  - Change the amount of points that will be gathered from a power up.
  - Add a new power up.
  - Store the catch of a power up in the game state.
  - Check whether two successive caught power ups are the same / different.
  - Only give bonus if power ups are caught in a certain order (e.g., forming a word).
- Feature Wormholes:
  Lets the ball jump/move to another position on the screen.
- Feature Get Knowledge:
  Catch this power up and see which other blocks contain power ups.

# Confused?
Don't panic! Let's talk and find a solution. ;-)
