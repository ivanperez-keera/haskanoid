[![Version on Hackage](https://img.shields.io/hackage/v/haskanoid.svg)](https://hackage.haskell.org/package/haskanoid)
[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=ivanperez-keera&url=https://github.com/ivanperez-keera/haskanoid&title=Haskanoid&language=&tags=github&category=software)

This is a Haskell breakout game implemented using the Functional
Reactive Programming library Yampa.
<p align="center">
<img src="/screenshots/android.gif?raw=true" alt="Haskanoid Video" style="max-width:100%;">
</p>
The game has been created for educational purposes, but tries to feature a
substantial amount of the complexity often found in real arcade games. In
particular:

* SDL 1.2 graphics and sound.

* Multiple input devices (keyboard, mouse, Wiimote infrared, Kinect).

* Differentiated subsystems for physics/collisions, input,
  rendering/multimedia, logic, etc.

A version of this game using SDL2 has been made available by [Keera
Studios](https://facebook.com/keerastudios) for free on [Google Play for
Android](https://play.google.com/store/apps/details?id=uk.co.keera.games.breakout.beta).
There is a bug that makes the app not close or save the game when you exit it.
I know how to solve it, I just haven't found the time to push that change.
Please, report other bugs of that Android app here. You can follow the progress
of the port for Android on [facebook](http://facebook.com/keerastudios) and
[twitter](http://twitter.com/KeeraStudios).

![Haskanoid on Android](screenshots/android.png?raw=true)

We would like to call on Haskell programmers, game developers and anyone with
an interest in Functional Reactive Programming and/or Game Programming to
review the code, ask for clarification when the code is not clear enough, and
help us improve the game, and the state of FRP/Yampa programming as well.

This game was used to present a Declarative Game Programming tutorial at PPDP
14 (see
http://keera.co.uk/blog/2014/09/24/game-programming-videos-code/ for
details). Slides are linked from that website.

# Installation

The game is available on [hackage](https://hackage.haskell.org/package/SpaceInvaders). All the media resources are included with the distribution (see LICENCE for redistribution terms).  You can install it with*:

```
$ cabal update
$ cabal sandbox init
$ cabal install haskanoid
$ ./.cabal-sandbox/bin/haskanoid
```

If you want to explore the code and possibly make changes, do the following:

```
$ cabal update
$ cabal unpack haskanoid            # or git clone http://github.com/ivanperez-keera/haskanoid
$ cd haskanoid-*                    # Game resources are here
$ cabal sandbox init
$ cabal install
$ ./dist/build/haskanoid/haskanoid
```

To play it with the wiimote, you need to run the program with the special
arguments +RTS -V0. See http://github.com/ivanperez-keera/hcwiid for an
explanation.

*__Additional notes__:

 * Users of GHC 7.8 need to run additional steps. See issue [#2](../../issues/2) for instructions.
 * MacOSX users (or anyone without a wiimote) might want to disable wiimote and kinect support. You can do so with the cabal flags `wiimote` and `kinect`, by running `cabal install --flags="-kinect -wiimote"`.
 * To use of the above installation instructions (with disabled wiimote and kinect support, see bullet point above) you need the following packages:

   * [GHC](https://www.haskell.org/ghc/)
   * [command-line interface for cabal](https://github.com/haskell/cabal/tree/master/cabal-install)
   * SDL, SDL-mixer, SDL-image, SDL-ttf

   On debian/ubuntu, you can install them with:

   ```
   $ sudo apt-get install ghc cabal-install
   $ sudo apt-get install libsdl1.2-dev libsdl-mixer1.2-dev libsdl-image1.2-dev libsdl-ttf2.0-dev
   ```

 * To enable wiimote and kinect support you also need the following packages:

   * CWiid (wiimote)
   * freenect (kinect)

   On debian/ubuntu, you can install them with, respectively:

   ```
   $ sudo apt-get install libcwiid-dev
   $ sudo apt-get install freenect
   ```

## Compilation of GHCJS branch

This game works on browsers (sound and kinect/wiimote are not supported).  To
compile it with GHCJS, you need to have GHCJS installed and compile haskanoid
with the following line:

```
$ cabal install --ghcjs -fghcjs -f-sdl -f-kinect -f-wiimote
```

# Documentation

To try and make things as clear as possible, the code includes a much haddock
documentation and comments as we could reasonably fit. You can compile
those with:

```
$ cabal unpack haskanoid     ## Or git clone this-repo
$ cd haskanoid-*
$ cabal sandbox init
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
  FRP game written in Haskell using Yampa that's available on
[iTunes](https://itunes.apple.com/us/app/magic-cookies/id1244709871) and
[Google
Play](https://play.google.com/store/apps/details?id=uk.co.keera.games.magiccookies&hl=en).

# Hands-on

In the [hands-on file](https://github.com/ivanperez-keera/haskanoid/blob/develop/docs/hands-on.md)
you find ideas to improve haskanoid while focussing on (game)
programming related areas that you might want to dive in deeper. The areas are:
functional (reactive) programming, performance, human-computer interaction and
input/output, and game design.

# Educators

If you find this game attractive and would like to use it to teach functional
programming or other subjects, we'd be very happy to know about it. We can
provide extra material that you can show to students (videos, screenshots,
etc.).
