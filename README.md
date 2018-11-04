# scamp

This was to kick off a haskell roguelike project.  Right now you can move an `@` around the screen, and not even on a grid.  Yay.

## MacOS Mojave (10.14) and later

You will need SDL 2.09 or later to avoid running afoul of Apple's latest shenanigans with OpenGL where you get a blank screen until you move or resize the window.  As of Nov 2018, homebrew only has 2.08, so you may need to `brew install sdl --HEAD`.  I'm unsure whether the fix comes from SDL fixing the OpenGL context or whether it's using the new Metal renderer.  The latter would be nifty to use, but working OpenGL apps would also be nice :)
