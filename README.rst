=====================
 Emacswrapper README
=====================

About
-----
Emacswrapper (ew) is a wrapper program of Emacs.

It executes Emacs in server mode if no Emacs is running in server mode.
Otherwise, it executes emacsclient and raise the frame of running Emacs.

There is no reason the program is written in Haskell except that
I like Haskell.

It does not support Cygwin Emacs.


Setup
-----
1. Checkout repository.

2. Install emacswrapper.::

   > cd emacswrapper
   > cabal install

3. There are 2 options for emacswrapper to find Emacs executables.

   (a) Put ew(.exe) under Emacs bin directory.
   (b) Add Emacs bin directory to PATH environment variable.
