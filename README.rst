=====================
 Emacswrapper README
=====================

About
-----
Emacswrapper is a wrapper program of Emacs (NTEmacs) on Windows platform.

It executes Emacs (runemacs.exe) in server mode if no Emacs is running
in server mode.
Otherwise, it executes emacsclient (emacsclientw.exe) and raise the frame of
running Emacs.

There is no reason the program is written in Haskell except that
I like Haskell.
You might find something useful in Win32Utils.hs.

It does not support Cygwin Emacs.


Setup
-----
1. Checkout repository.

2. Install emacswrapper.::

   > cd emacswrapper
   > cabal install

3. There are 2 options for emacswrapper to find Emacs executables.

   (a) Put emacswrapper.exe under Emacs bin directory.
   (b) Add Emacs bin directory to PATH environment variable.


TODO
----
1. Conversion from short name to long name.
2. Support cygwin path.
3. Better error handling. (failure, attempt?)
4. Original icon?
