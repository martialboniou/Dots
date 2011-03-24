Installation
============

All users
---------

As packages will be fetched on the Internet then installed and configured, you only need to byte-compile the ~/.emacs.d/lisp directory and the available subdirectories by hand. **First, add the shell script** `emacs-compile-directory` **to your shell path** (and also `emacs-compile-file`). This file is used by the script running in `$HOME/.emacs.d/confs/packs.el` to install missing packages *(for instance)*. To get good performance with byte-compilation, type the following command in any virtual terminal running *Bourne-compatible* shell like `bash` or `zsh`:

    $ emacs-compile-directory ~/.emacs.d/lisp
    $ emacs-compile-directory ~/.emacs.d/lisp/bookmark-plus

You don't need to do this before running your emacs for the first time.

Windows users
-------------

*This version is unchecked*

You will need `msys` and/or `MingW` and the following program: `bash`, `tar`, `gzip`, `autoconf`, `make`, `svn`, `git`, `cvs`, `darcs`, `wget`, `rake`/`ruby` (via `pik`), `python` and its setup tool in order to use `easy_install`. You should install *Haskell plateform* (useful to get `darcs` via `cabal`, the Haskell package manager), `bzr`, `mplayer` (to use `emms`) and/or `mpd`.
