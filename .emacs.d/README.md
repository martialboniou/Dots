Installation
============

All users
---------

As packages will be fetched on the Internet then installed and configured, you only need to byte-compile the ~/.emacs.d/lisp directory and the available subdirectories by hand. **First, add the shell script** `emacs-compile-directory` **to your shell path** (and probably also `emacs-compile-file`). This file is used by the script running in `$HOME/.emacs.d/confs/packs.el` to install missing packages *(for instance)*. To get good performance with byte-compilation, type the following command in any virtual terminal running *Bourne-compatible* shell like `bash` or `zsh`:

    $ emacs-compile-directory ~/.emacs.d/lisp
    $ emacs-compile-directory ~/.emacs.d/lisp/bookmark-plus

You don't need to do this before running your emacs for the first time.

Modern UN*X/BSD users
---------------------

Ensure you installed the following packages via `macports` on OS X or your favorite `deb`/`rpm` package manager:

* `cvs`
* `svn`
* `git`
* `autoconf`
* `curl`
* `python 2.x (should be 2.7+)`
* `setuptools` (to get python manager named `easy_install`)
* `mplayer`
* OPTIONAL: `wget`

On most systems, you should have:
* `sed`
* `awk`
* `make`
* `bash`

In order to install `ruby` I firmly recommend to get it via `rvm`:

    $ bash < <( curl http://rvm.beginrescueend.com/releases/rvm-install-head )
    $ source "$HOME/.rvm/scripts/rvm"
    $ rvm install ree,1.9.2-head,jruby

You should add `[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"` to your `.bashrc` or `.zshrc`. If you use the complete `Dots` packages, the `Dots/.zsh/env/03-rvm.zsh` creates a good path for you for your `zsh` configuration.

To install `haskell`, I don't recommend you to install the `ghc` package and try to build everything by hand: you should install [haskell-plateform](http://sporkcode.wordpress.com/2009/07/11/installing-the-haskell-platform-in-ubuntu/) instead.

Now you can install:

* *Bazaar* installer via `python` manager:
    $ easy_install bzr
* `darcs` distributed RCS via `cabal`, the `haskell` package manager:
    $ cabal update
    $ cabal install darcs
* OPTIONAL: `chit`, as small reference card to keep an eye on your favorite cheatsheets, via `rubygem`:
    $ gem install chit

Windows users
-------------

You will need *MSYS* and/or *MinGW* and the following program: ``touch`, bash`, `tar`, `gzip`, `autoconf`, `make`, `svn`, `git`, `cvs`, `darcs`, `curl`, `rake`/`ruby`, `python` and its setup tool in order to use `easy_install`. You should install *Haskell plateform* (useful to get `darcs` via `cabal`, the Haskell package manager), `bzr`, `mplayer` (to use `emms`) and/or `mpd`. For an easy installation, please, follow these instructions:

* install [mingw-get-inst](http://sourceforge.net/projects/mingw/files/Automated%20MinGW%20Installer/mingw-get-inst/). By default, the installer provides you with `bash`, `gcc`, `sed`, `awk` and a lot of UN*X tools like `tar`:
* open a *MinGW Shell* terminal and type:
    $ mingw-get update
    $ mingw-get install msys-system-builder msys-unzip msys-cvs msys-gmp
* OPTIONAL: you may install `msys-man`, `msys-flex`, `msys-patch` and `msys-wget` too;
* install [ruby](http://rubyforge.org/projects/rubyinstaller/) (`rvm` doesn't seem to work on Windows even if you got `bash` installed);
* OPTIONAL: when ruby is installed, you may install a small reference card to keep an eye on your favorite cheatsheets named `chit`:
    $ gem install chit
* install [svn](http://subversion.tigris.org/servlets/ProjectDocumentList?folderID=91&expandFolder=91&folderID=74);
* install [git](http://code.google.com/p/msysgit/downloads/list);
* install [haskell](http://hackage.haskell.org/platform/windows.html).

To install `python`, this is the recommended way:

* install the current production version in 2.X version [python](http://www.python.org/download/releases/);
* create a `%PYTHONPATH%` environment variable and set `C:\Python2X\Lib` and `C:\Python2X\Lib\site-packages`;
* add `C:\Python2X` to your local or global `%PATH%`;
* fetch the [setuptools](http://pypi.python.org/pypi/setuptools#downloads) for `win32` for the **Py Version** matching your `python` version number in 2.X;
* add `C:\Python2X\Lib\site-packages\setuptools` to your local or global `%PATH%`.

You can now install `bzr` via `easy_install`:
    $ easy_install bzr

After the `haskell` installation, you should get the `cabal` package manager. You should use it to build the patch-based RCS named `darcs`. You may follow the `darcs` wiki way <http://wiki.darcs.net/BuildingUnderWindows>, by  installing  [OpenSSL (32-bit)](http://www.slproweb.com/products/Win32OpenSSL.html) (not the *Light* version), executing on the *MinGW Shell* command line:
    $ cd /c/OpenSSL-Win32/lib/MinGW
    $ cp libeay32.a libcrypto.a
    $ cp ssleay32.a libssl.a
and then, fetching the [libcurl](http://curl.haxx.se/latest.cgi?curl=win32-ssl-devel-msvc) source code (notice that a `curl.exe` is installed when you install `git`) and install it via the next command:
    $ ./configure --with-ssl=/c/OpenSSL-Win32; make; make install
I prefer a simpler way by downloading one of the [`libcurl` *MinGW* builds](http://haskell.forkio.com/Home/curl-win32/curl-7.19.4-mingw32.zip?attredirects=0&d=1) after the OpenSSL installation. Say you unzipped `curl-7.19.4` in a `Code\src` subdirectory in  `"My Documents"` directory. Install `darcs` via your *MinGW32* terminal:
    $ cd $HOME/"My Documents"/Code/src/curl-7.19.4
    $ cabal install --extra-lib-dirs="`echo $PWD/lib`" --extra-include-dirs="`echo $PWD/include`"

Finally, you'll need to install [w3m]().
You may:
* Grab [it](http://www.daionet.gr.jp/~knok/software/misc/w3m.exe) and copy it to `/usr/bin` in the *MinGW shell*.
* Fetch the [Garbage collection for C++](http://sourceforge.net/projects/libgc/) library and unzip it in a **new directory** (the archive is not inside a subdirectory);

Note: Some additional libraries/binaries contained in [*GnuWin*](http://gnuwin32.sourceforge.net/) may be useful for some developers.
