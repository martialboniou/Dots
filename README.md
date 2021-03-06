Martialism Dot Files
====================

Everything should work with a bit of customization on Mac OS X and most of Linux distribution. I promise to test and perform enhancements this year to make those scripts easier to install (list of packages and resources). A cygwin/Windows revision may be available soon.

Install
-------

Use the [easy](https://github.com/holman/dotfiles) way:

    mkdir -p ~/Documents/Code
    git clone https://github.com/martialboniou/Dots.git !$/Dots
    cd !$ && script/bootstrap

Zsh
---

Because the best shell make the most beautiful pearls. Zsh is a better Bourne with every fancy things from `csh` like *aliases*. Forget [Bash](http://www.bash2zsh.com/) ! And get a better shell with smarter completions.

For example, some functions are useful to navigate in directories:

* Ctrl-U: go **up** in directory;
* Ctrl-P: go to the **previous** visited directory;

are great bindings to change directory without touching your command line (and so letting ugly `cd ..` in your history).

The starting point is the `.zshenv` then visit:

* `.zsh/zshrc.d/` subdirectory for environment setting and functions' configuration;
* `.zsh/env/` subdirectory for `path` customization.

Notice that the numbers in front of file names show you the loading **order** like on a lot GNU Debian classical library setup in `/etc`.

`tmux` must be installed in order to use `.zsh/zshrc.d/99-tmux-sessions.zsh`: this script embeds `zsh` in a `tmux` session and permits re-attachment on exit. **I firmly advised MacOSX users to install** [tmux-MacOSX-pasteboard](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard) to enable the `pbcopy`/`pbpaste` to be wrapped in `tmux` copy-mode.

Vim 7
-----

Tested on POSIX. The packages will be auto-installed via vundle.

GNU Emacs 24.3
--------------

Tested on Mountain Lion. Require CVS (if Xcode 5 CLI doesn't have it, use `brew tap martialboniou/lostplusfound && brew install cvs`).

All scripts in `.emacs.d` are currently tested on Emacs 24.3. **Ensure your LOAD-PATH know this path:** `~/.emacs.d/lisp` to be able to launch partial setup (see my `zsh` setup).

A great monothread OS for manipulating files, code, versioning tools, email but a poor editor compared to Vim. Fortunately Vimpulse gives us a better Emacs with modal edition mixed with the powertools of Emacs.

The first Emacs is launched, Emacs asks at startup about:

* your environment of typing: *Vim-like* or *Emacs*. If *Vim-like* is emulated, *Evil* is used by default and the other tools are customized to work with this environment;
* your keyboard: *Dvorak* or *Qwerty*. If *Dvorak* is chosen, a new keybinding appears for copy/paste. This keybinding is based upon the Oracle/Sun 5/6 keyboards' extra keys' order (on the left part of the keyboard): `C-; C-;` stands for `kill-region` (`CUT`), `C-; C-a` stands for `yank` (`PASTE`) and eventually `C-; C-'` stands for `copy-region-as-kill` (`COPY`);
* your use of `C-w` and `C-h` for deletion as on ANSI terminals: if true, those keybindings behave for delete a word and a character, respectively. The `C-w` keybinding is normally used to cut a region: alternatively, a *Vim-like* will use the *Vi* keybindings in *Evil* normal mode; a `CUA` user will use the delayed `C-x` keybinding; a *Dvorak* typist will use the new `C-; C-;` keybinding; and others would like to create their own keybindings (by default `C-x 6 ;` should be available);
* your need of packages: *Full install* or *None*. If you want the complete installation, the system will install the recommended packages to optimize the use of this setup. Otherwise, no `el-get` packages will be installed (useful for custom/testing/offline session). If you want a *Vim-like* environement at startup and *None* of the recommended packages, be careful that you will need to `M-x el-get-install` the package named `evil`.

Once your Emacs is launched, a file `.emacs.d/data/.launched` is created to register your choice. If you want to change, you may delete it. If you want another setup for one specific session, you may force *Emacs* to be launched with other options using:

* `*vim-now*` boolean to enable/disable the *Vim-like* setup;
* `*dvorak-now*` boolean to enable/disable the *Dvorak* keyboard setup;
* `*term-now*` boolean to enable/disable the `C-w` & `C-h` as deletion keybindings;
* `*full-ammo-now*` boolean to enable/disable the full `el-get-sources` install.

Those forcing options may be set at the heading of the `init.el` file in the `.emacs.d/` directory by using `defvar`.

A minimal setup may be launched any time by running explicitly the `kernel` script (Emacs will detect an abnormal startup and won't require additional setup):

    $ emacs -q -l kernel

Other partial setup may be launched by this method: `mail` will load every `org` and `wl` (wanderlust) configuration files, `peyton-jones-family` and `rectify` together should run a good environment uniquely enhanced to develop programs based on *ML* with snippets and flymake shortcuts... You also may force options in command line. Say, we want *minimal setup with Vim-like environement*:

    $ emacs -q -e "(defvar *vim-now* t)" -l kernel

It works even if your initial statement was not to let `vim` emulation to run. A simpler approach would be to load `vim-everywhere`:

    $ emacs -q -l vim-everywhere

The following list is some of my recommended `el-get` packages (or, if unreachable, those provided by programs in `vendor`directory or better, in the `emacs-revival` archive):

* `evil` and `undo-tree`
* `ido`
* `org-agenda` on `org-mode` 7+
* `remember`
* `wanderlust`
* `wdired` (an *editable* `dired`; some wrappers in `lisp` for `viper` or `evil` case)
* `anything`
* `ibuffer` (a `dired` for buffers)
* `yasnippet`
* `auto-complete`
* `hippie-expand` (on `C-p`)

Some packages may also be useful in order to help this configuration to work fine. For example:

* `tiling`: to split and reorganise windows;
* `revive`: to save and restore multiple window configurations;
* `emms`: to transform emacs in media player;
* `pp-c-l`: to print `^L` as a page separator;
* `newsticker`: to read RSS (not optimal for instance);
* `multi-term`: to manage multiple `term` windows on POSIX systems.

`anything` is not as fast/smart as `ido` but it's very useful for displaying a one shot buffer. For example, type `<f5><f8>` to display a list including:

* current buffers;
* recently open files;
* all files and directory in the current.

See `.emacs.d/lisp/shortcuts.el` for good ideas of bindings especially for buffer/window/frame navigation using tiling/cycling (thing about larswm or [Xmonad](http://xmonad.org/tour.html)) or for `<f5>`-`<f8>` keys (`<f5><f5>` toggle the current frame from multiple buffers to a single view on the current buffer via `revive.el`).

Read `.emacs.d/lisp/vars.el` to configure your path. Remember this! By default, `.emacs` makes Emacs to:

1. support UTF-8 (`.emacs.d/lisp/formats.el` to switch to another encoding) / remove eye candies (no toolbar / no scrollbar);
2. create `load-path` (all files and *subdirs* from `.emacs.d/vendor`);
3. load uninstalled packages via scripts in `lisp/packs.el`;
4. generate `autoloads` at the root of `.emacs.d/vendor` (loading `cedet` if there's extended autoloads for `eieio` classes and methods);
5. add handmade autoloads if needed;
6. configure general behavior:
  * buffers and `ido` (**BEWARE**: `C-x C-b` to switch buffers, `C-x C-f` to find file in history, `C-x f` to find file normally),
  * `desktop` + `revive` (to save window configuration on quit),
  * `autosave` and `auto-save-list` (no more `*~` everywhere; all temporary files are kept in `.emacs.d/data`)
7. load specific configuration files.

Here, `require` is often avoided because automagically generated `autoload`s and `eval-after-load` is over-used. (That's why Emacs should start quickly. Don't worry if your first startup is slow: Emacs will fetch additional packages like `nxhtml`, a newest `cedet` or `wanderlust` in order to work ultimately.)

`.emacs.d/lisp/vim-everywhere` is a bit special and hard to understand. It is here to:

* load `evil-mode`
* extend the fringe to display line numbering
* extend colors with `font-lock-number-face` and `hl-line+`
* colorize keywords like **FIXME:**... and global line `<.>` viper state
* grab your current `colorscheme` in your `.vimrc` and use the same in `emacs` if any (a 256 colors compatible colorscheme adaptation named `wombat256mod` is displayed by default)
* add bindings like `M-0` to open the current buffer in Vim


Windows compatibility
=====================

*Under construction*: Cygwin should be installed in order to use symlinks and `bash` scripts correctly but the installation should be compatible with `cmd.exe`. `git` and `vim` setup uses *conduit* files (instead of *symlink*) so that the files won't be symlinked but copied on Windows system to be able to use both Cygwin and standard programs.
