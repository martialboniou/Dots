Martialism Dot Files
====================

Everything should work with a bit of customization on Mac OS X and most of Linux distribution. I promise to test and perform enhancements this year to make those scripts easier to install (list of packages and resources). A cygwin/Windows revision may be available soon.

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

*Need some tidy up*

Gnu Emacs 23.2+ or 24
---------------------

All scripts in `.emacs.d` are tested on Emacs 23.3 and Emacs 24.0.90.

A great monothread OS for manipulating files, code, versioning tools, email but a poor editor compared to Vim. Fortunately Vimpulse gives us a better Emacs with modal edition mixed with the powertools of Emacs.

A lot of work is needed to enhance the portability but some snippets may be useful for you if you need to set some packages up. My daily tools are:

* `vimpulse`
* [`sticky-control`](http://www.cs.utoronto.ca/~ryanjohn/sticky-control.el) (excellent after a [yubitsume](http://en.wikipedia.org/wiki/Yubitsume))
* `ido`
* `undo-tree` (use `C-x u` to display tree)
* `org-agenda` on `org-mode` 7+
* `remember`
* `semantic` (from `cedet`)
* `wanderlust`
* `wdired` (an *editable* `dired`; some wrappers in `confs` for `viper`/`vimpulse` case)
* `anything`
* `ibuffer` (a `dired` for buffers)
* `yasnippet`
* `auto-complete`
* `hippie-expand` (on `C-p`)
* `nxhtml` (`js` (previously known as `espresso`) should be used as `js2-mode` doesn't work with `mmm-mode`)

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

See `.emacs.d/confs/shortcuts.el` for good ideas of bindings especially for buffer/window/frame navigation using tiling/cycling (thing about larswm or [Xmonad](http://xmonad.org/tour.html)) or for `<f5>`-`<f8>` keys (`<f5><f5>` toggle the current frame from multiple buffers to a single view on the current buffer via `revive.el`).

The current version should match **24.0** and need a major 23 if possible. (I recommend to use an `emacs` 23.2 or more because some radical changes where made in the byte-code after `emacs` 23.1.)

Read `.emacs.d/confs/vars.el` to configure your path. Remember this! By default, `.emacs` makes Emacs to:

1. support UTF-8 (`.emacs.d/confs/formats.el` to switch to another encoding) / remove eye candies (no toolbar / no scrollbar);
2. create `load-path` (all files and *subdirs* from `.emacs.d/lisp`);
3. load uninstalled packages via scripts in `confs/packs.el`;
4. generate `autoloads` at the root of `.emacs.d/lisp` (loading `cedet` if there's extended autoloads for `eieio` classes and methods);
5. add handmade autoloads if needed;
6. configure general behavior:
  * buffers and `ido` (**BEWARE**: `C-x C-b` to switch buffers, `C-x C-f` to find file in history, `C-x f` to find file normally),
  * `desktop` + `revive` (to save window configuration on quit),
  * `autosave` and `auto-save-list` (no more `*~` everywhere; all temporary files are kept in `.emacs.d/data`)
7. load specific configuration files.

Here, `require` is often avoided because automagically generated `autoload`s and `eval-after-load` is over-used. (That's why Emacs should start quickly. Don't worry if your first startup is slow: Emacs will fetch additional packages like `nxhtml`, a newest `cedet` or `wanderlust` in order to work ultimately.)

`.emacs.d/confs/vim-everywhere` is a bit special and hard to understand. It is here to:

* load `viper` and `vimpulse`
* extend the fringe to display line numbering
* extend colors with `font-lock-number-face` and `hl-line+`
* colorize keywords like **FIXME:**... and global line `<.>` viper state
* grab your current `colorscheme` in your `.vimrc` and use the same in `emacs` if any (a 256 colors compatible colorscheme adaptation named `wombat256mod` is displayed by default)
* add bindings like `M-0` to open the current buffer in Vim


Final Thoughts
==============

I'll do a blog for all this ASAP.

* **TODO:** bindings map database for my bindings in and out of the shell, Emacs and Vim (useful in Mac OS X and Xmonad/Gnome context).
* **TODO:** snippets database in another project to organize some tidbits and hacks used in those kind of dotfiles. For instance, read `chit` files with, ruby [`chit`](https://github.com/robin/chit).
