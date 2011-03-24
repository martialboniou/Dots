Martialism Dot Files
====================

Everything should work with a bit of customization on Mac OS X and most of Linux distribution. I promise to test and perform enhancements this year to make those scripts easier to install (list of packages and resources). A cygwin/Windows revision may be available soon.

Zsh
---

Because the best shell make the most beautiful pearls. Zsh is a better Bourne with every fancy things from Korn. Forget [Bash](http://www.bash2zsh.com/) ! And get a better shell with smarter completions.

For example, some functions are useful to navigate in directories:
* Alt-U: go **up** in directory;
* Alt-P: go to the **previous** visited directory;
are great bindings to change directory without touching your command line.

The starting point is the `.zshenv` then visit:
* `.zsh/zshrc.d/` subdirectory for environment setting and functions' configuration;
* `.zsh/env/` subdirectory for `path` customization
Notice that the numbers in front of file names show you the loading **order** like on a lot GNU Debian classical library setup in `/etc`.

Vim 7
-----

*Need some tidy up*

Gnu Emacs 23
------------

A great monothread OS for manipulating files, code, versioning tools, email but a poor editor compared to Vim. Fortunately Vimpulse gives us a better Emacs with modal edition mixed with the powertools of Emacs.

A lot of work is needed to enhance the portability but some snippets may be useful for you if you need to set some packages up. My daily tools are:

* vimpulse
* ido
* undo-tree (use `C-x u` to display tree)
* org-agenda on org-mode 7+
* remember
* semantic (cedet)
* wanderlust
* wdired (**TODO:** need some hack to work with viper/vimpulse)
* anything
* ibuffer (a `dired` for buffers)
* yasnippet
* auto-complete
* hippie-expand (on `C-p`)
* Nxhtml (`espresso` should be used (newly renamed `js`) as `js2-mode` doesn't work with MuMaMo)

Some packages may also be useful in order to help this configuration to work fine. Please download:

* newsticker
* tiling
* revive
* multi-term
* pp-c-l

Anything is not as fast/smart as ido but it's very useful for displaying a one shot buffer. For example, type `<f5><f8>` to display a list including: 

* current buffers;
* recently open files;
* all files and directory in the current 

See `.emacs.d/confs/shortcuts.el` for good ideas of bindings especially for buffer/window/frame navigation using tiling/cycling (thing about larswm or [Xmonad](http://xmonad.org/tour.html)) or for `<f5>`-`<f8>` keys (`<f5><f5>` toggle the current frame from multiple buffers to a single view on the current buffer via `revive.el`).

The current version should match **23.3** and need a major 23 if possible.

Read `.emacs.d/confs/vars.el` to configure your path. Remember this! By default, `.emacs` makes Emacs to:

1. support utf-8 (`.emacs.d/confs/formats.el` to switch to another encoding) / remove eye candies (no toolbar / no scrollbar);
2. create `load-path` (all files and *subdirs* from `.emacs.d/lisp`);
3. load uninstalled packages via scripts in `confs/packs.el` or by using `pases`;
4. generate `autoloads` at the root of `.emacs.d/lisp` (loading `cedet` if there's extended autoloads for `eieio` classes and methods);
5. add handmade autoloads if needed;
6. configure general behavior:
  * buffers and `ido` (**BEWARE**: `C-x C-b` to switch buffers, `C-x C-f` to find file in history, `C-x f` to find file normally),
  * desktop + revive (to save window configuration on quit),
  * autosave (no more `*~` everywhere; all temporary files are kept in `.emacs.d/data`)
7. load specific configuration files.

Here, `require` is often avoided because automagically generated `autoload`s and `eval-after-load` is over-used.
`.emacs.d/confs/vim-everywhere` is a bit special and hard to understand. It is here to:

* load `viper` and `vimpulse`
* extend colors with `font-lock-number-face` and `hl-line+`
* colorize keywords like **FIXME:**... and global line `<.>` viper state
* grab your current `colorscheme` in your `.vimrc` and use the same in Emacs if any (an old colorscheme adaptation named `inkpot` is displayed by default)
* add bindings like `M-0` to open the current buffer in Vim


Final Thoughts
==============

I'll do a blog for all this ASAP.

* **TODO:** bindings map database for my bindings in and out of the shell, Emacs and Vim (useful in Mac OS X and Xmonad/Gnome context).
* **TODO:** snippets database in another project to organize some tidbits and hacks used in those kind of dotfiles.
