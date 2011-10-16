#!/usr/bin/env emacs
(progn
 (defvar *vim-now* t)   ; force to load vim-everywhere
 (load (expand-file-name "~/.emacs.d/confs/vars"))
 (switch-to-buffer (get-buffer "*scratch*")))
