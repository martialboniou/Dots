;;; .emacs
;;
;; Filename: .emacs
;; Description: fichier de configuration Emacs (cf. Compatibility)
;; Author: Martial Boniou
;; Maintainer: Martial Boniou (hondana.net/about)
;; Created: Wed Nov 18 11:53:01 2006
;; Version: 3.0
;; Last-Updated: Wed Mar 23 15:30:35 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 1891
;; URL: hondana.net/private/emacs-lisp
;; Keywords:
;; Compatibility: C-\ is linked to Esc-map
;;
;; Features that might be required by this library:
;;
;;   CEDET 1.0 normally loaded..
;;   REMARK: `load-path' and `loaddefs' generated
;;   TIPS: http://cheat.errtheblog.com/s/emacs_tips/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;; (DEFUSER 'martial (super alloc))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; *MOOD*
(defvar *i-am-an-emacsen-dev* t
  "If true, elisp helpers will be loaded.")
(defvar *i-am-a-vim-user* t
  "If true, Emacs will be Vimpyrized. (ViViVi, the beast)")
(defvar *i-am-a-dvorak-typist* t
  "If true, additional Dvorak-friendly keybindings.")
(defvar *i-am-a-common-lisp-advocate* t
  "If true, require CL emacs extension (eg. EIEIO).")

;;; *VARS*
(defvar *emacs/normal-startup* t
  "If `TRUE', load every configuration file. Used by `vars.el' .
Actually Emacs may be loaded from this file too when called by
another configuration file.")
(when *emacs/normal-startup*
  (load "~/.emacs.d/confs/vars"))       ; the only hardcoded path
(defvar renew-autoloads-at-startup nil) ; (re-)create autoloads (eg. after a change in `confs/packs.el') FIXME: find a better process
(defvar the-desktop-file nil)           ; desktop
(defvar the-desktop-lock nil)
(defvar desktop-dir nil)
(defvar autosave-dir nil                ; autosave
  "Temporary variable use to make autosaves directory name.
(That's where #foo# goes.) It should normally be nil if
`user-init-file' is compiled.")
(defvar backup-dir   nil                ;backup
  "Temporary variable use to make backups directory name.
(That's where foo~ goes.) It should normally be nil if
`user-init-file' is compiled.")
(defvar confirm-frame-action-buffer-alist nil ; kill frame alert
  "Associated list of buffer properties in order to get a confirmation
alert during action on the frame containing this buffer. A property
is a CONS formed by an information and a LIST of parameters for
this information.
Example: (MAJOR-MODE . (CHESS-MASTER-MODE MAIL-DRAFT-MODE).
See the advised `delete-frame' at the end of this file as a use case.")

;;; *TIMER*
(defvar emacs-load-start (current-time))
(defvar emacs/breaktime-on-error 3
  "Time (in seconds) of the pause on error.")
(defvar mars/desktop-saver-timer nil)
(defvar mars/fast-kill t)               ; (setq mars/fast-kill nil) to redo 'loaddefs on quit

;;; *APPEARANCE*
(defmacro if-bound-call (form &rest args)
  "If `FORM' is bound as a function, call it with `ARGS'."
  `(if (fboundp ',form)
       (,form ,@args)))
(defmacro disable-eyecandies (&rest modes)
  `(progn ,@(mapcar '(lambda (x) `(if-bound-call ,x -1)) modes)))
(defun which-emacs-i-am ()
  (if (string-match "XEmacs\\|Lucid" emacs-version) "xemacs" "gnuemacs"))
(defun build-custom-file-name (subdirectory-in-data &optional general)
  "A custom file for different emacsen and system version or `NIL' if
none (and not makeable). If `GENERAL' is true, it will refer to or creates
a simple `custom.el'."
  (concat (file-name-as-directory mars/local-root-dir)
          (file-name-as-directory mars/personal-data)
          (file-name-as-directory subdirectory-in-data)
          (if general "custom.el"
            (concat (which-emacs-i-am)
                    "-" (number-to-string emacs-major-version)
                    "-" (replace-regexp-in-string "/" "-" (symbol-name system-type)) ".el"))))
(defun safe-build-custom-file (subdirectory-in-data &optional general)
  (let ((file (build-custom-file-name subdirectory-in-data general)))
    (if (file-exists-p file)
        file
      (let ((custom-dir (expand-file-name (file-name-directory file))))
        (if (file-exists-p custom-dir)
            file            ; path exists
          (let ((dirs-to-create (split-string custom-dir "/"))
                (path ""))
            (nbutlast dirs-to-create)
            (while dirs-to-create
              (setq path (concat path (pop dirs-to-create) "/"))
              (unless (file-exists-p path)
                (condition-case nil
                    (make-directory path)
                  (error
                   (setq dirs-to-create nil)))))
            (when (file-exists-p custom-dir)
              file)))))))       ; path created
;; - disable eyecandies
(disable-eyecandies menu-bar-mode tool-bar-mode scroll-bar-mode)
;; - basic customization / see `confs/init/common-pre-custom' too
(custom-set-variables
 '(inihibit-startup-message t)
 '(custom-file (safe-build-custom-file "Customize"))
 '(line-number-mode t)
 '(column-number-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(resize-mini-windows 'grow-only)   ; resize windows/frame if needed
 '(max-mini-window-height 0.1)
 '(focus-follows-mouse nil)
 '(mac-allow-anti-aliasing t))
(mouse-avoidance-mode 'jump)

;;; *HELPERS*
(defun flatten (list)
  (cond ((atom list) list)
        ((listp (car list)) (append (flatten (car list)) (flatten (cdr list))))
        (t (append (list (car list)) (flatten (cdr list))))))
(defun safe-load (library)
  "Secure load a library."
  (condition-case err
      (load-library library)
    (error
     (progn
       (message "Failed to load %s: %s" library err)
       (sleep-for emacs/breaktime-on-error)))))
(defun conf-locate (conf)
  "Locate a configuration file. Normally out of the `LOAD-PATH'."
  (let ((path (mapcar '(lambda (x) (concat (file-name-as-directory mars/local-root-dir) x)) mars/local-conf-path)))
    (locate-library conf nil path)))
(defun conf-load (conf)
  "Load a configuration file."
  (let ((found (conf-locate conf)))
    (when found
      (load-file found))))      ; load compiled version if any
(defun safe-autoloads-load (loaddefs)
  "Secure load a `loaddefs' file. Load additional libraries
if special autoload format (eg: `cedet' autoloads)."
  (condition-case err
      (load loaddefs)
    (error
     (message "Cedet must be loaded to parse `%s' correctly: %s" loaddefs err)
     (safe-load-cedet)
     (load loaddefs))))
(defun safe-load-cedet ()
  "Load `cedet'. Be sure to not load the compiled common file."
  (condition-case err
      (load-file (concat
                  (file-name-directory
                   (locate-library "cedet")) "cedet.el"))
    (error (message "error: cedet environment not loaded: %s" err))))
(defun twb/autoload (library &rest functions)
  "Autoload LIBRARY when one of the FUNCTIONS is invoked.
twb#emacs at http://paste.lisp.org/display/43546,"
  (when (locate-library library)
    (mapc (lambda (y)
            (autoload y library nil t))
          functions)))
(defun mergeable-to-path-p (dir)
  "Checks if `DIR' is a visitable directory or link."
  (and (file-exists-p dir)
       (save-excursion
         (let ((inspectable t))
           (condition-case nil
               (cd dir)
             (error
              (setq inspectable nil)))
           inspectable))))
(defun mars/add-to-load-path (root &optional &rest pathname)
  "Add directories to `load-path' according the two following
patterns:
ROOT (LIST PATH1 PATH2 ...) => ROOT / PATH1 & ROOT / PATH2 & ...
ROOT                        => ROOT"
  (let ((path (if pathname
                  (let ((r (file-name-as-directory root)))
                    (mapcar (lambda (x)
                              (expand-file-name (concat r x)))
                            (flatten pathname)))
                (list (expand-file-name root)))))
    (mapc
     (lambda (dir)
       (when (and (mergeable-to-path-p dir)
                  (not (file-exists-p (concat
                                       (file-name-as-directory dir)
                                       ".nosearch")))) ; test exclusion on `dir'
         (let ((default-directory dir)
               (orig-load-path load-path))
           (setq load-path (list dir))
           (normal-top-level-add-subdirs-to-load-path)
           (nconc load-path orig-load-path)))) ; reverse path construct
     path)))
(defun mars/autoload (libraries-and-functions)
  (mapc (lambda(x)
          (apply 'twb/autoload x))
        libraries-and-functions))
(defmacro bind-key (key function)
  `(global-set-key (read-kbd-macro ,key) ,function))
(defun bind-keys (bindings)
  "Map keys from a list."
  (if (null (cdr bindings))
      t
    (let ((key (car bindings))
          (function (cadr bindings)))
      (bind-key key function)
      (bind-keys (cddr bindings)))))

;;; UTF-8
(let ((encoding 'utf-8))
  (setq locale-coding-system encoding)
  (set-terminal-coding-system encoding)
  (set-keyboard-coding-system encoding)
  (set-selection-coding-system encoding)
  (prefer-coding-system encoding)
  (set-language-environment (symbol-name encoding)))

;;; GENERATE PATH (add your 'SITE-LISP)
;;
;; Notice:
;;
;; touch a file in a SITE-LISP directory to control visibility:
;; - .nosearch => not in `load-path' / no `autoloads' generated (eg. resources)
;; - .noauto   =>     in `load-path' / no `autoloads' generated (eg. `ecb' and `nxhtml' frameworks)
;; - .cedet    =>     in `load-path' / use `cedet-autoloads' (for `defclass')
;;
(mars/add-to-load-path mars/local-root-dir mars/site-lisp-path)
;; FIXME: use subdirs.el in those two directories

;;; PACKAGE MANAGEMENT
;; - check/install (use `pases' for up-to-date wl & org 7)
(conf-load "packs")                     ; additional path + autoloads here

;;; COMMON LISP EXTENSION
(when *i-am-a-common-lisp-advocate*
  (unless (fboundp 'eieio-defclass)     ; `cedet' if no CLOS
    (safe-load-cedet)))

;;; GENERATE AUTOLOADS (fetch your 'SITE-LISP 's LOADDEFS or create it)
;; FIXME: work for one site-lisp dir for instance!!
(mapc '(lambda (x)
         (let ((mars/loaddefs
                (concat
                 (file-name-as-directory mars/local-root-dir)
                 (file-name-as-directory x) "loaddefs.el")))
           (unless (and (file-exists-p mars/loaddefs)
                        (not renew-autoloads-at-startup)) ; force to renew in some case even if `loaddefs' exists
             (load "update-auto-loads")
             (update-autoloads-in-package-area)) ; adds 'update-auto-loads autoloads in loaddefs too
                                        ; updates CEDET autoloads for CEDET directories
           (safe-autoloads-load mars/loaddefs)))
      mars/site-lisp-path)
(setq renew-autoloads-at-startup nil)   ; reset to prevent slow reloading
(add-hook 'kill-emacs-hook 'update-autoloads-in-package-area)

;;; HANDMADE AUTOLOADS
(mars/autoload '(("unbound"           describe-unbound-keys) ; display unbound keys ([F1-b] to display all bindings)
                 ("tiling"            tiling-cycle tiling-master) ; tiling window manager
                 ("buffer-move"       buf-move-down buf-move-up buf-move-left buf-move-right) ; quick move for buffers
                 ("iswitchb"          iswitchb-minibuffer-setup)
                 ("sunrise-commander" sunrise sr-virtual-mode)
                 ("anything"          anything anything-config)
                 ("hideshowvis"       hideshowvis-enable hideshowvis-minor-mode)
                 ("header2"           auto-make-header auto-update-file-header)
                 ("psvn"              psvn)
                 ("hippie-exp"        hippie-expand he-init-string he-substitute-string)
                 ("calc-ext"          calc-do-calc-eval)
                 ("hexview-mode"      hexview-find-file)
                 ("simple-call-tree"  simple-call-tree-analyze simple-call-tree-alist)
                 ("pymacs"            pymacs-apply pymacs-call pymacs-eval pymacs-exec pymacs-load)
                 ("markdown-mode"     markdown-mode)
                 ("yaml-mode"         yaml-mode)
                 ("newsticker"        newsticker-start newsticker-show-news)
                 ("wl-mailto"         wl-mailto-compose-message-from-mailto-url)
                 ("emms-source-file"  emms-dired emms-add-directory-tree emms-add-directory emms-add-file)
                 ("emms"              emms-playlist-buffer-list emms)
                 ("emms-streams"      emms-streams emms-stream-init)))

;;; ESSENTIAL TOOLS
;; - auto-cache byte-compiled libraries
;; (safe-load "byte-code-cache")

;; - defs = functions/macros to start with
(conf-load "defs")
;; - delete keys' behavior
(bind-keys
 '("<kp-delete>" delete-char))

;; - undo-tree | redo+ (both `vimpulse' compatible)
(if (locate-library "undo-tree")
    (require 'undo-tree)                ; display tree by using C-x u
  (progn
    (require 'redo+)
    (eval-after-load "redo"
      '(progn (setq undo-no-redo t)))))

;;; MODAL EDITING & COLOR-THEME WITH PARENTHESES' SUPPORT
(if *i-am-a-vim-user*
    (conf-load "vim-everywhere") ; vimpulse (incl. parens' matching) + colorscheme
  (progn
    (require 'mic-paren)         ; faces for (mis)matching parentheses
    (color-theme-initialize)
    (eval-after-load "color-theme" '(color-theme-ld-dark))
    (eval-after-load "mic-paren"   '(paren-activate))))

;;; DESKTOP
(require 'desktop)
;; variables
(custom-set-variables
 '(desktop-buffers-not-to-save "\\(^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|.*_flymake.*\\)$")
 '(desktop-files-not-to-save (concat "\\("
                                     "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                                     "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                                     "/[^/:]*:\\|.*_flymake\\..*"
                                     "\\)$"))
 '(desktop-minor-mode-table (quote ((auto-fill-function auto-fill-mode)
                                    (vc-mode nil)
                                    (vc-dired-mode nil)
                                    (flymake-mode nil)
                                    (ecb-minor-mode nil)
                                    (semantic-show-unmatched-syntax-mode nil)
                                    (semantic-stickyfunc-mode nil)
                                    (senator-minor-mode nil)
                                    (semantic-idle-scheduler-mode nil)))))
;; hooks
(add-hook 'desktop-save-hook
          'kiwon/save-window-configuration)
(add-hook 'desktop-after-read-hook
          'kiwon/restore-window-configuration) ; save/restore the last window
                                        ; configuration with `DESKTOP'
;; desktop + autosave + backup files: see eof

;;; BUFFERS
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets ; name<foo/bar> | name<quux/bar>
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*"
      global-auto-revert-mode t)        ; IMPORTANT: buffers synchronization
(eval-after-load "recentf"
  '(progn
     (defun undo-kill-buffer (arg)
       "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
       (interactive "p")
       (let ((recently-killed-list (copy-sequence recentf-list))
             (buffer-files-list
              (delq nil (mapcar (lambda (buf)
                                  (when (buffer-file-name buf)
                                    (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
         (mapc
          (lambda (buf-file)
            (setq recently-killed-list
                  (delq buf-file recently-killed-list)))
          buffer-files-list)
         (message "echo: %s" (prin1-to-string
                              (reduce 'cons recently-killed-list
                                      :start 0 :end 10)))
         (find-file
          (if arg (nth arg recently-killed-list)
            (car recently-killed-list)))))))
;; minibuffer: see eof

;;; BOOKMARKS
(require 'bookmark+)

;;; CEDET is loaded from `SAFE-AUTOLOADS-LOAD' if `LOADDEFS' is inside the `mars/site-lisp-path', from `UPDATE-AUTOLOADS-IN-PACKAGE-AREA' otherwise or previously if `*i-am-a-common-lisp-advocate*'

;;; SERVER
(when *emacs/normal-startup* (start-named-server "mars"))

;;; GLOBAL KEYS (see <confs>/shortcuts.el for additionnal keys)
(bind-keys
 '("C-x C-f"   ido-recentf-file-name-history          ; IMPORTANT: C-x C-f is not `find-file' anymore (C-f to switch to `ido-find-file' only works from `ido-buffer-internal' and `ido-file-internal' derivatives.)
   "C-x F"     ido-recentf
   "C-x f"     ido-find-file ; may be called from `ido-switch-buffer' (doing C-x C-b C-f)
   "C-h"       delete-backward-char ; C-w as 'DELETE-BACKWARD-WORD in Vi emu
   "C-x C-h"   help-command ; use F1 for contextual help / C-h and C-w are used as in ASCII
   "C-\\"      esc-map      ; if ESC- is needed
   "C-="       shell-command
   "M-n"       new-frame                ; XXX check if no issue
   "M-<f2>"    apply-macro-to-region-lines ; use F3/F4 for kmacro start/end
   "C-c o"     anything-occur              ; or simply occur ?
   "M-X"       anything-M-x             ; a super command executor
   ;; "M-:"       anything-eval-expression-with-eldoc ; a super evaluator
   "C-:"       anything-M-x             ; C-S-; = M-X
   "C-c l"     org-store-link ; [default]
   "C-x C-b"   ido-switch-buffer        ; switch buffer on "C-x C-b" (faster than typing "C-x b")
   "C-x b"     ibuffer                  ; nice buffer browser (a la `dired')
   "C-p"       hippie-expand            ; like Vim next-expansion key
   "C-c _ w"   whitespace-mode
   "C-c _ t"   whitespace-toggle-options
   "C-c = w"   global-whitespace-mode
   "C-c = t"   global-whitespace-toggle-options
   "C-x 4 t"   transpose-buffers
   "C-c C-m"   make-directory           ; or M-m in `ido'
   "C-c C-0"   anything-mini            ; buffers (w/o `ibuffer' tags) & recentf (w/o `ido-recentf' completion)
                                        ; See shortcuts.el: anything on [<f5><f8>] for fast navigation in:
                                        ; - buffers (prefer `ido-switch-buffer' [C-x C-b] or `cycle-buffer' [<f5><f[4|6]>] for faster cycling)
                                        ; - recentf (prefer `ido-recentf-file-name-history' [C-x C-f] for faster finding)
                                        ; - files in `default-directory' (not present in `anything-mini')
   "C-c C-9"   anything-imenu))         ; IMPORTANT: useful for fast code navigation (w/o `ecb' fancies)
                                        ;            anything-browse-code map on [<f7><f7>] too


;; C-w should be used for 'backward-word-delete so there should be
;; another way to do cut/copy/paste:
;; 1) Vi commands (d/y/p) for Vim user
;; 2) special C-; (;/'/a) for Dvorak typist (including Vim user)
;; 3) standard commands (x/v/c) for Qwerty typist (including Vim user)
(if *i-am-a-dvorak-typist*
    (bind-keys                          ; Sun help keys' order
     '("C-; C-'" copy-region-as-kill
       "C-; C-a" yank
       "C-; C-;" kill-region))
  (progn
    (cua-mode t)                    ; C-c/C-x got timeout in order to make
                                        ; combinations to work
    (eval-after-load "cua-mode"
      '(progn
         (setq cua-auto-tabify-rectangles nil)
         (transient-mark-mode 1)
         (setq cua-keep-region-after-copy t))))) ; MS Windows behavior
(global-unset-key (kbd "C-z"))      ; ELSCREEN or other packages may use it
(global-unset-key (kbd "C-x C-z"))  ; reserved for viper-mode

;;; IDO (faster than icicles)
(eval-after-load "ido"
  '(progn
     (setq confirm-nonexistent-file-or-buffer nil)
     (ido-mode 1)
     (setq ido-enable-tramp-completion nil
           ido-enable-flex-matching t
           ido-everywhere t
           ido-max-directory-size 100000
           ido-create-new-buffer 'always
           ido-enable-last-directory-history nil
           ido-confirm-unique-completion nil ; wait for RET, even for unique
           ido-show-dot-for-dired t         ; put . as the first item
           ido-use-filename-at-point 'guess ; ido guesses the context
           ido-use-url-at-point nil
           ido-default-file-method 'raise-frame ; you may ask if it should be displayed in the current
                                        ; window via `maybe-frame'. Let `ido-switch-buffer' do this.
           ido-default-buffer-method 'selected-window
           ido-ignore-extensions t))) ; `completion-ignored-extensions'

;;; IBUFFER (better than 'electric-buffer-list)
(eval-after-load "ibuffer"
  '(define-key ibuffer-mode-map (kbd "'") 'kill-all-dired-buffers))

;;; ANYTHING
(setq anything-c-adaptive-history-file (concat (file-name-as-directory mars/local-root-dir)
                                               (file-name-as-directory mars/personal-data)
                                               "anything-c-adaptive-history"))
(require 'anything-match-plugin)
(require 'anything-config)
(eval-after-load "anything-config"
  '(progn
     (defvar mars/anything-pers-action-binding "C-."
       "New binding for `anything-execute-persistent-action'. Was originally
the should-be-forbidden C-z.")
     (define-key anything-map (kbd "C-l") 'anything-find-files-down-one-level) ; originally C-. in 'window-system
     (define-key anything-map (kbd "C-z") nil)
     (define-key anything-map (read-kbd-macro mars/anything-pers-action-binding)
       'anything-execute-persistent-action)
     (setcdr (assoc 'persistent-help anything-c-source-advice)
             (concat "Describe function / C-u "
                     mars/anything-pers-action-binding
                     ": Toggle advice"))
     ;; ido case
     (eval-after-load "ido"
       '(progn
          nil
          ;; (anything-lisp-complete-symbol-set-timer 150) ; collect by 150 sec
          ;; (define-key emacs-lisp-mode-map "\C-\M-i"
          ;;   'anything-lisp-complete-symbol-partial-match)
          ;; (define-key lisp-interaction-mode-map "\C-\M-i"
          ;;   'anything-lisp-complete-symbol-partial-match)
          ;; (anything-read-string-mode 0)))))
          ))))

;;; RECENTF & IDO
;; (setq recentf-auto-cleanup 'never) ; WARNING: tramp issue
(recentf-mode 1)
(eval-after-load "recentf"
  '(progn
     (setq recentf-max-saved-items 1000
           recentf-max-menu-items 60
           recentf-keep '(file-remote-p file-readable-p))
     ;; open recent files according to history of mini-buffer (incl. files search
     ;; and management) or according to the list of recently loaded ones.
     (defun ido-recentf-file-name-history ()
       "Find a file in the `file-name-history' using ido for completion. Written by Markus Gattol."
       (interactive)
       (let* ((all-files (remove-duplicates (mapcar 'expand-file-name file-name-history) :test 'string=)) ; remove dups after expanding
              (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
              (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
              (ido-make-buffer-list-hook
               (lambda ()
                 (setq ido-temp-list filename-list)))
              (filename (ido-read-buffer "File History: "))
              (result-list (delq nil (mapcar (lambda (x) (if (string= (car x) filename) (cdr x))) file-assoc-list)))
              (result-length (length result-list)))
         (find-file
          (cond
           ((= result-length 0) filename)
           ((= result-length 1) (car result-list))
           (t (let ((ido-make-buffer-list-hook
                     (lambda () (setq ido-temp-list result-list))))
                (ido-read-buffer (format "%d matches:" result-length))))))))
     (defun ido-recentf ()
       "Use ido to select a recently opened file from the `recentf-list'. Written by xsteve."
       (interactive)
       (let ((home (expand-file-name (getenv "HOME"))))
         (find-file (ido-completing-read "Recent File: "
                                         (mapcar
                                          (lambda (path)
                                            (replace-regexp-in-string home "~" path)) recentf-list) nil t))))
     (defmacro mars/recentf/override-keys (map)
       "Force the keys overriding in some modes."
       (list 'lambda nil
             (list 'define-key map (list 'kbd '"C-c C-f") ''ido-recentf-file-name-history)
             (list 'define-key map (list 'kbd '"C-c F") ''ido-recentf)
             (list 'define-key map (list 'kbd '"C-c C-m") ''make-directory)))
     ;; (add-hook 'emacs-startup-hook (mars/recentf/override-keys global-map))
     ))

;;; CONFIGURATION
;; init / launch custom or creates a custom-file if none
(when custom-file
  (load custom-file 'noerror))
(defadvice custom-buffer-create (before my-advice-custom-buffer-create activate)
  "Exit the current Customize buffer before creating a new one, unless there are modified widgets."
  (if (eq major-mode 'Custom-mode)
      (let ((custom-buffer-done-kill t)
            (custom-buffer-modified nil))
        (mapc (lambda (widget)
                (and (not custom-buffer-modified)
                     (eq (widget-get widget :custom-state) 'modified)
                     (setq custom-buffer-modified t)))
              custom-options)
        (if (not custom-buffer-modified)
            (Custom-buffer-done)))))

;; confs' files
(let ((confs-to-load '(
                       "formats"        ; emacs as an universal typewriter (format + encodings)
                       "window-manager" ; emacs as a window-manager
                       "shortcuts"      ; emacs as a key commander
                       )))
  (when *emacs/normal-startup*
    (setq confs-to-load
          (append confs-to-load
                  '(
                    "crypto"       ; emacs as a secret agent
                    "gtd"          ; emacs as an planner
                    "www"          ; emacs as web browser / rss reader
                    "mail"         ; emacs as a MUA
                    "file-manager" ; emacs as a file-manager
                    "code"         ; emacs as a coding tool
                    "rectify"      ; emacs as a rectifier (error corrector/smart completion)
                    "version"      ; emacs as a vc tool
                    "vt"           ; emacs as a virtual terminal
                    "media"        ; emacs as a multimedia player (after 'w3m iff needed)
                    "toolbox"      ; emacs as a swiss army knife
                    )
                  (when *i-am-an-emacsen-dev*
                    '("elisp"        ; emacs as an emacs-lisp development tool (helpers inside)
                      "ladybug"))))) ; emacs as a fast hacking tool (temporary bugfixes inside)

  (mapc
   (lambda (x)
     (conf-load x))
   confs-to-load))

;; special init
;; - eshell path built from 'exec-path (which is set in Customize file)
(add-hook 'eshell-mode-hook
          '(lambda ()
             (setenv "PATH" (mapconcat (lambda (dir) (or dir ".")) exec-path path-separator))
             (setq eshell-path-env (getenv "PATH"))))

;; bytecompile .emacs and files in `conf-path' on change
(when (functionp 'byte-compile-user-init-file)
  (defun mars/byte-compile-user-init-hook ()
    (when (equal buffer-file-name user-init-file)
      (add-hook 'after-save-hook 'byte-compile-user-init-file t t)))
  (add-hook 'emacs-lisp-mode-hook 'mars/byte-compile-user-init-hook))
;; (when (functionp 'byte-compile-emacs-config)
;;  (defun mars/byte-compile-emacs-config-hook ()
;;    (when (member (file-name-directory buffer-file-name)
;;                  (mapcar '(lambda (x)
;;                             (expand-file-name
;;                              (file-name-as-directory
;;                               (concat (file-name-as-directory mars/local-root-dir) x))))
;;                          mars/local-conf-path))
;;      (add-hook 'after-save-hook 'byte-compile-emacs-config t t)))
;;  (add-hook 'emacs-lisp-mode-hook 'mars/byte-compile-emacs-config-hook))

;; restore windows archiver at startup
(add-hook 'emacs-startup-hook
          'mars-windows-archiver-load-in-session)

;; minibuffer histories
(savehist-mode 1)

;; desktop & autosave & backup files -> one place (like vim/backup)
;; - desktop directories
(setq desktop-dir (expand-file-name (concat (file-name-as-directory mars/local-root-dir)
                                            (file-name-as-directory mars/personal-data)
                                            mars/desktop-name))
      desktop-path (list desktop-dir)
      history-length 250)

(defmacro define-local-temporary-directory (local-work-directory-symbol)
  "Define the best temporary directory for autosaving or backing files up."
  (let ((local-tmp-dir (concat (file-name-as-directory mars/local-root-dir)
                               (file-name-as-directory mars/personal-data)
                               (file-name-as-directory "Temporary"))))
    (let ((dir-symbol (intern (concat (symbol-name local-work-directory-symbol) "-dir"))))
      (unless (symbol-value dir-symbol) ; creates directory iff unset (eg. `vars' may override)
                                        ; this <symbol>-dir must be 'NIL before compiling this macro
        (if (file-exists-p local-tmp-dir)
            `(setq ,dir-symbol ,(concat local-tmp-dir
                                        (file-name-as-directory
                                         (capitalize (concat
                                                      (symbol-name local-work-directory-symbol) "s")))))
          (let ((name (concat (file-name-as-directory mars/temporary-dir)
                              "emacs_" (symbol-name local-work-directory-symbol) "s/"
                              (file-name-as-directory (user-login-name)))))
            `(progn
               (setq ,dir-symbol ,name)
               (message ,(concat "Beware: your autosave directory named `" name
                                 "' may be publicly accessed. Be sure to make it hidden to other users.")))))))))
(define-local-temporary-directory autosave)
(define-local-temporary-directory backup)
(setq auto-save-file-name-transforms `(("\\([^/]*/\\)*\\(.*\\)" ,(concat autosave-dir "\\2") nil))
      backup-directory-alist (list (cons "." backup-dir)))
;; - desktop load
(when *emacs/normal-startup*
  (desktop-save-mode 1))
(make-directory autosave-dir t)         ; be sure it exists
(setq the-desktop-file (concat (file-name-as-directory desktop-dir)
                               desktop-base-file-name)
      the-desktop-lock (concat (file-name-as-directory desktop-dir)
                               desktop-base-lock-name))
(defun desktop-in-use-p ()
  (and (file-exists-p the-desktop-file) (file-exists-p the-desktop-lock)))
(defun autosave-desktop ()
  (when (desktop-in-use-p) (desktop-save-in-desktop-dir)))
(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;; - desktop autosave
(when *emacs/normal-startup*
  (setq mars/desktop-saver-timer
        (run-with-timer 5 300 'autosave-desktop)))
;; - unset temporary directory names
(setq autosave-dir nil
      backup-dir nil)                   ; otherwise `define-local-temporary-directory' compilation
                                        ; of the symbol test (in UNLESS clause) doesn't work

;; fast kill emacs or not but confirm anyway
(defadvice update-autoloads-in-package-area (around mars/fast-kill-version activate)
  (unless mars/fast-kill
    (progn
      ad-do-it)))
(setq confirm-kill-emacs 'y-or-n-p)

;; confirm deleting frame iff multiple windows or buffer property in `confirm-frame-action-buffer-alist'
(when (fboundp 'delete-frame)
  (defadvice delete-frame (around confirm-delete-frame
                                  (&optional frame force) activate)
    (if (< (length (frame-list)) 2)
        (kill-emacs)
      (let ((windows (window-list frame)))
        (if (> (length windows) 1)
            (if (y-or-n-p (format "Multiple windows in this frame detected. Close anyway? ")) (progn ad-do-it) nil)
          ;; one window case
          (let ((pending confirm-frame-action-buffer-alist)
                (buf (window-buffer (car windows)))
                found)
            (while (and (null found) pending)
              (let ((property (pop pending)))
                (when (member (with-current-buffer
                                  buf
                                (symbol-value (car property))) (cdr property))
                  (message (prin1-to-string property))
                  (setq found t))))
            (if (and found (not (y-or-n-p (format "Are you sure you want to delete this frame? "))))
                nil
              (progn ad-do-it))))))))
;; reload emacs
(defun reload-emacs ()
  (let ((memo *emacs/normal-startup*))
    (setq *emacs/normal-startup* t)
    (load user-init-file)
    (setq *emacs/normal-startup* memo)))

;; time
(let ((load-time (destructuring-bind (hi lo ms) (current-time)
                   (- (+ hi lo) (+ (first emacs-load-start)
                                   (second emacs-load-start))))))
  (message "Emacs loaded in %ds" load-time)
  (when (and *i-am-an-emacsen-dev* (fboundp 'display-external-pop-up))
    (display-external-pop-up "Emacs startup"
                             (concat "Emacs loaded in "
                                     (number-to-string load-time) "s"))))
(put 'narrow-to-region 'disabled nil)
