;;; vim-everywhere.el ---
;;
;; Filename: vim-everywhere.el
;; Description: Vim Emulation Setup
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 18:19:43 2011 (+0100)
;; Version: 0.6.2-linum-settings
;; Last-Updated: Mon Dec  2 11:36:16 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 420
;; URL: 
;; Keywords: 
;; Compatibility: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: Evil + your Vim colorscheme in Emacs
;;
;; keep (quite) same syntax highlighting everywhere
;; by hondana@gmx.com 2001-2013
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar *vim-now* t) ; vemacs case
(provide 'emulation-context)
(require 'preamble)

;; (unless (fboundp 'mars/add-to-load-path)
;;   (let ((local-site-lisp-path (mapcar #'(lambda (x) (concat (file-name-as-directory mars/local-root-dir)
;;                                                            (file-name-as-directory x))) mars/site-lisp-path)))
;;     (setq load-path (append local-site-lisp-path load-path))
;;     (dolist (local-site-lisp local-site-lisp-path)
;;       (mapc #'(lambda (x)
;;                 (let ((found-dirs (directory-files local-site-lisp t (symbol-name x))))
;;                   (when found-dirs
;;                     (mapc #'(lambda (found-dir)
;;                               (save-excursion
;;                                 (condition-case nil
;;                                     (progn
;;                                       (cd found-dir)
;;                                       (push (expand-file-name found-dir) load-path))
;;                                   (error nil))))
;;                           found-dirs))))
;;             '(color-theme evil))))
;;   (load-library "color-theme-autoloads"))


;;; VIPER CASE IF REQUIRED
(defvar viper-custom-file-name nil
  "Viper file name")
(defvar viper-toggle-key nil
  "Viper key to switch between Emacs and Vi Normal modes")
(defvar viper-mode nil
  "Boolean flag to run Viper or not")
(let ((vi-conf (conf-locate (concat (user-login-name) ".viper"))))
  (if vi-conf
      (setq viper-custom-file-name (convert-standard-filename vi-conf))
    (let ((default-conf (conf-locate "default.viper")))
      (when default-conf
        (setq viper-custom-file-name (convert-standard-filename default-conf))))))
;; (setq viper-toggle-key "\C-x\C-z" ; no more C-z -- 'minimize' then works on Mac
;;       viper-mode t)
(eval-after-load "viper"                ; UNUSED but kept for 'viper user
  '(progn
     ;; 0- tweaks
     (define-key viper-vi-global-user-map [(control kp-delete)] nil)
     ;; 1- ruby case
     (add-to-list 'viper-vi-state-mode-list 'ruby-mode)
     ;; 2- autopair case
     (eval-after-load "autopair"
       '(progn
          (when (locate-library "autopair-viper-compat")
            (require 'autopair-viper-compat))))
     ;; 3- additional keyboard bindings
     (define-key viper-vi-global-user-map [(kp-delete)] 'viper-forward-char)
     (define-key viper-insert-basic-map [(kp-delete)] 'viper-delete-char)
     (define-key viper-vi-global-user-map [(control backspace)] 'viper-backward-word)
     (define-key viper-insert-basic-map [(control backspace)] 'viper-delete-backward-word)
     ;; idea: http://stackoverflow.com/users/2797/sebastien-roccaserra
     (define-key viper-vi-global-user-map "/"        'isearch-forward-regexp)
     (define-key viper-vi-global-user-map "?"        'isearch-backward-regexp)
     (define-key viper-vi-global-user-map "\C-wh"    'windmove-left)
     (define-key viper-vi-global-user-map "\C-wj"    'windmove-down)
     (define-key viper-vi-global-user-map "\C-wk"    'windmove-up)
     (define-key viper-vi-global-user-map "\C-wl"    'windmove-right)
     (define-key viper-vi-global-user-map "\C-wv"    '(lambda () (interactive)
                                                        (split-window-horizontally)
                                                        (other-window 1)
                                                        (switch-to-buffer (other-buffer))))
     (define-key viper-insert-basic-map "\C-e" nil) ; IMPORTANT: in order to use ASCII C-a/C-e in insert mode
     (define-key viper-vi-basic-map "\C-e" 'viper-scroll-up-one)
     (push '("only"  (delete-other-windows)) ex-token-alist) ; on  in Evil
     (push '("close" (delete-window))        ex-token-alist) ; clo in Evil
     (define-key viper-vi-global-user-map " d" 'viper-kill-buffer)
     (when *i-am-a-terminator*
       ;; global-unset-key "\C-h"
       (define-key viper-vi-global-user-map "\C-h" 'viper-backward-char)
       (define-key viper-insert-global-user-map "\C-h" 'viper-delete-backward-char))
     ;; 4- colorize <> modes
     (setq viper-vi-state-id
           (concat (propertize "<V>" 'face 'font-lock-string-face) " ")
           viper-insert-state-id
           (concat (propertize "<I>" 'face 'font-lock-string-face) " ")
           viper-replace-state-id
           (concat (propertize "<R>" 'face 'font-lock-string-face) " "))
     (require-if-located 'hi-lock)
     (setq viper-emacs-state-id
           (concat (propertize "<E>" 'face 'hi-red-b) " "))
     (put 'viper-mode-string 'risky-local-variable t)))

;;; EVIL (VIPER FREE)
(require-if-located 'evil)
;; add C-w outside Evil -- eg. C-w C-w -> other-window
(eval-after-load "evil"
  '(progn
     ;; 0- Evil, don't touch my cursor, will you?!
     (setq evil-default-cursor t)
     ;; 1- boot Evil & friends properly
     (if-bound-call viper-go-away)      ; FIXME: missing hooks in Evil? to shutdown viper on turn-on
     (evil-mode 1)
     ;; enable C-x C-z as evil-mode toggle key
     (when (window-system)
       (eval-after-load "escreen"
         '(progn
            (evil-set-toggle-key "C-x C-z") ; unset C-z
            (setq escreen-prefix-char "\C-z")
            ;; fast escreen keybindings for Dvorak typists
            ;; (using bottom right diamond combination)
            (when *i-am-a-dvorak-typist*
              (define-key escreen-map (kbd "C--") 'escreen-goto-next-screen)
              (define-key escreen-map "\C-v" 'escreen-goto-prev-screen)
              (define-key escreen-map "\C-s" 'escreen-menu))
            (global-set-key escreen-prefix-char 'escreen-prefix)))
       (eval-after-load "elscreen"
         '(progn
            (evil-set-toggle-key "C-x C-z") ; unset C-z
            (elscreen-set-prefix-key "\C-z"))))
     (require-if-located 'evil-leader)
     (eval-after-load "evil-leader"
       ;; cf. 'SHORTCUTS to customize 'EVIL-LEADER
       '(progn
          ;; FIXME: https://github.com/cofi/evil-leader/issues/7
          (add-hook 'emacs-startup-hook 'global-evil-leader-mode)
          (evil-leader/set-leader ","))) ; default is ,
     (require-if-located 'surround)      ; via evil-surround
     (eval-after-load "surround"
       '(global-surround-mode 1))
     (require-if-located 'evil-numbers)
     (eval-after-load "evil-numbers"
       '(progn
          (define-key evil-normal-state-map "\C-c+" 'evil-numbers/inc-at-pt)
          (define-key evil-normal-state-map "\C-c-" 'evil-numbers/dec-at-pt)))
     ;; manage special modes where Emacs state should be activated
     (defmacro mars/set-evil-emacs-in-modes (&rest mode-list)
       `(progn
          ,@(mapcar #'(lambda (x)
                        `(evil-set-initial-state ',x 'emacs)) mode-list)))
     (mars/set-evil-emacs-in-modes wl-summary-mode
                                   bbdb-mode
                                   shell-mode eshell-mode
                                   magit-status-mode magit-log-edit-mode)
     (eval-after-load "wl-folder"       ; FIXME: evil-set-initial-state fails!
       `(add-hook 'wl-folder-mode-hook #'evil-emacs-state))
     ;; manage special modes where 'C-w' should be activated
     (fset 'evil-like-window-map (copy-keymap evil-window-map))
     (defmacro mars/activate-C-w-in-modes (&rest context-mode-alist)
       `(progn
          ,@(mapcar #'(lambda (x)
                        `(mars/activate-state-in-modes ,(cadr x)
                                                       ,(car x)
                                                       define-key
                                                       "\C-w" 'evil-like-window-map))
                    context-mode-alist)))
     (mars/activate-C-w-in-modes (dired (dired-mode-map))
                                 (ibuffer (ibuffer-mode-map))
                                 (org-agenda (org-agenda-mode-map)))

     ;; 2- parenface to add a default color to parentheses as Vim does
     (if (locate-library "hl-line+")
         (global-hl-line-mode)
       (progn
         (defface hl-line '((t (:background "grey10"))) "Dummy hl-line.")
         (setq hl-line-face 'hl-line)))
     (require-if-located 'parenface)

     ;; 3- TODO: nothing to add (mis)match parentheses -- check 'show-parens
     ;;    so there's no need to add 'mic-paren

     ;; 4- line numbering
     (require-if-located 'linum+)
     (eval-after-load "linum"
       '(progn
          (defun am-add-hooks (hooks function &optional append local)
            "Call `add-hook' on hook list HOOKS use arguments FUNCTION, APPEND, LOCAL.

HOOKS can be one list or just a hook."
            (if (listp hooks)
                (mapc
                 `(lambda (hook)
                    (add-hook hook ',function append local))
                 hooks)
              (add-hook hooks function append local)))
          (am-add-hooks
           `(find-file-hook help-mode-hook Man-mode-hook log-view-mode-hook chart-mode-hook
                            compilation-mode-hook gdb-mode-hook lisp-interaction-mode-hook
                            browse-kill-ring-mode-hook completion-list-mode-hook hs-hide-hook
                            inferior-ruby-mode-hook custom-mode-hook Info-mode-hook
                            svn-log-edit-mode-hook package-menu-mode-hook dired-mode-hook
                            apropos-mode-hook svn-log-view-mode-hook diff-mode-hook
                            emacs-lisp-mode-hook ibuffer-mode-hook html-mode-hook
                            w3m-mode-hook data-debug-hook debugger-mode-hook text-mode-hook
                            color-theme-mode-hook semantic-symref-results-mode-hook
                            sh-mode-hook groovy-mode-hook)
           (lambda()
             (unless (eq major-mode 'image-mode)
               (linum-on))))))

     ;; 5- colorize numbers, todos & warnings
     (defface font-lock-number-face
       '((((type tty) (class color)) (:foreground "pink"))
         (((type tty) (class mono))  (:inverse-video t))
         (((class color) (background dark))  (:foreground "pink"))
         (((class color) (background light)) (:foreground "grey10"))
         (t ()))
       "Face name to use for numbers."
       :group 'basic-faces)
     (defvar font-lock-number-face 'font-lock-number-face)
     (defun font-lock-fontify-numbers ()
       "Hook function to `font-lock-number-face'-ify numbers."
       (font-lock-add-keywords nil
                               '(("[^a-zA-Z_]\\(0x[0-9a-fA-F]+\\)"     1 font-lock-number-face) ; hexa
                                 ("[^a-zA-Z_]\\(-?[0-9]+\\.[0-9]+\\)"  1 font-lock-number-face) ; float
                                 ("[^a-zA-Z_1-9-]\\(-?[0-9]+U?L?L?\\)" 1 font-lock-number-face)))) ; int
     (defface font-lock-todo-face
       '((((type tty) (class color)) (:foreground "yellow"))
         (((type tty) (class mono))  (:underline t))
         (((class color) (background dark))  (:foreground "gold"))
         (((class color) (background light)) (:foreground "goldenrod"))
         (t ()))
       "Face name to use for todos."
       :group 'basic-faces)
     (defvar font-lock-todo-face 'font-lock-todo-face)
     (defface font-lock-notify-face
       '((((type tty) (class color)) (:foreground "blue"))
         (((type tty) (class mono))  (:inverse-video t :underline t))
         (((class color) (background dark))  (:foreground "DarkSlateGray1"))
         (((class color) (background light)) (:foreground "DarkSlateGray4"))
         (t ()))
       "Face name to use for notifications. As `font-lock-warning-face' may be used as Vim's
ErrorMsg al alternative, Vim's WarningMsg may be mapped to this face."
       :group 'basic-faces)
     (defvar font-lock-notify-face 'font-lock-notify-face)
     (dolist (mode '(lisp-mode
                     emacs-lisp-mode
                     lisp-interaction-mode
                     scheme-mode
                     shen-mode
                     qi-mode
                     c-mode
                     cc-mode
                     js-mode
                     espresso-mode
                     shell-script-mode
                     ruby-mode
                     python-mode
                     php-mode
                     java-mode
                     cperl-mode
                     howm-mode
                     org-mode
                     haskell-mode
                     smalltalk-mode
                     factor-mode
                     erlang-mode
                     caml-mode
                     clojure-mode))     ; TODO: mix in code.el
       (progn
         (add-hook (intern (concat (symbol-name mode) "-hook"))
                   #'font-lock-fontify-numbers)
         (font-lock-add-keywords mode
                                 '(("\\(FIXME:\\|TODO:\\)"
                                    1 font-lock-builtin-face prepend))) ; FIXME: builtin here b/c todo is too grayish in `Wombat256mod'
         (font-lock-add-keywords mode
                                 '(("\\(IMPORTANT:\\|WARNING:\\|NOTE:\\|UNTESTED\\|DEPRECATED\\)"
                                    1 font-lock-notify-face prepend)))
         (font-lock-add-keywords mode
                                 '(("\\(ERROR:\\|XXX\\|OBSOLETE\\)"
                                    1 font-lock-warning-face prepend)))))

     ;; 6- additional keyboard bindings
     ;; NOTE: Evil manages C-h correctly
     (define-key evil-insert-state-map (kbd "C-,") 'evil-copy-from-below)
     (define-key evil-visual-state-map "F" 'evil-find-char-backward)
     (define-key evil-visual-state-map "t" 'evil-forward-char-forward)
     (define-key evil-visual-state-map "T" 'evil-backward-char)
     (define-key evil-visual-state-map "e" '(lambda ()
                                              (interactive)
                                              (evil-forward-word-end)
                                              (evil-forward-char)))
     (define-key evil-normal-state-map " d" 'kill-buffer)

     ;; 7- colorize status bar
     (lexical-let ((default-color (cons (face-background 'mode-line)
                                        (face-foreground 'mode-line))))
       (add-hook 'post-command-hook
                 (lambda ()
                   (let ((color (cond ((minibufferp) default-color)
                                      ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                      ((evil-visual-state-p) '("#666666" . "#ffffff"))
                                      ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                      ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                      (t default-color))))
                     (set-face-background 'mode-line (car color))
                     (set-face-foreground 'mode-line (cdr color))))))

     (unless (and (< emacs-major-version 24)
                  (not (fboundp 'color-theme-install)))
       ;; choose a theme according to your Vim setup -- ~/.vimrc by default
       (defvar *theme-header* "custom-vim-colorscheme-"
         "The string to add as a custom/color-theme prefix.")

       (defvar *chosen-theme* nil
         "The lambda to draw.")

       (defun remove-last-unwanted-char (lookup-string)
         "Remove -, _, ? or ! characters at the end of a string."
         (while (and
                 (> (length lookup-string) 0)
                 (string-match-p (substring lookup-string -1) "-_?!=")) ; keep '+'
           (setq lookup-string (substring lookup-string 0 -1)))
         lookup-string)

       (defun compose-color-theme-name (theme-name)
         "Make a lispian name for the color theme function."
         (if (or (null theme-name)
                 (< (length theme-name) 2))
           ""                                ; no short name
           (cl-flet ((replace-underscore (string-to-replace)
                                         (replace-regexp-in-string
                                          (char-to-string ?_)
                                          (char-to-string ?-)
                                          (remove-last-unwanted-char string-to-replace))))
             (concat *theme-header* (replace-underscore theme-name)))))

       (defun colorscheme-to-color-theme (colorscheme-name)
         " Returns a color theme function in a lambda from a colorscheme name. Works
if the function exists."
         (let ((theme (if (= (length colorscheme-name) 0)
                          nil
                        (intern (compose-color-theme-name colorscheme-name)))))
           (if (functionp theme)
               theme
             nil)))

       (defun mars/extract-colorscheme-from-vimrc (&optional fpath)
         "Check the contents of the vimrc file to get the same theme in emacs.
TODO: case of '''colorscheme' this'' where this is
''this = my_colorscheme''."
         (let ((vimfile (or fpath "~/.vimrc")))
           (if (file-exists-p vimfile)
               ;; locate used colorscheme in your .vimrc
               (with-temp-buffer
                 (insert-file-contents vimfile)
                 (goto-char (point-max))                       ; backward searching
                 (makunbound 'vim-colorscheme-used)
                 (while (not (boundp 'vim-colorscheme-used))
                   (let ((mat
                          (re-search-backward
                           "[^i]colorscheme \\\([-_A-Za-z0-9]+\\\)" nil t))) ; don't get guicolorscheme
                     (if (not (null mat))
                         (let ((str (match-string 1)))
                           (save-excursion
                             (save-restriction ; FIXME: write it w/o narrowing using
                                        ; a 'end instead of nil in 're-search-forward
                               (let ((pt (point)))
                                 (beginning-of-line)
                                 (narrow-to-region (point) pt)
                                 ;; " is a vimscript comment
                                 (if (not (re-search-forward "\"" nil t))
                                     (setq vim-colorscheme-used str))))))
                       (progn
                         (message "vim-theme::Vim colorscheme: not found at all")
                         (setq vim-colorscheme-used nil)))))
                 (if (stringp vim-colorscheme-used)
                     (let ((found-theme (colorscheme-to-color-theme vim-colorscheme-used)))
                       (message "vim-theme::Your Vim setting file calls \"%s\" as colorscheme" vim-colorscheme-used)
                       (if (functionp found-theme)
                           (progn
                             (setq *chosen-theme* found-theme)
                             (message "vim-theme::loading..."))
                         (message "vim-theme::no corresponding color-theme found; default one loading..."))))))))

       ;; defcolor = Vim-like color-theme
       ;; NOTE: - parenface added to customize parentheses' color
       ;;       - numbers colors added
       ;;       - recommended parentheses matching is 'mic-paren
       ;;         (paren-face-(mis)match) but Evil uses its
       ;;         own matcher based on 'show-paren

       (defmacro defcolor-theme (name &rest colors)
         "Define a color theme and provide it."
         (let ((funsym (intern (compose-theme-name name))))
           `(progn
              (defun ,funsym ()
                (interactive)
                (color-theme-install
                 '(,funsym
                   ,@colors)))
              (provide ',funsym))))

       ;; molokai theme (https://github.com/hbin/molokai-theme/blob/master/molokai-theme.el)
       (defun custom-vim-colorscheme-molokai ()
         (interactive)
         (let ((class '((class color) (min-colors 89)))
               ;; molokai palette
               (molokai-white          "#ffffff")
               (molokai-fg             "#f8f8f0")
               (molokai-red            "#ff0000")
               (molokai-pink           "#f92672")
               (molokai-orange+5       "#ef5939")
               (molokai-orange         "#fd971f")
               (molokai-yellow         "#ffff00")
               (molokai-darkgoldenrod  "#e6db74")
               (molokai-wheat          "#c4be89")
               (molokai-olive          "#808000")
               (molokai-chartreuse     "#a6e22e")
               (molokai-lime           "#00ff00")
               (molokai-green          "#008000")
               (molokai-darkwine       "#1e0010")
               (molokai-maroon         "#800000")
               (molokai-wine           "#960050")
               (molokai-teal           "#008080")
               (molokai-aqua           "#00ffff")
               (molokai-blue           "#66d9ef")
               (molokai-slateblue      "#7070f0")
               (molokai-purple         "#ae81ff")
               (molokai-palevioletred  "#d33682")
               (molokai-grey-2         "#bcbcbc")
               (molokai-grey-1         "#8f8f8f")
               (molokai-grey           "#808080")
               (molokai-grey+2         "#403d3d")
               (molokai-grey+3         "#4c4745")
               (molokai-grey+5         "#232526")
               (molokai-bg             "#1b1d1e")
               (molokai-grey+10        "#080808")
               (molokai-dark           "#000000")
               (molokai-base01         "#465457")
               (molokai-base02         "#455354")
               (molokai-base03         "#293739")
               (molokai-dodgerblue     "#13354a"))
           (color-theme-install
            `(,(intern (compose-theme-name "molokai"))
              ;; base
              ((foreground-color . ,molokai-fg)  ; +Normal guifg
               (background-color . ,molokai-bg)  ; +Normal guibg
               (background-mode . dark))         ; color orientation
              (default ((t (:background ,molokai-bg :foreground ,molokai-fg))))
              (cursor ((t (:background ,molokai-fg :foreground ,molokai-bg))))
              (fringe ((t (:foreground ,molokai-base02 :background ,molokai-bg))))
              (highlight ((t (:background ,molokai-grey))))
              (region ((t (:background  ,molokai-grey+2))
                       (t :inverse-video t)))
              (warning ((t (:foreground ,molokai-palevioletred :weight bold))))
              ;; font lock
              (font-lock-builtin-face ((t (:foreground ,molokai-chartreuse))))
              (font-lock-comment-face ((t (:foreground ,molokai-base01))))
              (font-lock-comment-delimiter-face ((t (:foreground ,molokai-base01))))
              (font-lock-constant-face ((t (:foreground ,molokai-purple))))
              (font-lock-doc-string-face ((t (:foreground ,molokai-darkgoldenrod))))
              (font-lock-function-name-face ((t (:foreground ,molokai-chartreuse))))
              (font-lock-keyword-face ((t (:foreground ,molokai-pink :weight bold))))
              (font-lock-negation-char-face ((t (:foreground ,molokai-wine))))
              (font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
              (font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
              (font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
              (font-lock-string-face ((t (:foreground ,molokai-darkgoldenrod))))
              (font-lock-type-face ((t (:foreground ,molokai-blue :weight bold))))
              (font-lock-variable-name-face ((t (:foreground ,molokai-orange))))
              (font-lock-warning-face ((t (:foreground ,molokai-palevioletred :weight bold))))
              ;; bonus faces = number & notify & todo
              (font-lock-number-face ((t (:inherit font-lock-constant-face))))
              (font-lock-negation-char-face ((t (:inherit font-lock-number-face))))
              (font-lock-notify-face ((t (:inherit font-lock-warning-face))))
              (font-lock-todo-face ((t (:foreground ,molokai-base02 :italic t :slant italic))))
              ;; mode line
              (mode-line ((t (:foreground ,molokai-fg
                                          :background ,molokai-base03
                                          :box nil))))
              (mode-line-buffer-id ((t (:weight bold))))
              (mode-line-inactive ((t (:foreground ,molokai-fg
                                                   :background ,molokai-base02
                                                   :box nil))))
              ;; search
              (isearch ((t (:foreground ,molokai-dark :background ,molokai-wheat :weight bold))))
              (isearch-lazy-highlight-face ((t (:foreground ,molokai-base02 :background ,molokai-fg))))
              (isearch-fail ((t (:foreground ,molokai-wine :background ,molokai-darkwine))))
              ;; linum-mode
              (linum ((t (:foreground ,molokai-grey-2 :background ,molokai-grey+5))))
              ;; hl-line-mode
              (hl-line-face ((,class (:background ,molokai-grey+5)) (t :weight bold)))
              (hl-line ((,class (:background ,molokai-grey+5)) (t :weight bold)))
              ;; ido-mode
              ;; flycheck
              ;; paren-face
              (paren-face ((t (:inherit font-lock-builtin-face ))))
              ;; show-paren
              (show-paren-match-face
               ((t (:foreground ,molokai-dark :background ,molokai-orange :bold t)))) ; +MatchParen
              (show-paren-match-face
               ((t (:foreground ,molokai-pink :background ,molokai-grey+5 :bold t)))) ; +ErrorMsg
              ;; auto-complete
              (ac-candidate-face ((t (:foreground ,molokai-blue :background ,molokai-dark)))) ; Pmenu
              (ac-selection-face ((t (:foreground ,molokai-fg :background ,molokai-grey)))) ; PmenuSel
              ;; ediff
              (ediff-current-diff-A ((t (:foreground ,molokai-grey-1 :background ,molokai-grey+3)))) ; +DiffChange
              (ediff-fine-diff-A ((t (:background ,molokai-grey+3 :italic t :bold t)))) ; +DiffText
               ;; rainbow-delimiters
               ;; highlight-symbols
               ))))

       ;; wombat colorscheme 256 mod (2010/07/23 version) Liang/Bespalov/Nielsen (dengmao@gmail.com)
       (defcolor-theme "wombat256mod"
         ((foreground-color . "#e3e0d7")  ; +Normal guifg
          (background-color . "#242424")  ; +Normal guibg
          (background-mode . dark))       ; color orientation
         (default ((((class color) (min-colors 4096)) (:foreground "#e3e0d7" :background "#242424"))
                   (((class color) (min-colors 256)) (:foreground "#d0d0d0" :background "#1c1c1c")))) ; +Normal
         (border ((((class color) (min-colors 4096)) (:background "#32322f"))
                  (((class color) (min-colors 256)) (:background "#303030")))) ; +CursorLine
         (cursor ((((class color) (min-colors 4096)) (:foreground "#242424" :background "#eae788"))
                  (((class color) (min-colors 256)) (:foreground "#1c1c1c" :background "#ffff87")))) ; +Cursor
         (region ((((class color) (min-colors 4096)) (:foreground "#c3c6ca" :background "#554d4b"))
                  (((class color) (min-colors 256)) (:foreground "#c6c6c6" :background "#303030")))) ; +Visual
         (highlight ((((class color) (min-colors 4096)) (:background "#32322f"))
                     (((class color) (min-colors 256)) (:background "#303030")))) ; +CursorLine
         (hl-line ((((class color) (min-colors 4096)) (:background "#32322f"))
                   (((class color) (min-colors 256)) (:background "#303030")))) ; +CursorLine
         (fringe ((((class color) (min-colors 4096)) (:foreground "#857b6f" :background "#080808"))
                  (((class color) (min-colors 256)) (:foreground "#626262" :background "#080808")))) ; +LineNr
         (paren-face ((((class color) (min-colors 4096)) (:foreground "#eadead"))
                      (((class color) (min-colors 256)) (:foreground "#ffffaf")))) ; +Special
         (show-paren-match-face
          ((((class color) (min-colors 4096)) (:foreground "#eae788" :background "#857b6f" :bold t))
           (((class color) (min-colors 256)) (:foreground "#ffff87" :background "#87875f" :bold t)))) ; +MatchParen
         (show-paren-mismatch-face
          ((((class color) (min-colors 4096)) (:foreground "#ff2026" :background "#3a3a3a" :bold t))
           (((class color) (min-colors 256)) (:foreground "#ff0000" :background "#303030" :bold t)))) ; +ErrorMsg
         (isearch ((t (:inverse-video t :bold nil :underline nil)))) ; +Search | (inverse-video)
         (isearch-lazy-highlight-face ((t (:inherit font-lock-comment-face :inverse-video t))))
         (modeline
          ((((class color)) (:italic t   :bold nil :foreground "#ffffd7" :background "#444444")))) ; +StatusLine
         (modeline-inactive
          ((((class color) (min-colors 4096)) (:italic nil :bold nil :foreground "#857b6f" :background "#444444"))
           (((class color) (min-colors 256))  (:italic nil :bold nil :foreground "#626262" :background "#444444")))) ; +StatusLineNC | User2
         (modeline-buffer-id
          ((((class color)) (:italic t :slant italic :bold nil :foreground "#ffffd7" :background "#444444")))) ; +StatusLine
         (minibuffer-prompt
          ((((class color)) (:bold t :foreground "#ffffd7")))) ; +Title | User2 guifg
         (font-lock-builtin-face
          ((((class color) (min-colors 4096)) (:foreground "#eadead"))
           (((class color) (min-colors 256)) (:foreground "#ffffaf")))) ; +Special
         (font-lock-comment-face
          ((((class color) (min-colors 4096)) (:foreground "#9c998e" :italic t :slant italic))
           (((class color) (min-colors 256)) (:foreground "#949494"  :italic t :slant italic)))) ; +Comment
         (font-lock-comment-delimiter-face
          ((((color class)) (:inherit font-lock-comment-face))))
         (font-lock-constant-face
          ((((class color) (min-colors 4096)) (:foreground "#e5786d"))
           (((class color) (min-colors 256)) (:foreground "#d7875f")))) ; +Constant
         (font-lock-preprocessor-face
          ((((class color) (min-colors 4096)) (:foreground "#e5786d"))
           (((class color) (min-colors 256)) (:foreground "#d7875f")))) ; +PreProc
         (font-lock-function-name-face
          ((((class color) (min-colors 4096)) (:foreground "#cae982"))
           (((class color) (min-colors 256)) (:foreground "#d7ff87")))) ; +Function | Normal guifg (bold)
         (font-lock-variable-name-face
          ((((class color) (min-colors 4096)) (:foreground "#cae982"))
           (((class color) (min-colors 256)) (:foreground "#d7ff87")))) ; +Identifier
         (font-lock-keyword-face
          ((((class color) (min-colors 4096)) (:foreground "#88b8f6"))
           (((class color) (min-colors 256)) (:foreground "#87afff")))) ; +Keyword | Statement
         (font-lock-reference-face
          ((((class color) (min-colors 4096)) (:foreground "#88b8f6"))
           (((class color) (min-colors 256)) (:foreground "#87afff")))) ; +Statement
         (font-lock-string-face
          ((((class color) (min-colors 4096)) (:italic t :slant italic :background nil :foreground "#95e454"))
           (((class color) (min-colors 256))  (:italic t :slant italic :background nil :foreground "#87d75f")))) ; +String
         (font-lock-doc-face
          ((((color class)) (:inherit font-lock-string-face))))
         (font-lock-type-face
          ((((class color) (min-colors 4096)) (:foreground "#d4d987"))
           (((class color) (min-colors 256)) (:foreground "#d7d787")))) ; +Type
         (font-lock-warning-face         ; as Error
          ((((class color) (min-colors 4096)) (:foreground "#ff2026" :background "#3a3a3a" :bold t))
           (((class color) (min-colors 256)) (:foreground "#ff0000" :background "#303030" :bold t)))) ; ErrorMsg
         ;; bonus faces = number & notify & todo
         (font-lock-number-face
          ((((class color) (min-colors 4096)) (:foreground "#e5786d"))
           (((class color) (min-colors 256)) (:foreground "#d7875f")))) ; +Number
         (font-lock-negation-char-face
          ((((class color)) (:inherit font-lock-number-face))))
         (font-lock-notify-face          ; as Warning
          ((((class color)) (:foreground "#ff5f55"))))                  ; WarningMsg
         (font-lock-todo-face            ; as Todo
          ((((class color) (min-colors 4096)) (:foreground "#857b6f" :italic t :slant italic))
           (((class color) (min-colors 256)) (:foreground "#87875f" :italic t :slant italic)))) ; +Todo
         ;; auto-complete
         (ac-candidate-face
          ((((class color)) (:foreground "#ffffd7" :background "#444444")))) ; Pmenu
         (ac-selection-face
          ((((class color) (min-colors 4096)) (:foreground "#080808" :background "#cae982"))
           (((class color) (min-colors 256)) (:foreground "#080808" :background "#d7ff87")))) ; PmenuSel
         ;; ediff faces
         (ediff-current-diff-A ((((class color) (min-colors 4096))
                                 (:background "#382a37"))
                                (((class color) (min-colors 256))
                                 (:background "3a3a3a"))
                                (t (:inverse-video t)))) ; +DiffChange
         (ediff-fine-diff-A ((((class color) (min-colors 4096))
                              (:background "#73186e"))
                             (((class color) (min-colors 256))
                              (:background "5f005f"))
                             (t (:inverse-video t))))) ; +DiffText

       ;; inkpot colorscheme from `github.com/ciaranm/inkpot' TODO: 256 colors' version
       (defcolor-theme "inkpot"
         ((foreground-color . "#cfbfad")  ; +Normal guifg
          (background-color . "#1e1e27")  ; +Normal guibg
          (border-color . "#2e2e37")      ; +CursorLine
          (cursor-color . "#8b8bff")      ; +Cursor guibg
          (background-mode . dark))       ; color orientation
         (region ((t (:foreground "#eeeeee" :background "#4e4e8f")))) ; +Visual
         (highlight ((t (:background "#2e2e37")))) ; +CursorLine
         (hl-line   ((t (:background "#2e2e37")))) ; +CursorLine
         (fringe ((t (:foreground "#8b8bcd" :background "#2e2e2e")))) ; +LineNr
         (paren-face ((t (:foreground "#c080d0")))) ; +Special
         (show-paren-match-face ((t (:foreground "#cfbfad" :background "#4e4e8f" :bold nil)))) ; +MatchParen
         (show-paren-mismatch-face
          ((((class color) (min-colors 4096)) (:foreground "#ffffff" :background "#6e2e2e" :bold t))
           (((class color) (min-colors 256)) (:foreground "#5fd7af" :background "#0087df" :bold t)))) ; +Error
         (isearch ((t (:bold t :foreground "#303030" :background "#ad7b57")))) ; +Search
         (modeline ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e")))) ; +StatusLine
         (modeline-inactive ((t (:foreground "#708090" :background "#3e3e5e")))) ; +StatusLineNC | User2
         (modeline-buffer-id ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e")))) ; +StatusLine
         (minibuffer-prompt ((t (:bold t :foreground "#7070a0")))) ; +User2 guifg
         (ediff-current-diff-A ((((class color) (min-colors 16))
                                 (:foreground "#ffffcd" :background "#306b8f"))
                                (((class color)) (:foreground "white" :background "blue3"))
                                (t (:inverse-video t)))) ; +DiffChange
         (ediff-fine-diff-A ((((class color) (min-colors 16))
                              (:foreground "#ffffcd" :background "#4a2a4a"))
                             (((class color)) (:foreground "white" :background "green4"))
                             (t (:inverse-video t)))) ; +DiffText
         (font-lock-builtin-face ((t (:foreground "#c080d0")))) ; +Special
         (font-lock-comment-face ((t (:foreground "#cd8b00" :italic nil)))) ; +Comment
         (font-lock-constant-face ((t (:foreground "#ffcd8b")))) ; +Constant
         (font-lock-doc-face ((t (:foreground "#cd8b00" :italic nil)))) ; +Comment
         (font-lock-function-name-face ((t (:foreground "#cfbfad" :bold t)))) ; +Normal guifg (bold)
         (font-lock-keyword-face ((t (:foreground "#808bed")))) ; +Statement
         (font-lock-preprocessor-face ((t (:foreground "#409090")))) ; +PreProc
         (font-lock-number-face ((t (:foreground "#f0ad6d")))) ; +Number
         (font-lock-reference-face ((t (:bold t :foreground "#808bed")))) ; +Statement
         (font-lock-string-face ((t (:foreground "#ffcd8b" :background "#404040" :italic nil)))) ; +String
         (font-lock-type-face ((t (:foreground "#ff8bff")))) ; +Type
         (font-lock-variable-name-face ((t (:foreground "#ff8bff")))) ; +Identifier
         (font-lock-warning-face
          ((((class color) (min-colors 4096)) (:foreground "#ffffff" :background "#6e2e2e" :bold t))
           (((class color) (min-colors 256)) (:foreground "#5fd7af" :background "#0087df" :bold t))))) ; +Error

       ;; fetch your vim colorscheme and load a mockup adapter
       (setq *chosen-theme* (colorscheme-to-color-theme "molokai")) ; default theme
       ;; or (setq *chosen-theme* (colorscheme-to-color-theme "wombat256mod"))
       ;; or (setq *chosen-theme* (colorscheme-to-color-theme "inkpot"))

       (mars/extract-colorscheme-from-vimrc)
       (funcall *chosen-theme*)) ;; FIXME: slow down

     ;; 8- open the current buffer in Vim (when Emacs modal editing comes short)
     (defun open-with-vim ()
       "Open current buffer with Vim. To ensure buffers synchronization, set 'GLOBAL-AUTO-REVERT-MODE to T."
       (interactive)
       (let* ((os (symbol-name system-type))
              (vim-name (if (string-match "Darwin" os)
                            "mvim"
                          (if (string-match "^Windows.*" os)
                              "gvim.exe"
                            "gvim"))))
         (message "Open %s with %s..." buffer-file-name vim-name)
         (shell-command (concat vim-name " " buffer-file-name))))
     (global-set-key '[(meta \0)] 'open-with-vim)))

;; - some keybindings
;; remember:
;; C-w = Window manipulation in normal modes; a kind of `DELETE-BACKWARD-WORD' elsewhere

(provide 'vim-everywhere)
(unintern 'emulation-context obarray)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vim-everywhere.el ends here
