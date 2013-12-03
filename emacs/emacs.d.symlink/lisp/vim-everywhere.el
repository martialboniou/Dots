;;; vim-everywhere.el ---
;;
;; Filename: vim-everywhere.el
;; Description: Vim Emulation Setup
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 18:19:43 2011 (+0100)
;; Version: 0.6.2-linum-settings
;; Last-Updated: Tue Dec  3 19:37:04 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 429
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
       ;;(unless (< emacs-major-version 24) (custom-set-variables '(custom-safe-themes (quote ("6c61100cf9667a6bbc99933f9e49b352147ba5f6918926d317f4a327a7e7de94" default)))))
       ;; choose a theme according to your Vim setup -- ~/.vimrc by default
      
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
                       vim-colorscheme-used)))))))
                 ;;       (progn
                 ;;         (message "vim-everywhere: Vim colorscheme: not found at all")
                 ;;         (setq vim-colorscheme-used nil)))))
                 ;; (if (stringp vim-colorscheme-used)
                 ;;     (let ((found-theme (colorscheme-to-color-theme vim-colorscheme-used)))
                 ;;       (message "vim-everywhere: your Vim setting file calls \"%s\" as colorscheme" vim-colorscheme-used)
                 ;;       (if (functionp found-theme)
                 ;;           (progn
                 ;;             (setq *chosen-theme* found-theme)
                 ;;             (message "vim-everywhere: theme loading..."))
                 ;;         (message "vim-theme::no corresponding theme found; default one loading..."))))))))

       ;; fetch your vim colorscheme and load a clone theme
       (let ((colorscheme (mars/extract-colorscheme-from-vimrc))
             (default-colorscheme "molokai"))
         (cl-flet ((compose-theme-name (theme-head theme-name)
                                       (if (or (null theme-name) (< (length theme-name) 2)) "" ; no short name
                                         (concat theme-head (replace-regexp-in-string (char-to-string ?_) (char-to-string ?-) (remove-last-unwanted-char theme-name))))))
           (when (or (not (stringp colorscheme))
                     (= (length colorscheme) 0))
             (setq colorscheme default-colorscheme))
           (if (< emacs-major-version 24)
               (let* ((theme-header "custom-vim-colorscheme-")
                      (theme (intern (compose-theme-name theme-header colorscheme))))
                 (require 'deprecated-emacs-themes)
                 (unless (functionp theme) (setq theme (intern (compose-color-theme-name theme-header default-colorscheme))))
                 (when (functionp theme)
                   (funcall theme)))
             (let ((theme-header "vim-"))
               (load (compose-theme-name theme-header colorscheme) t)
                 ;; else
               )))))

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
