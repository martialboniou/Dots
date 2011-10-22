;;; vim-everywhere.el ---
;;
;; Filename: vim-everywhere.el
;; Description: Vimpulse configuration
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 18:19:43 2011 (+0100)
;; Version: 0.4
;; Last-Updated: Sat Oct 22 14:21:56 2011 (+0200)
;;           By:
;;     Update #: 262
;; URL: 
;; Keywords: 
;; Compatibility: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: Vimpulse + your Vim colorscheme in Emacs
;; 
;; keep (quite) same syntax highlighting everywhere
;; by hondana@gmx.com 2001-2011
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
(unless (boundp 'mars/local-root-dir) (condition-case nil (load (concat (file-name-directory load-file-name) "vars")) (error "Unable to get custom variables")))
(unless (fboundp 'conf-locate)
  (defun conf-locate (conf) (let ((path (mapcar #'(lambda (x) (concat (file-name-as-directory mars/local-root-dir) x)) mars/local-conf-path))) (locate-library conf nil path))))
(unless (fboundp 'mars/add-to-load-path)
  (let ((local-site-lisp-path (mapcar #'(lambda (x) (concat (file-name-as-directory mars/local-root-dir)
                                                           (file-name-as-directory x))) mars/site-lisp-path)))
    (setq load-path (append local-site-lisp-path load-path))
    (dolist (local-site-lisp local-site-lisp-path)
      (mapc #'(lambda (x)
                (let ((found-dirs (directory-files local-site-lisp t (symbol-name x))))
                  (when found-dirs
                    (mapc #'(lambda (found-dir)
                              (save-excursion
                                (condition-case nil
                                    (progn
                                      (cd found-dir)
                                      (push (expand-file-name found-dir) load-path))
                                  (error nil))))
                          found-dirs))))
            '(color-theme vimpulse))))
  (load-library "color-theme-autoloads"))

;;; PREAMBULE
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
(setq viper-toggle-key "\C-x\C-z" ; no more C-z ('minimize' then works on Mac)
      viper-mode t)
(require 'viper)

(eval-after-load "viper"
  '(progn
     ;; 1- vimpulse
     (require 'vimpulse)
     (eval-after-load "vimpulse"
       '(progn
          (define-key viper-vi-global-user-map [(control kp-delete)] nil)
          ;; add C-w outside vimpulse (eg. C-w C-w -> other-window)
          (fset 'vimpulse-like-window-map (copy-keymap vimpulse-window-map))
          (eval-after-load "dired"
            '(progn
               (define-key dired-mode-map "\C-w" 'vimpulse-like-window-map)))
          (eval-after-load "ibuffer"
            '(progn
               (define-key ibuffer-mode-map "\C-w" 'vimpulse-like-window-map)))))
     ;; 2- parenface to add a default color to parentheses as Vim does
     (if (locate-library "hl-line+")
         (global-hl-line-mode)
       (progn
         (defface hl-line '((t (:background "grey10"))) "Dummy hl-line.")
         (setq hl-line-face 'hl-line)))
     (color-theme-initialize)
     (require 'parenface)
     ;; (dolist (mode '(c-mode cpp-mode java-mode html-mode-hook css-mode-hook emacs-lisp-mode lisp-mode)) (add-hook mode (parenface-add-support

     ;; 3- nothing to add (mis)match parentheses (vimpulse uses 'show-parens
     ;;    so there's no need to add 'mic-paren)

     ;; 4- line numbering
     (require 'linum-settings)

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
                     c-mode
                     cc-mode
                     espresso-mode
                     ruby-mode
                     java-mode
                     python-mode
                     ruby-mode
                     cperl-mode
                     howm-mode
                     org-mode
                     haskell-mode
                     qi-mode
                     shen-mode
                     smalltalk-mode
                     factor-mode
                     js2-mode
                     erlang-mode
                     caml-mode
                     clojure-mode))     ; TODO: mix in code.el
       (progn
         (add-hook (intern (concat (symbol-name mode) "-hook")) 'font-lock-fontify-numbers)
         (font-lock-add-keywords mode
                                 '(("\\(FIXME:\\|TODO:\\)"
                                    1 font-lock-builtin-face prepend))) ; FIXME: builtin here b/c todo is too grayish in `Wombat256mod'
         (font-lock-add-keywords mode
                                 '(("\\(IMPORTANT:\\|WARNING:\\|NOTE:\\|DEPRECATED\\)"
                                    1 font-lock-notify-face prepend)))
         (font-lock-add-keywords mode
                                 '(("\\(ERROR:\\|XXX\\|OBSOLETE\\)"
                                    1 font-lock-warning-face prepend)))))

     ;; 6- additional keyboard bindings
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
     (define-key viper-vi-basic-map "\C-e" 'viper-scroll-up-one) ; should be defined anyway
     (define-key viper-insert-basic-map (kbd "C-,") 'vimpulse-copy-from-below) ; switch 'C-e to 'C-,
     (define-key vimpulse-visual-basic-map "F" 'viper-find-char-backward)
     (define-key vimpulse-visual-basic-map "t" 'viper-goto-char-forward)
     (define-key vimpulse-visual-basic-map "T" 'viper-goto-char-backward)
     (define-key vimpulse-visual-basic-map "e" '(lambda ()
                                                  (interactive)
                                                  (viper-end-of-word 1)
                                                  (viper-forward-char 1)))
     (push '("only"  (delete-other-windows)) ex-token-alist)
     (push '("close" (delete-window))        ex-token-alist)
     (define-key viper-vi-global-user-map " d" 'viper-kill-buffer)
     (when *i-am-a-terminator*
       ;; (global-unset-key "\C-h")
       (define-key viper-vi-global-user-map "\C-h" 'viper-backward-char)
       (define-key viper-insert-global-user-map "\C-h" 'viper-delete-backward-char))

     ;; 7- colorize <> modes
     (setq viper-vi-state-id
           (concat (propertize "<V>" 'face 'font-lock-string-face) " ")
           viper-insert-state-id
           (concat (propertize "<I>" 'face 'font-lock-string-face) " ")
           viper-replace-state-id
           (concat (propertize "<R>" 'face 'font-lock-string-face) " "))
     (require 'hi-lock)
     (setq viper-emacs-state-id (concat (propertize "<E>" 'face 'hi-red-b) " "))
     (put 'viper-mode-string 'risky-local-variable t)

     ;; choose a theme according to your Vim setup (~/.vimrc by default)
     (defvar *color-theme-header* "color-theme-vim-"
       "The string to add as a color-theme prefix.")

     (defvar *chosen-theme* nil
       "The lambda to draw.")

     (defun remove-last-unwanted-char (lookup-string)
       "Remove -, _, ? or ! characters at the end of a string."
       (while (and
               (> (length lookup-string) 0)
               (string-match-p (substring lookup-string -1) "-_?!=")) ; keep '+'
         (setq lookup-string (substring lookup-string 0 -1)))
       lookup-string)

     (defun compose-theme-name (theme-name)
       "Make a lispian name for the color-theme function."
       (if (or (null theme-name)
               (< (length theme-name) 2))
           ""                                ; no short name
         (flet ((replace-underscore (string-to-replace)
                                    (replace-regexp-in-string
                                     (char-to-string ?_)
                                     (char-to-string ?-)
                                     (remove-last-unwanted-char string-to-replace))))
           (concat *color-theme-header* (replace-underscore theme-name)))))

     (defun colorscheme-to-color-theme (colorscheme-name)
       " Returns a theme function in a lambda from a colorscheme name. Works
if the function exists."
       (let ((theme (if (= (length colorscheme-name) 0)
                        nil
                      (intern (compose-theme-name colorscheme-name)))))
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
               (end-of-buffer)                       ; backward searching
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
     ;;         (paren-face-(mis)match) but vimpulse uses its
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
       (isearch ((t (:bold t :background "#303030" :foreground "#ad7b57")))) ; +Search
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
     (setq *chosen-theme* (colorscheme-to-color-theme "wombat256mod")) ; default theme
     ;; or (setq *chosen-theme* (colorscheme-to-color-theme "inkpot"))
     
     (mars/extract-colorscheme-from-vimrc)
     (funcall *chosen-theme*) ;; FIXME: slow down

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vim-everywhere.el ends here
