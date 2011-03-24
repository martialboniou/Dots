;;; vim-everywhere.el ---
;;
;; Filename: vim-everywhere.el
;; Description:
;; Author: Martial Boniou
;; Maintainer:
;; Created: Sat Feb 19 18:19:43 2011 (+0100)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 121
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
  (defun conf-locate (conf) (let ((path (mapcar '(lambda (x) (concat (file-name-as-directory mars/local-root-dir) x)) mars/local-conf-path))) (locate-library conf nil path))))
(unless (fboundp 'mars/add-to-load-path)
  (let ((local-site-lisp-path (mapcar '(lambda (x) (concat (file-name-as-directory mars/local-root-dir)
                                                           (file-name-as-directory x))) mars/site-lisp-path)))
    (setq load-path (append local-site-lisp-path load-path))
    (dolist (local-site-lisp local-site-lisp-path)
      (mapcar '(lambda (x)
                 (let ((found-dirs (directory-files local-site-lisp t (symbol-name x))))
                   (when found-dirs
                     (dolist (found-dir found-dirs)
                       (save-excursion
                         (condition-case nil
                             (progn
                               (cd found-dir)
                               (push (expand-file-name found-dir) load-path))
                           (error nil)))))))
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
  (when vi-conf
    (setq viper-custom-file-name (convert-standard-filename vi-conf))))
(setq viper-toggle-key "\C-x\C-z" ; no more C-z ('minimize' then works on Mac)
      viper-mode t)
(require 'viper)

(eval-after-load "viper"
  '(progn
     ;; 1- vimpulse
     ;; (setq vimpulse-enhanced-paren-matching nil)
     (require 'vimpulse)

     ;; 2- parenface to add a default color to parentheses as Vim does
     (unless (locate-library "hl-line+")
       (defface hl-line '((t (:background "grey10"))) "Dummy hl-line.")
       (setq hl-line-face 'hl-line))
     (color-theme-initialize)
     (require 'parenface)
     ;; (dolist (mode '(c-mode cpp-mode java-mode html-mode-hook css-mode-hook emacs-lisp-mode lisp-mode)) (add-hook mode (parenface-add-support

     ;; 3- nothing to add (mis)match parentheses (vimpulse uses 'show-parens
     ;;    so there's no need to add 'mic-paren)

     ;; 4- TODO: colorize numbers & warnings
     (defface font-lock-number-face
       '((((type tty) (class color)) (:foreground "pink"))
         (((type tty) (class mono))  (:inverse-video t))
         (((class color) (background dark))  (:foreground "pink"))
         (((class color) (background light)) (:foreground "grey10"))
         (t ()))
       "Face lock mode face used to highlight numbers."
       :group 'basic-faces)
     (defvar font-lock-number-face 'font-lock-number-face)
     (defun font-lock-fontify-numbers ()
       "Hook function to `font-lock-number-face'-ify numbers."
       (font-lock-add-keywords nil
                               '(("[^a-zA-Z_]\\(0x[0-9a-fA-F]+\\)"     1 font-lock-number-face) ; hexa
                                 ("[^a-zA-Z_]\\(-?[0-9]+\\.[0-9]+\\)"  1 font-lock-number-face) ; float
                                 ("[^a-zA-Z_1-9-]\\(-?[0-9]+U?L?L?\\)" 1 font-lock-number-face)))) ; int
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
                     smalltalk-mode
                     factor-mode
                     js2-mode
                     erlang-mode
                     caml-mode
                     clojure-mode))     ; TODO: mix in code.el
       (progn
         (add-hook (intern (concat (symbol-name mode) "-hook")) 'font-lock-fontify-numbers)
         (font-lock-add-keywords mode
                                 '(("\\(FIXME:\\|TODO:\\|IMPORTANT:\\|WARNING:\\|ERROR:\\|XXX\\|OBSOLETE\\|DEPRECATED\\)"
                                    1 font-lock-warning-face prepend)))))
     ;; (add-hook 'font-lock-mode-hook 'font-lock-fontify-numbers) ; FIXME: wl-summary doesn't use `font-lock-mode'

     ;; 5- additional keyboard bindings (some from http://stackoverflow.com/users/2797/sebastien-roccaserra)
     (define-key viper-vi-global-user-map [(delete)] 'delete-char)
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

     ;; 6- colorize <> modes
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

     ;; wombat colorscheme (2007/01/22 version) by Lars H. Nielsen
     (defcolor-theme "wombat"
       ((foreground-color . "#f6f3e8")  ; +Normal guifg
        (background-color . "#242424")  ; +Normal guibg
        (border-color . "#2d2d2d")      ; +CursorLine
        (cursor-color . "#656565")      ; +Cursor guibg
        (background-mode . dark))       ; color orientation
       (cursor ((t (:foreground nil :background "#656565")))) ; +Cursor
       (region ((t (:foreground "#f6f3e8" :background "#444444")))) ; +Visual
       (highlight ((t (:background "#2d2d2d")))) ; +CursorLine
       (hl-line   ((t (:background "#2d2d2d")))) ; +CursorLine
       (fringe ((t (:foreground "#857b6f" :background "#000000")))) ; +LineNr
       (paren-face ((t (:foreground "#e7f6da")))) ; +Special
       (show-paren-match-face ((t (:foreground "#f6f3e8" :background "#857b6f" :bold t)))) ; +MatchParen
       (show-paren-mismatch-face ((t (:foreground "#ffffff" :background "#6e2e2e"))))  ; +Error " from inkpot
       (isearch ((t (:inverse-video t :bold nil :underline nil)))) ; +Search | (inverse-video)
       (modeline ((t (:italic t :bold nil :foreground "#f6f3e8" :background "#444444")))) ; +StatusLine
       (modeline-inactive ((t (:bold nil :foreground "#857b6f" :background "#444444")))) ; +StatusLineNC | User2
       (modeline-buffer-id ((t (:italic t :bold t :foreground "#f6f3e8" :background "#202020")))) ; +StatusLine
       (minibuffer-prompt ((t (:bold t :foreground "##f6f3e8")))) ; +User2 guifg | Title guifg
       (ediff-current-diff-A ((((class color) (min-colors 16))
                               (:foreground "#ffffcd" :background "#306b8f"))
                              (((class color)) (:foreground "white" :background "blue3"))
                              (t (:inverse-video t)))) ; +DiffChange " from inkpot
       (ediff-fine-diff-A ((((class color) (min-colors 16))
                            (:foreground "#ffffcd" :background "#4a2a4a"))
                           (((class color)) (:foreground "white" :background "green4"))
                           (t (:inverse-video t)))) ; +DiffText " from inkpot
       (font-lock-builtin-face ((t (:foreground "#e7f6da")))) ; +Special
       (font-lock-comment-face ((t (:foreground "#99968b" :italic t)))) ; +Comment
       (font-lock-constant-face ((t (:foreground "#e5786d")))) ; +Constant
       (font-lock-doc-face ((t (:foreground "#99968b" :italic t)))) ; +Comment
       (font-lock-function-name-face ((t (:foreground "#cae682" :bold nil)))) ; +Function | Normal guifg (bold)
       (font-lock-keyword-face ((t (:foreground "#8ac6f2")))) ; +Statement
       (font-lock-preprocessor-face ((t (:foreground "#e5786d")))) ; +PreProc
       (font-lock-number-face ((t (:foreground "#e5786d")))) ; +Number
       (font-lock-reference-face ((t (:bold t :foreground "#8ac6f2")))) ; +Statement
       (font-lock-string-face ((t (:foreground "#95e454" :background nil :italic t)))) ; +String
       (font-lock-type-face ((t (:foreground "#cae682")))) ; +Type
       (font-lock-variable-name-face ((t (:foreground "#cae682")))) ; +Identifier
       (font-lock-warning-face ((t (:foreground "#ffffff" :background "#6e2e2e"))))) ; +Error | ErrorMsg " from inkpot

     ;; inkpot colorscheme from `github.com/ciaranm/inkpot'
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
       (show-paren-mismatch-face ((t (:foreground "#ffffff" :background "#6e2e2e"))))  ; +Error
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
       (font-lock-warning-face ((t (:foreground "#ffffff" :background "#6e2e2e"))))) ; +Error

     ;; ir_black colorscheme (2011/01/19 version) from `github.com/lukaszb/vim-irblack'
     (defcolor-theme "ir_black"
       ((foreground-color . "#F6F3E8")  ; +Normal guifg
        (background-color . "black")    ; +Normal guibg
        (border-color . "#121212")      ; +CursorLine
        (background-mode . dark))       ; color orientation
       (cursor ((t (:foreground "black" :background "white")))) ; +Cursor
       (region ((t (:background "#262D51")))) ; +Visual
       (highlight ((t (:background "#121212")))) ; +CursorLine
       (hl-line   ((t (:background "#121212")))) ; +CursorLine
       (fringe ((t (:foreground "#3D3D3D" :background "black")))) ; +LineNr
       (paren-face ((t (:foreground "#E18964")))) ; +Special
       (show-paren-match-face ((t (:foreground "#f6f3e8" :background "#857b6f" :bold nil)))) ; +MatchParen
       (show-paren-mismatch-face ((t (:foreground "black" :background "#FF6C60"))))  ; +Error | ErrorMsg
       (isearch ((t (:bold t :underline t)))) ; +Search
       (modeline ((t (:italic t :foreground "#CCCCCC" :background "#202020")))) ; +StatusLine
       (modeline-inactive ((t (:foreground "black" :background "#202020")))) ; +StatusLineNC | User2
       (modeline-buffer-id ((t (:bold t :foreground "#CCCCCC" :background "#202020")))) ; +StatusLine
       (minibuffer-prompt ((t (:bold t :foreground "#f6f3e8")))) ; +User2 guifg | Title guifg
       (ediff-current-diff-A ((((class color) (min-colors 16))
                               (:foreground "#ffffcd" :background "#306b8f"))
                              (((class color)) (:foreground "white" :background "blue3"))
                              (t (:inverse-video t)))) ; +DiffChange " from inkpot
       (ediff-fine-diff-A ((((class color) (min-colors 16))
                            (:foreground "#ffffcd" :background "#4a2a4a"))
                           (((class color)) (:foreground "white" :background "green4"))
                           (t (:inverse-video t)))) ; +DiffText " from inkpot
       (font-lock-builtin-face ((t (:foreground "#E18964")))) ; +Special
       (font-lock-comment-face ((t (:foreground "#7C7C7C" :italic nil)))) ; +Comment
       (font-lock-constant-face ((t (:foreground "#99CC99")))) ; +Constant
       (font-lock-doc-face ((t (:foreground "#7C7C7C" :italic nil)))) ; +Comment
       (font-lock-function-name-face ((t (:foreground "#FFD2A7" :bold nil)))) ; +Function | Normal guifg (bold)
       (font-lock-keyword-face ((t (:foreground "#6699CC")))) ; +Statement
       (font-lock-preprocessor-face ((t (:foreground "#96CBFE")))) ; +PreProc
       (font-lock-number-face ((t (:foreground "#FF73FD")))) ; +Number
       (font-lock-reference-face ((t (:bold t :foreground "#6699CC")))) ; +Statement
       (font-lock-string-face ((t (:foreground "#A8FF60" :background nil :italic nil)))) ; +String
       (font-lock-type-face ((t (:foreground "#FFFFB6")))) ; +Type
       (font-lock-variable-name-face ((t (:foreground "#C6C5FE")))) ; +Identifier
       (font-lock-warning-face ((t (:foreground "black" :background "#FF6C60"))))) ; +Error | ErrorMsg

     ;; 5- fetch your vim colorscheme and load a mockup adapter
     (setq *chosen-theme* (colorscheme-to-color-theme "wombat")) ; default theme
     (mars/extract-colorscheme-from-vimrc)
     (funcall *chosen-theme*)

     ;; 6- open the current buffer in Vim (when Emacs modal editing comes short)
     (defun open-with-vim ()
       "Open current buffer with Vim. To ensure buffers synchronization, set 'GLOBAL-AUTO-REVERT-MODE to T."
       (interactive)
       (let* ((os (symbol-name system-type))
              (vim-name (if (string-match "Darwin" os)
                            "mvim"
                          (if (string-match "^Windows.*" os)
                              "gvim.exe"
                            "vim"))))
         (message "Open %s with %s..." buffer-file-name vim-name)
         (shell-command (concat vim-name " " buffer-file-name))))
     (global-set-key '[(meta \0)] 'open-with-vim)))

;; - some keybindings
;; remember:
;; C-w = Window manipulation in normal modes; a kind of "'delete-backward-word" elsewhere

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vim-everywhere.el ends here

