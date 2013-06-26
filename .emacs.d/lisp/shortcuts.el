;;; shortcuts.el ---
;;
;; Filename: shortcuts.el
;; Description: Global Keybindings
;; Author: Martial Boniou
;; Maintainer:
;; Created: Sat Feb 19 18:34:57 2011 (+0100)
;; Version:
;; Last-Updated: Tue Jun 25 10:16:47 2013 (+0200)
;;           By: Martial Boniou
;;     Update #: 202
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: <f1>-<f4>  is reserved to Emacs
;;              <f9>-<f12> should be kept untouched as they are commands
;;                         for desktop like OSX or Gnome
;;              <f5>-<f8>  will be used in this system
;;              <f2>       normally used for 2C-two-columns, will be
;;                         extended especially for the NO-WINDOW-SYSTEM
;;                         mode where 'C-S..', 'C-<arrow>', 'C-<specialkey>',
;;                         'C-;', 'C-.' or 'C-\}' aren't VT100 compatible.
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

(require 'adapter)

;;; MAIN KEYS (see 'SHORTCUTS)
;;
(bind-keys
 '("C-x C-f"   ido-recentf-file-name-history          ; IMPORTANT: C-x C-f is not `find-file' anymore (C-f to switch to `ido-find-file' only works from `ido-buffer-internal' and `ido-file-internal' derivatives.) [but use [(jxf)] in `sticky-control']
   "C-x F"     ido-recentf
   "C-x f"     ido-find-file ; may be called from `ido-switch-buffer' (doing C-x C-b C-f) [but use [(jxjf)] in `sticky-control']
   "C-="       shell-command
   "M-n"       make-frame               ; NOTE: or #'screen-create-screen if 'ESCREEN
   "M-<f2>"    apply-macro-to-region-lines ; use F3/F4 for kmacro start/end
   "C-c o"     anything-occur              ; TODO: or simply occur ?
   "C-:"       anything-M-x             ; C-S-; NOTE: or #'smex if 'SMEX
   "C-c l"     org-store-link ; [default]
   "C-x C-b"   ido-switch-buffer        ; switch buffer on "C-x C-b" (faster than typing "C-x b") [but use [(jxb)] in `sticky-control']
   "C-x b"     ibuffer                  ; nice buffer browser (a la `dired') [but use [(jxjb)] in `sticky-control']
   "C-p"       hippie-expand            ; like Vim previous expansion key
   "C-c _ w"   whitespace-mode
   "C-c _ t"   whitespace-toggle-options
   "C-c = w"   global-whitespace-mode
   "C-c = t"   global-whitespace-toggle-options
   "C-x 4 t"   transpose-buffers
   "C-M-z"     toggle-transparency
   "C-c C-m"   make-directory           ; or M-m in `ido'
   "C-c C-0"   anything-mini            ; buffers (w/o `ibuffer' tags) & recentf (w/o `ido-recentf' completion)
                                        ; See shortcuts.el: anything on [<f5><f8>] for fast navigation in:
                                        ; - buffers (prefer `ido-switch-buffer' [C-x C-b] or `cycle-buffer' [<f5><f[4|6]>] for faster cycling)
                                        ; - recentf (prefer `ido-recentf-file-name-history' [C-x C-f] for faster finding)
                                        ; - files in `default-directory' (not present in `anything-mini')
   "C-c C-9"   anything-imenu           ; IMPORTANT: useful for fast code navigation (unless `ecb')
                                        ;            anything-browse-code map on [<f7><f7>] too
   "C-<f10>"   tmm-menubar))              ; key-controlled menu (`<f10>' is default but awkward on OSX/Gnome) IMPORTANT: remember this for `no-window-system' session

;; C-\\ as <meta> everywhere (except anywhere `viper-mode' rewrites it)
;; NOTE: ESCREEN settings below (as 'escreen-map is C-\\)
(fset 'new-meta-prefix (copy-keymap 'ESC-prefix))
(bind-key "C-\\" #'new-meta-prefix)

;; C-z case
(global-unset-key (kbd "C-z"))      ; ELSCREEN or other packages may use it
(global-unset-key (kbd "C-x C-z"))  ; reserved for viper-mode or Evil

;; M-: alias (useful for NO-WINDOW-SYSTEM)
(bind-key "<f2>:" #'eval-expression)

;; SMEX case
(eval-after-load "smex"
  '(progn
     (bind-keys
      '("<f2>x"  smex
        "M-x"    smex
        "C-:"    smex
        "<f2>X"  smex-major-mode-commands
        "M-X"    smex-major-mode-commands))))

;; EVIL-LEADER case
(eval-after-load "evil-leader"
  '(progn
     (evil-leader/set-leader ",")
     (evil-leader/set-key
       "e" 'ido-recentf-file-name-history ; memo: :e => open ,e => recently open
       "f" 'ido-recentf-file-name-history
       "F" 'ido-find-file
       "," 'ido-switch-buffer           ; memo: ,, => switch buffer
       "b" 'ido-switch-buffer
       "B" 'ibuffer
       "d" 'make-directory
       "k" 'kill-buffer
       "m" 'tmm-menubar)))

(when *i-am-a-terminator*
  (bind-keys
   '("C-x C-h"   help-command ; use F1 for contextual help / C-h being rebind
     "C-h"       delete-backward-char
     "C-w"       backward-kill-word)))    ; C-w as 'DELETE-BACKWARD-WORD in Vi emu

;; C-w may be used for 'backward-word-delete so there should be
;; another way to do cut/copy/paste:
;; 1- Vi commands (d/y/p) for Vim user
;; 2- special C-; (;/'/a) for Dvorak typist (including Vim user)
;; 3- standard commands (x/v/c) for Qwerty typist & "terminator" (including Vim user)
(if *i-am-a-dvorak-typist*
    (bind-keys                          ; Oracle/Sun Type 5/6 Keyboards extra keys' order
     '("C-; C-'" copy-region-as-kill
       "C-; C-a" yank
       "C-; C-;" kill-region
       ;; unavailable C-; in NO-WINDOW-SYSTEM => use <f2>
       "<f2>'"   copy-region-as-kill
       "<f2>a"   yank
       "<f2>;"   kill-region))
  (when *i-am-a-terminator*
    (cua-mode t)))                      ; C-c/C-x got timeout in order
                                        ; to make combinations to work

(eval-after-load "cua-mode"
  '(progn
     (setq cua-auto-tabify-rectangles nil)
     (transient-mark-mode 1)
     (setq cua-keep-region-after-copy t))) ; MS Windows behavior

;; sticky-control when required
(unless (and (not *i-can-do-yubitsume-now*) window-system)
  (when (locate-library "sticky-control")
      (require 'sticky-control))
  (eval-after-load "sticky-control"
    '(progn
       (setq sticky-control-timeout 0.3)
       ;; - revert FIND-FILE and SWITCH-BUFFER actions
       (setq sticky-control-shortcuts
             (append
              '("xf"  . 'ido-recentf-file-name-history)
              '("xjf" . 'ido-find-file)
              '("xb"  . 'ido-switch-buffer)
              '("xjb" . 'ibuffer)
              sticky-control-shortcuts))
       ;; - dvorak copy/paste or CUA shortcuts
       (if *i-am-a-dvorak-typist*
           (setq sticky-control-shortcuts
                 (cons '(59 . [(control \;)])
                       sticky-control-shortcuts))
         (setq sticky-control-shortcuts
               (cons '(?v . "\C-v")     ; paste in CUA-mode
                     sticky-control-shortcuts))))))

;;; UTILITY MACROS
;;
(defmacro partition-on-predicate (predicate list)
  "Creates a list of two lists. According to the predicate, the first one
   is the truth, the second one is the rejected members."
  `(let ((in nil)
         (out nil))
     (progn
       (dolist (elt ,list)
         (if (funcall ,predicate elt)
             (setq in (cons elt in))
           (setq out (cons elt out))))
       (cons (reverse in) (list (reverse out))))))

(defmacro mars/build-ordered-function-keys (function-key &rest funs)
  "Creates a list of bindings from a list of functions. The first function is bound to
   <FUNCTION-KEY>1. The second to <FUNCTION-KEY>2... FUNCTION-KEY is a string with angular
   brackets like <f4> or without bracket like END. A tuple (FUNCTION . KEY) is out of the
   1..0 numkeys listing. If KEY is 'id, KEY is like FUNCTION-KEY (useful as dble click key).
   'PREV and 'NEXT is used to get the previous and next functions keys if any.
   Example:
     '(lambda () (interactive) (foo bar)) binds Fx-1  to (FOO BAR)
     'zorg                                binds Fx-2  to #'ZORG
     '(zorg . `a')                        binds Fx-A  to #'ZORG
     '(baz . id)                          binds Fx-Fx to #'BAZ
  "
  (cl-flet ((funkey-assoc-p (fun) (and (consp fun) (or (stringp (cdr fun))
                                                       (symbolp (cdr fun))))))
    (let ((key (if (= (string-to-char function-key) 60)
                   function-key
                 (concat "<" function-key ">")))
          (num 0)
          (acc)
          (lists (partition-on-predicate #'funkey-assoc-p funs)))
      (dolist (elt (cadr lists) acc)
        (if (> num 8)
            (setq num 0)
          (setq num (1+ num)))
        (setq acc (cons
                   (cons (concat key (number-to-string num)) elt) acc)))
      (dolist (elt (car lists) acc)
        (let ((right (cdr elt)))
          (when (or (symbolp right) (stringp right))
            (let ((add-key (cond ((equal right 'id) key)
                                 ((equal right 'prev)
                                  (concat "<f"
                                          (number-to-string (1- (string-to-number (substring key 2 -1))))
                                          ">"))
                                 ((equal right 'next)
                                  (concat "<f"
                                          (number-to-string (1+ (string-to-number (substring key 2 -1))))
                                          ">"))
                                 (t right))))
              (setq acc (cons
                         (cons (concat key add-key) (car elt)) acc))))))
      `(progn
         ,@(mapcar (lambda (arg)
                     `(global-set-key
                       (kbd ,(car arg)) ',(cdr arg)))
                   acc)))))

(defmacro mars/build-windows-archiver-function-keys (key)
  "Creates the keybindings for windows-archiver."
  (let ((acc))
    (dotimes (elt 10 acc)
      (setq acc (append acc `((lambda () (interactive) (revive-plus:wconf-archive-restore ,elt))))))
    `(mars/build-ordered-function-keys ,key
                                       ,@acc
                                       (revive-plus:wconf-archive-save . id)            ; save current
                                       ((lambda () (interactive)
                                          (revive-plus:wconf-archive-restore 0)) . prev) ; restore last
                                       (revive-plus:wconf-archive-restore . next)))) ; restore which

;;; DUMMY FUNCTIONS
;;
;; tell your functions unreachable via autoloads (generally defined in confs file
;; instead of <site-lisp> files). Use `fmakunbound' to test it.
(defun-dummy t
  ("code"           . (mars/save-n-purge-code mars/toggle-ecb mars/simple-call-tree-view))
  ("media"          . (mars/safe-emms-start-stop mars/emms-any-streams))
  ("gtd"            . (make-remember-frame mars/today-calendar mars/two-days-calendar mars/unscheduled-tasks))
  ("mail"           . (mars/draft-email mars/wl))
  ("crypto"         . mars/hexedit))

;;; SPECIAL KEYS
;;
;; <f5> + <f6> => toggle single window + cycle/(undo-)kill-buffer + windows configuration archiver
(defun mars/toggle-single-window ()
  (interactive)
  (condition-case err
      (call-interactively 'revive-plus:toggle-single-window)
    (error (if (locate-library "revive+")
               (progn
                 (message "shortcuts: %s" err)
                 (require 'revive+))
             (message "shortcuts: revive-plus is recommended and not installed.")))))
(mars/build-ordered-function-keys "f5"
                                  (mars/toggle-single-window        . id)
                                  (delete-window                    . "<end>") ; C-<end> may be used too
                                  (kill-this-buffer                 . "<f1>")
                                  ((lambda () (interactive)
                                     (undo-kill-buffer ()))         . "<f2>"))
(eval-after-load "anything"
  '(mars/build-ordered-function-keys "f5"
                                     (anything                      . "<f8>")))
(unless (locate-library "anything") (message "installing anything is recommended."))
(eval-after-load "cycle-buffer"
  '(mars/build-ordered-function-keys "f5"
                                     (cycle-buffer-backward            . next)
                                     (cycle-buffer                     . prev)))
(unless (locate-library "cycle-buffer") (message "installing cycle-buffer is recommended."))
(mars/build-windows-archiver-function-keys "f6")
;; <f7> => hack tools
(mars/build-ordered-function-keys "f7" compile elisp-macroexpand describe-unbound-keys mars/hexedit
                                  (whitespace-mode                  . "<f1>")
                                  (mars/save-n-purge-code           . "<f4>")
                                  (mars/simple-call-tree-view       . "<f5>")
                                  ; (anything-simple-call-tree        . prev)
                                  (mars/toggle-ecb                  . next))
(eval-after-load "anything-config"
  '(mars/build-ordered-function-keys "f7"
                                     (anything-browse-code          . id))) ; faster than ecb
(unless (locate-library "anything-config") (message "installing anything-config is recommended."))
(eval-after-load "ispell"
  '(mars/build-ordered-function-keys "f7"
                                     (cycle-ispell-languages        . "<f2>")))
;; <f8> => daily tasks
(global-unset-key [f8])
(mars/build-ordered-function-keys "f8" mars/emms-any-streams
                                  (mars/draft-email                 . "<f4>")
                                  (mars/two-days-calendar           . "<f5>")
                                  (mars/unscheduled-tasks           . "<f6>")
                                  (make-remember-frame              . prev)
                                  (mars/safe-emms-start-stop        . id)) ; used as a 'START/STOP switch

;; XMonad like keybindings for windows manipulation
;; - Navigating: Windmove uses C-<up> etc.
(windmove-default-keybindings 'control)
;; - Split & Resize
(bind-keys
 '("C-<up>"       windmove-up
   "<f2><up>"     windmove-up
   "<f2><down>"   windmove-down
   "<f2><left>"   windmove-left
   "<f2><right>"  windmove-right
   ;; - Split windows
   "C-S-<left>"   (lambda () (interactive)
                    (split-window-horizontally)
                    (previous-buffer))
   "C-S-<right>"  (lambda () (interactive)
                    (split-window-horizontally)
                    (buf-move-right)
                    (previous-buffer))
   "C-S-<up>"     (lambda () (interactive)
                    (split-window-vertically)
                    (previous-buffer))
   "C-S-<down>"   (lambda () (interactive)
                    (split-window-vertically)
                    (buf-move-down)
                    (previous-buffer))
   ;; - Delete the current window
   "C-<end>"      delete-window         ; <f5><end> may be used too
   ;; - Resize windows
                                        ;"C-<prior>"    shrink-window-horizontally

                                        ;"C-<next>"     enlarge-window-horizontally
                                        ;"C-^"          enlarge-window
   ;; - Scroll horizontally
   "<f2><prior>"  scroll-right
   "<f2><next>"   scroll-left
   ;; - Rotate all buffers: M-S-<up> | M-S-<down>
   "M-S-<down>"   (lambda () (interactive) (mars/rotate-windows 'down))
   "M-S-<up>"     (lambda () (interactive) (mars/rotate-windows 'up))
   "<f2><end>"    (lambda () (interactive) (mars/rotate-windows 'down))
   "<f2><home>"   (lambda () (interactive) (mars/rotate-windows 'up))))

;; - Tile
(eval-after-load "mars-tiling"
  '(bind-keys
    '("M-S-<left>"   (lambda () (interactive) ; left favorite layouts
                       (tiling-cycle 3 mars-tiling-favorite-main-layouts))
      "<f2><kp-delete>" (lambda () (interactive) ; '<f2>C-d' for 2C-two-columns
                          (tiling-cycle 3 mars-tiling-favorite-main-layouts))
      "M-S-<right>"  (lambda () (interactive) ; right favorite layouts
                       (tiling-cycle 3 mars-tiling-favorite-secondary-layouts))
      "M-S-<end>"    tiling-cycle)))
(unless (locate-library "mars-tiling") (message "installing mars-tiling is recommended."))
;; - Swap buffers: M-<up> ...
(eval-after-load "buffer-move"
  '(bind-keys
    '("M-<up>"       buf-move-up
      "M-<down>"     buf-move-down
      "M-<right>"    buf-move-right
      "M-<left>"     buf-move-left)))
(unless (locate-library "buffer-move") (message "installing buffer-move is recommended."))

;; escreen keybindings
;; NOTE: the use of <f2> doesn't impact 2C-two-columns keybindings
(eval-after-load "escreen"
  '(progn
     (defmacro escreen-keybindings-builder (super)
       "Builds keybindings for ESCREEN. SUPER may be a Meta key.
In this case, type \"M-\" as argument."
       `(bind-keys
         ;; - manage
         '(,(concat super "c") escreen-create-screen
           ,(concat super "n") escreen-create-screen
           ,(concat super "k") escreen-kill-screen
           ;; - Vi-like navigation
           ,(concat super "l") escreen-goto-next-screen
           ,(concat super "h") escreen-goto-prev-screen
           ;; - jump
           ,(concat super "j") escreen-goto-screen
           ;; - print
           ,(concat super "p") escreen-get-active-screen-numbers-with-emphasis)))
     (escreen-keybindings-builder "M-") ; M-n was defined for `make-frame' so it's ok!
     (escreen-keybindings-builder "<f2>"))) ; use <f2> for NO-WINDOW-SYSTEM case

;; paredit keybindings
(eval-after-load "paredit"
  '(progn
     ;; vt-100 and alike don't know keys like "\C-\S-0" and so on
     ;; NOTE: ensure function keys like <f2> are well mapped on termcaps like '\eOQ'
     (define-key paredit-mode-map (kbd "<f2>]") #'paredit-forward-barf-sexp) ; \C-\S-\]
     (define-key paredit-mode-map (kbd "<f2>[") #'paredit-backward-barf-sexp) ; \C-\S-\[
     (define-key paredit-mode-map (kbd "<f2>0") #'paredit-forward-slurp-sexp) ; \C-\)
     (define-key paredit-mode-map (kbd "<f2>9") #'paredit-backward-slurp-sexp))) ; \C-\(

;; remember keybindings (use <f8><f7> or "C-c C-r" to open in another frame)
(eval-after-load "remember"
  '(bind-keys
    '("C-c C-r" make-remember-frame
      "C-c r"   make-remember-frame)))

;; flymake keybindings
(eval-after-load "flymake"
  '(bind-keys
    '("M-S-h" flymake-goto-prev-error   ; MEMO: ESCREEN Vi-like navigation
      "M-S-l" flymake-goto-next-error)))

;; miscellaneous launcher keybindings
(bind-keys
 '("C-c t" default-term
   "C-c w" mars/wl))                    ; 'WL-OTHER-FRAME but ensure the `lisp/mail' load

(provide 'shortcuts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortcuts.el ends here
