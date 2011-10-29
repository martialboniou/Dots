;;; shortcuts.el ---
;;
;; Filename: shortcuts.el
;; Description:
;; Author: Martial Boniou
;; Maintainer:
;; Created: Sat Feb 19 18:34:57 2011 (+0100)
;; Version:
;; Last-Updated: Sun Oct 23 17:48:17 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 102
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: Arrow / Functions keys (should be loaded at the end of
;;              .emacs)
;;              Other keys (see .emacs for main bindings)
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

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'defs)

;;; MACROS
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
  (flet ((funkey-assoc-p (fun) (and (consp fun) (or (stringp (cdr fun))
                                                    (symbolp (cdr fun))))))
    (let ((key (if (= (string-to-char function-key) 60)
                   function-key
                 (concat "<" function-key ">")))
          (num 0)
          (acc)
          (lists (partition-on-predicate 'funkey-assoc-p funs)))
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
      (setq acc (append acc `((lambda () (interactive) (mars-windows-archiver-restore ,elt))))))
    `(mars/build-ordered-function-keys ,key
                                       ,@acc
                                       (mars-windows-archiver-save  . id)            ; save current
                                       ((lambda () (interactive)
                                          (mars-windows-archiver-restore 0)) . prev) ; restore last
                                       (mars-windows-archiver-restore . next)))) ; restore which

;;; DUMMY FUNCTIONS
;; tell your functions unreachable via autoloads (generally defined in confs file
;; instead of <site-lisp> files). Use `fmakunbound' to test it.
(defun-dummy t
  ("code"           . (mars/save-n-purge-code mars/toggle-ecb))
  ("window-manager" . mars/toggle-single-window)
  ("media"          . (mars/safe-emms-start-stop mars/emms-any-streams))
  ("gtd"            . (make-remember-frame mars/today-calendar mars/unscheduled-tasks))
  ("mail"           . (mars/draft-email mars/wl))
  ("crypto"         . mars/hexedit))
(defun-dummy nil undo-kill-buffer ibuffer cycle-buffer cycle-buffer-backward cycle-ispell-languages)

;;; KEYBINDINGS
;; <f5> + <f6> => toggle single window + cycle/(undo-)kill-buffer + windows configuration archiver
(mars/build-ordered-function-keys "f5"
                                  (mars/toggle-single-window  . id)
                                  (cycle-buffer-backward      . next)
                                  (cycle-buffer               . prev)
                                  (kill-this-buffer           . "<f1>")
                                  ((lambda () (interactive)
                                     (undo-kill-buffer ()))   . "<f2>")
                                  (anything                   . "<f8>"))
(mars/build-windows-archiver-function-keys "f6")
;; <f7> => hack tools
(mars/build-ordered-function-keys "f7" compile elisp-macroexpand describe-unbound-keys mars/hexedit
                                  (whitespace-mode            . "<f1>")
                                  (mars/save-n-purge-code     . "<f5>")
                                  (anything-simple-call-tree  . prev)
                                  (anything-browse-code       . id) ; faster than ecb
                                  (mars/toggle-ecb            . next)
                                  (cycle-ispell-languages     . "<f2>"))
;; <f8> => daily tasks
(global-unset-key [f8])
(mars/build-ordered-function-keys "f8" mars/emms-any-streams
                                  (mars/draft-email          . "<f4>")
                                  (mars/today-calendar       . "<f5>")
                                  (mars/unscheduled-tasks    . "<f6>")
                                  (make-remember-frame       . prev)
                                  (mars/safe-emms-start-stop . id)) ; used as a 'START/STOP switch

;; XMonad like keybindings for windows manipulation
;; - Navigating: Windmove uses C-<up> etc.
(windmove-default-keybindings 'control)
;; - Split & Resize
(bind-keys '("C-<up>"      windmove-up
             "C-S-<left>"  (lambda () (interactive)
                             (split-window-horizontally)
                             (previous-buffer))
             "C-S-<right>" (lambda () (interactive)
                             (split-window-horizontally)
                             (buf-move-right)
                             (previous-buffer))
             "C-S-<up>"    (lambda () (interactive)
                             (split-window-vertically)
                             (previous-buffer))
             "C-S-<down>"  (lambda () (interactive)
                             (split-window-vertically)
                             (buf-move-down)
                             (previous-buffer))
             "C-S-<end>"   delete-window
             "C-{"         shrink-window-horizontally ; TODO: replace both b/c paredit
             "C-}"         enlarge-window-horizontally
             "C-^"         enlarge-window
             ;; - Swap buffers: M-<up> etc.
             "M-<up>"      buf-move-up
             "M-<down>"    buf-move-down
             "M-<right>"   buf-move-right
             "M-<left>"    buf-move-left
             ;; - Rotate all buffers: M-S-<up> | M-S-<down>
             "M-S-<down>"  (lambda () (interactive) (mars/rotate-windows 'down))
             "M-S-<up>"    (lambda () (interactive) (mars/rotate-windows 'up))
             ;; - Tile
             "M-S-<left>"  (lambda () (interactive) ; left favorite layouts
                             (tiling-cycle 3 mars-tiling-favorite-main-layouts))
             "M-S-<right>" (lambda () (interactive) ; right favorite layouts
                             (tiling-cycle 3 mars-tiling-favorite-secondary-layouts))
             "M-S-<end>"   tiling-cycle)) ; accepts prefix number

;; Remember (use <f8><f7> or "C-c (C-)r" to open in another frame)
(bind-keys
 '("C-c C-r" make-remember-frame
   "C-c r"   make-remember-frame))
;; Utils

(bind-keys
 '("C-c t" default-term
   "C-c w" mars/wl))                    ; 'WL-OTHER-FRAME but ensure the `confs/mail' load

(provide 'shortcuts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortcuts.el ends here
