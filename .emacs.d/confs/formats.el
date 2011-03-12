;;; formats.el --- 
;; 
;; Filename: formats.el
;; Description: Formats, styles, encodings and image support
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Feb 23 12:16:46 2011 (+0100)
;; Version: 
;; Last-Updated: Thu Mar 10 17:42:22 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 19
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: Pretty (pp-c-l) + encodings switiching + format on save
;;              helpers + style + image support (iimage)
;; 
;; formats by Martial (2010-2011)
;;
;; iso-8859-1 support (when needed)
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

;;; PRETTY
;; pretty control-l
(require 'pp-c-l)
(pretty-control-l-mode 1)

;;; SWITCH ENCODINGS
(setq *supported-encodings* '(utf-8 latin-1))

(defun mars/define-encodings (encoding)
  (setq locale-coding-system encoding)
  (set-terminal-coding-system encoding)
  (set-keyboard-coding-system encoding)
  (set-selection-coding-system encoding)
  (prefer-coding-system encoding)
  (set-language-environment (symbol-name encoding)))

(defun next-elt-in-circular-list(elt list)
  (let ((my-list (append list (list (car list)))))
      (cadr (member elt my-list))))

(defun switch-encodings ()
  (interactive)
  (let ((current-coding (symbol-name locale-coding-system)))
    (if (y-or-n-p (concat "The current encoding is " current-coding ". Change it? "))
        (let ((next (next-elt-in-circular-list locale-coding-system *supported-encodings*)))
          (unless (null next)
            (message (concat "The current encoding is " (symbol-name next) " now!"))
            (mars/define-encodings next))))))
(mars/define-encodings 'utf-8)

;;; FORMAT ON SAVE HELPERS
;; special save (see shortcuts) to tidy the code up
(defun mars/save-n-purge-code ()
  "Indent, ask to create a header (with clean eof) if none,
remove whitespace and save the current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (mars/create-header-if-none)
;;  (delete-trailing-whitespace)
  (save-buffer))
(unless (fboundp 'mars/create-header-if-none) (defun mars/create-header-if-none nil nil))
;; automatic untabify code belonging to `alexott/untabify-modes'
(defcustom alexott/untabify-modes
  '(emacs-lisp-mode lisp-mode scheme-mode clojure-mode haskell-mode smalltalk-mode erlang-mode espresso-mode)
  "List of major modes where the content should be untabified. DON'T PUT `makefile-mode' HERE."
  :type '(repeat (symbol :tag "Major Mode"))
  :group 'indent)
(defun alexott/untabify-hook ()
  (when (member major-mode alexott/untabify-modes)
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'alexott/untabify-hook)
;; delete trailing whitespace
(defalias 'dtw 'delete-trailing-whitespace)
(defadvice delete-trailing-whitespace (after advising-deletion nil activate)
  (message "Trailing whitespace deleted"))

;;; STYLE
(setq-default c-basic-offset 4)
(setq c-default-style "linux")
(defun mars/c-brace-and-indent-hook ()
  (c-set-offset 'substatement-open 0)
  (setq c-basic-offset 4))
(add-hook 'c-mode-common-hook 'mars/c-brace-and-indent-hook)

(defun c-reformat-buffer()
  (interactive)
  (save-buffer)
  (setq sh-indent-command (concat
                           "indent -st -bad --blank-lines-after-procedures "
                           "-bli0 -i4 -l79 -ncs -npcs -nut -npsl -fca "
                           "-lc79 -fc1 -cli4 -bap -sob -ci4 -nlp "
                           buffer-file-name))
  (mark-whole-buffer)
  (universal-argument)
  (shell-command-on-region
   (point-min)
   (point-max)
   sh-indent-command
   (buffer-name))
  ;;(save-buffer)
)

;; FIXME: (define-key c-mode-base-map [f7] 'c-reformat-buffer)

;;; IMAGE SUPPORT
;; iimage
(mapc '(lambda (x)
         (add-hook x 'turn-on-iimage-mode))
      '(Info-mode-hook texinfo-mode-hook wikipedia-mode)) ; info/wiki case
(eval-after-load "org"             ; org-mode case
  '(progn
     (require 'iimage)
     (add-to-list 'iimage-mode-image-regex-alist
                  (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                                "\\)\\]")  1))
     (defun org-toggle-iimage-in-org ()
       "Display images in your org file."
       (interactive)
       (if (face-underline-p 'org-link)
           (set-face-underline-p 'org-link nil)
         (set-face-underline-p 'org-link t))
       (iimage-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; formats.el ends here
