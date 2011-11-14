;;; peyton-jones-family.el --- 
;; 
;; Filename: peyton-jones-family.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Mar 16 20:40:26 2011 (+0100)
;; Version: 
;; Last-Updated: Mon Nov 14 13:59:56 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 38
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: haskell-mode
;; 
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

(provide 'one-language-spoken)
(require 'code-preamble)
(unintern 'one-language-spoken obarray)

;;; HASKELL MODE
(setq haskell-program-name mars/haskell-program-name)

;;; SHEN MODE
(add-to-list 'auto-mode-alist '("\\.shen\\'" . shen-mode))
;; (eval-after-load "inf-shen"
;;   '(progn
;;      (defun check-balanced-parens (start end)
;;        "Check if parentheses in the region are balanced. Signals a 
;; scan-error if not." 
;;        (save-restriction 
;;          (save-excursion 
;;            (let ((deactivate-mark nil)) 
;;              (condition-case c 
;;                  (progn 
;;                    (narrow-to-region start end) 
;;                    (goto-char (point-min))
;;                    (while (/= 0 (- (point)
;;                                    (forward-list)))) 
;;                    t)
;;                (scan-error (signal 'scan-error '("Region parentheses not balanced."))))))))
;;      (defadvice shen-eval-region (around shen-secure-eval (start end &optional and-go) activate)
;;        (interactive "r\nP")
;;        (when (condition-case err
;;                  (progn
;;                    (check-balanced-parens start end)
;;                    t)      
;;                (error
;;                 (y-or-n-p (format "%s Eval anyway ?" (error-message-string err)))))
;;          ad-do-it))))

(provide 'peyton-jones-family)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; peyton-jones-family.el ends here
