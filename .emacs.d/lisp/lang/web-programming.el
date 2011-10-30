;;; web-programming.el --- 
;; 
;; Filename: web-programming.el
;; Description: Web Development
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sun Mar  6 21:14:44 2011 (+0100)
;; Version: 
;; Last-Updated: Wed Oct 26 21:47:50 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 38
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: mweb / js / nxhtml (WARNING: unused)
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

(require 'preamble)
(require 'www)

(defvar nxhtml-env nil)

;; MWEB
(unless nxhtml-env
  (load-library "mweb-example-config"))

;;; JS (was ESPRESSO)
(unless (or (> emacs-major-version 23)
            (and (= emacs-major-version 23)
                 (> emacs-minor-version 1)))
  (add-to-list 'auto-mode-alist '("\\.js$"    . espresso-mode))
  (add-to-list 'auto-mode-alist '("\\.json$"  . espresso-mode)))

;;; PHP
(dolist (php-exts '("\\.php[s34]?\\'"
                    "\\.phtml\\'"
                    "\\.inc\\'"))
  (add-to-list 'auto-mode-alist (cons php-exts 'php-mode)))
;; provided php-mode 1.5.0 or Nxhtml version if enabled
(eval-after-load "php-mode"
  '(progn
     (eval-after-load "flymake"
       '(progn
          (defun flymake-php-init ()
            "Use php to check the syntax of the current file. -- sacha chua"
            (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
                   (local (file-relative-name temp (file-name-directory buffer-file-name))))
              (list "php" (list "-f" local "-l"))))
          (add-to-list 'flymake-err-line-patterns
                       '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))
          (defmacro add-php-flymake-masks (&rest extensions)
            `(progn
               ,@(mapcar (lambda (x)
                           `(add-to-list
                             'flymake-allowed-file-name-masks
                             '(,x flymake-php-init)))
                         extensions)))
          (add-php-flymake-masks "\\.php$" "\\.module$" "\\.install$" "\\.inc$" "\\.engine$")))))

;;; NXHTML - not recommended
(when nxhtml-env
  (nxhtml-loader))                      ; defined in `confs/defs'
(autoload 'nxhtml-mumamo-mode "~/.emacs.d/packages/nxhtml/autostart.el" nil t)
(eval-after-load "nxhtml-mumamo-mode"
  '(progn
     (setq mumamo-chunk-coloring 'submode-colored)
     (setq nxhtml-skip-welcome t)
     (setq rng-nxml-auto-validate-flag nil)
     (add-to-list 'auto-mode-alist '("\\.\\(ctp\\|xml\\|htm\\|html\\|xslt\\|pt\\|zcm\\|xsl\\|rhtml\\|php\\|inc\\)\\'" . nxhtml-mumamo))
     (defvar hexcolour-keywords
       '(("#[abcdef[:digit:]]\\{6\\}"
          (0 (put-text-property
              (match-beginning 0)
              (match-end 0)
              'face (list :background
                          (match-string-no-properties 0)))))))
     (defun hexcolour-add-to-font-lock ()
       (font-lock-add-keywords nil hexcolour-keywords))
     (add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)))

(provide 'web-programming)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-programming.el ends here


