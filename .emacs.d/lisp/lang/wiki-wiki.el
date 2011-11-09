;;; wikiwiki.el --- 
;; 
;; Filename: wikiwiki.el
;; Description: Markup and other Wiki text-modes
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Mar 16 20:02:05 2011 (+0100)
;; Version: 
;; Last-Updated: Sun Nov  6 12:34:44 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 32
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: markdown / textile / yaml-mode / haml-mode / auctex
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

;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md\\'\\|\\.mkdn\\'\\|\\.markdown\\'" . markdown-mode))

;;; TEXTILE
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;;; YAML-MODE
;; not a markup actually!
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(eval-after-load "yaml-mode"
    '(progn
       (add-hook 'yaml-mode-hook
                 '(lambda ()
                    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))

;;; HAML (XHTML Abstraction Markup Language)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(eval-after-load "haml-mode"
  '(progn
     (add-hook 'haml-mode-hook
               '(lambda ()
                  (setq indent-tabs-mode nil) ; uses spaces (no tabs)
                  (define-key haml-mode-map "\C-m" 'newline-and-indent)))))

;;; AUCTEX
(require 'tex-site nil t)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-synctex-tex-flags (format "--synctex=1 --servername=%s" (user-login-name)))
(eval-after-load "tex-site"
  '(progn
     (require 'tex-style nil t)
     (setq TeX-PDF-mode t)              ; pdflatex as default
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
     ;; (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill) ; auto fill if you wish
     (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
     (add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
     (add-hook 'reftex-mode-hook '(lambda ()
                                    (setq TeX-open-quote "«~")
                                    (setq TeX-close-quote "~»")))
     ;; OSX users should use Skim.app instead of Preview.app
     (when (eq system-type 'darwin)
       (add-hook 'LaTeX-mode-hook (lambda ()
                                    (add-to-list 'TeX-expand-list
                                                 '("%q" skim-make-url))))
       (defun skim-make-url ()
         (concat (TeX-current-line)
                 " "
                 (expand-file-name (funcall file (TeX-output-extension) t)
                                   (file-name-directory (TeX-master-file)))
                 " "
                 (buffer-file-name)))
       (setq TeX-view-program-list
             '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))
       (setq TeX-view-program-selection '((output-pdf "Skim"))))))

(provide 'wiki-wiki)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wikiwiki.el ends here
