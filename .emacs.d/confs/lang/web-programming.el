;;; web-programming.el --- 
;; 
;; Filename: web-programming.el
;; Description: Web Development
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sun Mar  6 21:14:44 2011 (+0100)
;; Version: 
;; Last-Updated: Thu Oct  6 21:10:14 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 8
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: Nxhtml & MuMaMo / Espresso (JavaScript)
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


;;; NXHTML
(nxhtml-loader)                         ; defined in DEFS

  ;; PHP/CSS/HTML/JavaScript
  ;; (autoload 'nxhtml-mumamo-mode "~/.emacs.d/packages/nxhtml/autostart.el" nil t)
  ;; (setq mumamo-chunk-coloring 'submode-colored)
  ;; (setq nxhtml-skip-welcome t)
  ;; (setq rng-nxml-auto-validate-flag nil)
  ;; (add-to-list 'auto-mode-alist '("\\.\\(ctp\\|xml\\|htm\\|html\\|xslt\\|pt\\|zcm\\|xsl\\|rhtml\\|php\\|inc\\)\\'" . nxhtml-mumamo))

  ;; (defvar hexcolour-keywords
  ;;   '(("#[abcdef[:digit:]]\\{6\\}"
  ;;      (0 (put-text-property
  ;;          (match-beginning 0)
  ;;          (match-end 0)
  ;;          'face (list :background
  ;;                      (match-string-no-properties 0)))))))
  ;; (defun hexcolour-add-to-font-lock ()
  ;;   (font-lock-add-keywords nil hexcolour-keywords))
  ;; (add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
  ;; (remove-hook 'css-mode-hook 'hexcolour-add-to-font-lock)

;;; auto-complete and snippets
 ;;  (setq my-snippets-root "~/.emacs.d/packages/yasnippet/")
;;   (setq my-yasnippet-directory-root "~/.emacs.d/data/Snippets/")
;;   (require 'yasnippet)
;;   (setq yas/trigger-key (kbd "S-SPC"))
;;   (yas/initialize)
;;   (add-hook 'yas/minor-mode-on-hook
;;             '(lambda ()
;;                (define-key yas/minor-mode-map yas/trigger-key 'yas/expand)))
;;   (setq yas/root-directory (list
;;                             (concat my-snippets-root "snippets")
;;                             (concat my-snippets-root "yasnippets-rails/rails-snippets")))
;;   (when (boundp 'my-yasnippet-directory-root) ; martial's snippets (03-2009)
;;     (add-to-list 'yas/root-directory my-yasnippet-directory-root)) ; main directory
;;   (mapc 'yas/load-directory yas/root-directory) ; don't use #'yas/load-directory
;;   (setq yas/global-mode t)
;; ;  (require 'auto-complete-yasnippet)
;;   )
;; (define-key c-mode-base-map [f7] 'c-reformat-buffer)

;;; ESPRESSO
(add-to-list 'auto-mode-alist '("\\.js$"    . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$"  . espresso-mode))

(provide 'web-programming)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-programming.el ends here


