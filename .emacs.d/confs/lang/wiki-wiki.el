;;; wikiwiki.el --- 
;; 
;; Filename: wikiwiki.el
;; Description: Markup and other Wiki text-modes
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Mar 16 20:02:05 2011 (+0100)
;; Version: 
;; Last-Updated: Sat Mar 19 16:38:07 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 9
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: markdown / textile / yaml-mode / haml-mode
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

(provide 'wiki-wiki)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wikiwiki.el ends here
