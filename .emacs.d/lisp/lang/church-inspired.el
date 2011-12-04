;;; church-inspired.el --- 
;; 
;; Filename: church-inspired.el
;; Description: Lisp
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Mar 16 20:38:43 2011 (+0100)
;; Version: 
;; Last-Updated: Thu Dec  1 12:01:57 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 4
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: quicklisp slime
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Note: for a standalone Common Lisp environment, don't boot SLIME
;;        manually but run it from the QUICKLISP directory with CLBUILD2
;;        script like this:
;; 
;;        $ EMACS="emacs -q -l church-inspired"       
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

;;; QUICKLISP SLIME
(add-lambda-hook 'emacs-startup-hook
  ;; check helper wasn't loaded at startup (using CLBUILD maybe)
 (unless (and (boundp 'quicklisp-slime-helper-dist) 
              (not (boundp 'mars/quicklisp-slime-rep)))
   (condition-case err
       (load (expand-file-name "slime-helper.el" mars/quicklisp-slime-rep))
     (error "church-inspired: quicklisp slime helper not loaded: %s" err))
   ;; NOTE: remember REPL may not be the same as the one loaded by another
   ;;       helper launcher like the CLBUILD2 internal slime script
   (eval-after-load "slime-autoloads"
     '(progn
        (when (and (boundp 'mars/common-lisp-program-name)
                   (executable-find mars/common-lisp-program-name))
          (setq inferior-lisp-program mars/common-lisp-program-name))
        (slime-setup '(slime-fancy slime-tramp slime-asdf))))
   (eval-after-load "slime"
     '(progn
        (slime-require :swank-listener-hooks)))))

(provide 'church-inspired)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; church-inspired.el ends here
