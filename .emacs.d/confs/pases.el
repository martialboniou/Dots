;;; pases.el --- 
;; 
;; Filename: pases.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 12:33:51 2011 (+0100)
;; Version: 
;; Last-Updated: Wed Mar  9 15:58:19 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 4
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
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

(unless (boundp 'mars/local-root-dir) (condition-case nil (load (concat (file-name-directory load-file-name) "vars")) (error "Unable to get custom variables")))

;;; PASES
;; Erik Hetzner's Pases (http://e6h.org/pases/)
(let ((pases-name "pases")
      (pases-version "0.2")
      (dot ".") (hdot "~/.") (d ".d"))
  (let ((pases-source-dir (expand-file-name (concat hdot pases-name d)))
        (pases-dir (expand-file-name (concat hdot pases-name dot (which-emacs-i-am) dot
                                             (number-to-string emacs-major-version) d))))
  (if (file-exists-p pases-dir)
      (progn
        (add-to-list 'load-path pases-dir)
        ;; TODO: rebuild `update-autoloads-in-package-area' to create/update loaddefs and `safe-autoloads-load' to load-file it in the case of a different `base' directory.
        ;; (if pases-source-dir
        ;;    (let ((f (concat (file-name-as-directory pases-source-dir) "loaddefs.el")))
        ;;        (if (file-exists-p p)
        ;;           (safe-autoloads-load p))) 
        (mars/autoload '(("pases" pases:install-package pases:uninstall-package
                          pases:disable-package pases:enable-package)
                         ("wl" wl wl-other-frame)
                         ("wl-draft" wl-draft wl-user-agent-compose))))
    (load (expand-file-name (concat (file-name-as-directory pases-source-dir)
                                    (file-name-as-directory 
                                     (concat pases-name "-" pases-version))
                                    "pases-load"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pases.el ends here
