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
;;; Commentary: load pases-load + install packages if needed
;; 
;; TODO: tidy this mess up
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

(when (eq system-type 'windows-nt)
  (unless (executable-find "unzip")
    (error "IMPORTANT: Windows need unzip to be installed. Please go to `http://gnuwin32.sourceforge.net/packages/unzip.htm', install the convenient setup program and add `C:\\Program Files\\GnuWin32\\bin' or your GnuWin32 binary path in your path")))

(defun mars/pases:get-package (name url local-path)
  "Download a package named `name'."
  (url-copy-file
   (concat "http://" url "/" name)
   (expand-file-name (concat (file-name-as-directory local-path)
			     name))
   t))


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
	;;; faster startup = pases autoload
        (add-to-list 'load-path pases-dir)
	(mars/autoload '(("pases" pases:install-package pases:uninstall-package
			  pases:disable-package pases:enable-package)
			 ("wl" wl wl-other-frame)
			 ("wl-draft" wl-draft wl-user-agent-compose))))
    (let ((pases-file (expand-file-name (concat (file-name-as-directory pases-source-dir)
					       (file-name-as-directory 
						(concat pases-name "-" pases-version))
					       "pases-load"))))
      (condition-case err
	    (load "~/.pases.d/pases-0.2/pases-load") ; (load pases-file)
	(error
	 (progn
	   ;; bootstrap if nothing to load
	   (message (format "pases: %s" err))
	   (let* ((pases-url (concat "http://launchpad.net/" pases-name "/trunk/" pases-version "/+download/"))
		  (buffer (condition-case err (url-retrieve-synchronously
					       (concat pases-url
						       "pases-bootstrap.el"))
			    (error (progn
				     (message (format "pases: %s" err))
				     nil)))))
	     (when buffer
	       (save-excursion
		 (set-buffer buffer)
		 (goto-char (point-min))
		 (re-search-forward "^$" nil 'move)
		 (eval-region (point) (point-max))
		 (kill-buffer (current-buffer)))
	     )))))
      ;; fetch and install essential packages after bootstrap
      (when (fboundp 'pases:install-package)
	(when (file-exists-p (expand-file-name 
			      (concat (file-name-as-directory pases-source-dir)
				      "pases-0.2.pases")))
	  (when (y-or-n-p
		 (format "Would you like to reset the pases source directory? ")) 
	    (delete-directory (expand-file-name pases-source-dir) t))
	  (make-directory pases-source-dir))
	(let ((pases-file-name "pases-0.2.pases"))
	  (mars/pases:get-package pases-file-name "launchpad.net/pases/trunk/0.2/+download" (expand-file-name (file-name-as-directory "~/.pases.d")))
	  (pases:install-package (concat (file-name-as-directory pases-source-dir) pases-file-name)))
	(let ((pases-packages-url (concat "e6h.org/" pases-name))
	      (pases-subdir (concat (file-name-as-directory pases-source-dir)
				    pases-name "-" pases-version)))
	  ;; Wanderlust
	  (mapc
	   (lambda (x)
	     (when
		 (mars/pases:get-package (concat x dot pases-name) pases-packages-url pases-source-dir)
	       (pases:install-package (expand-file-name
				       (concat (file-name-as-directory pases-source-dir)
					       x dot pases-name)))))
	   '("apel-10.8"
	     "flim-1.14.9_20100804"
	     "semi-1.14.6_20101024"
	     "wl-2.15.9"))
	  ;; Org 7
	  (mars/pases:get-package "org-mode-7.5.pases" "e6h.org/pases" "~/.pases.d")
	  (pases:install-package (expand-file-name "~/.pases.d/org-mode-7.5.pases"))
	  (delete-other-windows)))))))

;; TODO: rebuild `update-autoloads-in-package-area' to create/update loaddefs and `safe-autoloads-load' to load-file it in the case of a different `base' directory.
;; (if pases-source-dir
;;    (let ((f (concat (file-name-as-directory pases-source-dir) "loaddefs.el")))
;;        (if (file-exists-p p)
;;           (safe-autoloads-load p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pases.el ends here
