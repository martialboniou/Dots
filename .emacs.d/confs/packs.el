;;; packs.el --- 
;; 
;; Filename: packs.el
;; Description: Maintain/install packages
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 12:33:51 2011 (+0100)
;; Version: 0.4
;; Last-Updated: Mon Mar 21 18:59:52 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 62
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: populate site-lisp (install if unchecked) + elpa
;;              (nothing yet!) + pases (load or bootstrap)
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

;;; POPULATE SITE-LISP
;; - check if required subdirs are present in `mars/site-lisp-path'
;; - install in the first directory of `mars/site-lisp-path' (by default) if not found
;; - add required subdirs to LOAD-PATH if newly installed
(defvar mars/site-lisp-packages nil)
(setq mars/site-lisp-packages '((vimpulse . ((install . "git git://gitorious.org/vimpulse/vimpulse")
                                                ))))
(assoc 'install (cdr (car mars/site-lisp-packages)))
(unless mars/site-lisp-path
  (let ((site-lisp-path (mapcar (lambda (x)
                                  (expand-file-name
                                   (concat (file-name-as-directory mars/local-root-dir)
                                           (file-name-as-directory x)))) mars/site-lisp-path)))
    (mapc (lambda (x)
            (let ((path site-lisp-path) (found nil) pending)
              (while (null (or found path))
                (setq pending (pop path))
                (when (file-directory-p (concat pending (symbol-name (car x))))
                  (setq found pending)        ; FIXME: maybe check 'load-path too
                  ))
              (unless found
                (let ((install (assoc 'install (cdr x))))
                  (if (not install)
                      (error "No method to install %s" (symbol-name (car x)))
                    (
                     (save-excursion
                       (cd found)
                       )
                     ))
                 ))))
          mars/site-lisp-packages)))

(error "check")

;;; ELPA
;; nothing

;;; PASES
;; Erik Hetzner's Pases (http://e6h.org/pases/)
(unless (executable-find "unzip")     ; hang on unreachable `unzip'
  (cond
   ((eq system-type 'window-nt)
    (error "IMPORTANT: Pases need unzip to be installed. Please go to `http://gnuwin32.sourceforge.net/packages/unzip.htm', install the convenient setup program and add `C:\\Program Files\\GnuWin32\\bin' or your GnuWin32 binary path in your path"))
   (t (error "IMPORTANT: You need to install unzip and make it accessible to be able to use pases package manager"))))
(defun mars/pases:get-package (pkg-name url local-path)
  "Download a package named `name'."
  (let ((try-limit 2)                   ; retry if empty
        (complete-url (concat "http://" url "/" pkg-name))
        (file (expand-file-name
               (concat (file-name-as-directory local-path)
                       pkg-name)))
        (count 0))
    (while (and (< count try-limit)
                (or (not (file-exists-p file))
                    (eq 0 (nth 7 (file-attributes file))))) ; check empty file
      (incf count)
      (url-copy-file complete-url file t))))
(defvar mars/pases:loader-file-name nil
  "Relative path of the `pases' file to load. It must be in one of the directories to load pases packages from as set in the variable `pases:packages-dirs'.")
(let ((pases-name "pases")
      (pases-version "0.2")
      (dot ".") (hdot "~/.") (d ".d")
      (wanderlust-package-list '("apel-10.8"
                                 "flim-1.14.9_20100804"
                                 "semi-1.14.6_20101024"
                                 "wl-2.15.9"))
      (org-package-list '("org-mode-7.5")))
  (let ((pases-source-dir (expand-file-name (concat hdot pases-name d)))
        (pases-dir (expand-file-name (concat hdot pases-name dot (which-emacs-i-am) dot
                                             (number-to-string emacs-major-version) d))))
    (setq mars/pases:loader-file-name (concat (file-name-as-directory 
                                               (concat pases-name "-" pases-version))
                                              "pases-load"))
    (if (file-exists-p pases-dir)
        (progn
          ;; faster startup = pases autoload
          (add-to-list 'load-path pases-dir)
          (mars/autoload '(("pases" pases:install-package pases:uninstall-package
                            pases:disable-package pases:enable-package)
                           ("wl" wl wl-other-frame)
                           ("wl-draft" wl-draft wl-user-agent-compose))))
      (let ((pases-file (expand-file-name (concat (file-name-as-directory pases-source-dir)
                                                  mars/pases:loader-file-name))))
        (condition-case nil
            (load pases-file)
          (error
           (progn
             ;; bootstrap if nothing to load
             (message (format "pases: bootstrap pases"))
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
                   (kill-buffer (current-buffer)))))
             ;; fetch and install essential packages after bootstraping
             (if (not (fboundp 'pases:install-package))
                 (error "pases: bootstraping error: no PASES:INSTALL-PACKAGE function")
               (let ((pases-file-name (concat pases-name "-" pases-version dot pases-name)))
                 (when (file-exists-p (expand-file-name
                                       (concat (file-name-as-directory pases-source-dir)
                                               pases-file-name)))
                   (when (y-or-n-p
                                 (format "Would you like to reset the pases source directory? ")) 
                     (delete-directory (expand-file-name pases-source-dir) t)))
                 ;; fetch pases package and install it
                 (mars/pases:get-package pases-file-name "launchpad.net/pases/trunk/0.2/+download"
                                         (expand-file-name (file-name-as-directory pases-source-dir)))
                 (pases:install-package (concat (file-name-as-directory pases-source-dir) pases-file-name)))
               (let ((pases-packages-url (concat "e6h.org/" pases-name)))
                 ;; fetch wanderlust & org 7 packages and install them
                 (mapc
                  (lambda (x)
                    (mars/pases:get-package (concat x dot pases-name) pases-packages-url pases-source-dir)
                    (pases:install-package (expand-file-name
                                            (concat (file-name-as-directory pases-source-dir)
                                                    x dot pases-name))))
                  (append wanderlust-package-list org-package-list))
                 ;; FIXME: remove *Compile-Log* window instead of other-windows
                 (delete-other-windows))))))))))
(defun mars/pases:reload ()
      "Reload pases. Find the loader file inside the `pases:package-dirs'."
      (interactive)
      (let ((dirs pases:package-dirs)
            pending)
        (while (not (null dirs))
          (setq pending (pop dirs))
          (let ((file-name (expand-file-name
                            (concat (file-name-as-directory pending)
                                    mars/pases:loader-file-name))))
            (when (locate-library file-name)
              (setq dirs nil)
              (load file-name))))))

;; TODO: rebuild `update-autoloads-in-package-area' to create/update loaddefs and `safe-autoloads-load' to load-file it in the case of a different `base' directory.
;; (if pases-source-dir
;;    (let ((f (concat (file-name-as-directory pases-source-dir) "loaddefs.el")))
;;        (if (file-exists-p p)
;;           (safe-autoloads-load p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packs.el ends here
