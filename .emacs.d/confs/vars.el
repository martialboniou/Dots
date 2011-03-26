;;; vars.el ---
;;
;; Filename: vars.el
;; Description:
;; Author: Martial Boniou
;; Maintainer:
;; Created: Wed Feb 23 11:22:37 2011 (+0100)
;; Version:
;; Last-Updated: Sat Mar 26 21:52:42 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 54
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: common variables for all configurations
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

;;; CONVENTIONS
;; a `path' is a list of relative or absolute directories
;; a `dir'  is a relative directory
;; a `rep'  is an absolute directory

;;; GLOBAL PATH
(defvar mars/local-root-dir (if (boundp 'user-emacs-directory) user-emacs-directory "~/.emacs.d"))
(defvar mars/temporary-dir (file-name-as-directory "/tmp"))

;;; DIRECTORY NAME
(defvar mars/personal-data "data")
(defvar mars/desktop-name "desktop")

;;; DIRECTORIES
(defvar mars/local-conf-path (list "confs" "confs/init"))
(defvar mars/site-lisp-path (list "lisp")) ; subdirs are loaded in 'load-path too (FIXME: need reboot .emacs if Custom)
(let ((pases-source-dir (expand-file-name
                         (concat
                          (file-name-as-directory "~")
                          (file-name-as-directory ".pases.d"))))) ; or (locate-library "wl") if standard install
  (when (file-exists-p pases-source-dir) ; IMPORTANT: if `pases' is installed with `confs/packs.el', reboot Emacs
    (unless (fboundp 'remove-if-not)
     (require 'cl))
    (let ((wl-name-list (remove-if-not (lambda (x)
                                         (and (string-match "^wl" x)
                                              (null (string-match ".pases$" x))))
                                       (directory-files pases-source-dir))))
      (when wl-name-list
        (defvar wl-resource-rep (concat pases-source-dir
                                        (file-name-as-directory
                                         (car (last wl-name-list))))
          "Wanderlust resource repository."))))) ; used in `confs/mail.el'

;;; GLOBAL SYSTEM CUSTOMIZATION
;; general behavior/convention (`custom-file' being specific to a system)
(let ((common-pre-custom (expand-file-name
                          (concat
                           (file-name-directory load-file-name)
                           (file-name-as-directory "init")
                           "common-pre-custom"))))
  (condition-case err
      (load common-pre-custom)
    (error
     (message "vars: global system customization loading error: %s" err)
     (sleep-for emacs/breaktime-on-error))))

;;; DATA PATH
(defvar c-include-path nil "Additional include path for C programs.")
(defvar cpp-include-path nil "Additional include path for C++ programs")

;;; PROGRAM NAMES
(defvar mars/haskell-program-name "/usr/bin/ghci"
  "Haskell interpreter fullname.")
(custom-set-variables
 '(tramp-default-method "ssh"))

;;; SPECIFICS (<data>/sys/vars-<hostname>.el or <data>/vars-<hostname>.el)
(let ((sys-rep (concat (file-name-as-directory mars/local-root-dir)
                       (file-name-as-directory mars/personal-data))))
  (let ((sys-subrep (concat sys-rep (file-name-as-directory "sys"))))
    (when (file-exists-p sys-subrep)
      (setq sys-rep sys-subrep))
    (condition-case nil
        (load-file (concat sys-rep "vars-" system-name ".el"))
      (error nil))))

;;; LOAD `USER-INIT-FILE' IF EXTERNAL CALL
(setq *emacs/normal-startup t)
(unless (boundp '*emacs/normal-startup*)
  (defvar *emacs/normal-startup* nil))
(when (and (not *emacs/normal-startup*) (not user-init-file))
  (let ((msg "init file post-load error: %s"))
    (condition-case err
        (load "~/.emacs")
      (error (progn
               (message (format msg err))
               (condition-case err
                   (load "~/_emacs")
                 (error (message (format (concat msg " no candidate found: ") err)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vars.el ends here

