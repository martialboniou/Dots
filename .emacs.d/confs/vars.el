;;; vars.el ---
;;
;; Filename: vars.el
;; Description:
;; Author: Martial Boniou
;; Maintainer:
;; Created: Wed Feb 23 11:22:37 2011 (+0100)
;; Version:
;; Last-Updated: Fri Oct 21 20:14:41 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 100
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

;;; DIRECTORIES
(defvar mars/local-conf-path (list "confs" "confs/init"))
(defvar mars/site-lisp-path (list "lisp")) ; subdirs are loaded in 'load-path too (FIXME: need reboot .emacs if Custom)
(defvar wl/pases-install nil "True if Wanderlust is a PASES package.")
(defvar wl-resource-rep nil "Wanderlust resource repository.")
(when wl/pases-install
    (let ((pases-source-dir (expand-file-name
                             (concat
                              (file-name-as-directory "~")
                              (file-name-as-directory ".pases.d"))))) ; or (locate-library "wl") if standard install
      (when (file-exists-p pases-source-dir) ; IMPORTANT: if `pases' is installed with `confs/packs.el', reboot Emacs
        (unless (fboundp 'remove-if)
          (require 'cl))
        (let ((wl-name-list (remove-if (lambda (x)
                                         (string-match ".pases$" x))
                                       (directory-files pases-source-dir nil "^wl"))))
          (when wl-name-list
            (setq wl-resource-rep (concat pases-source-dir
                                          (file-name-as-directory
                                           (car (last wl-name-list))))))))))

;;; UTILITIES
(unless (fboundp 'conf-locate)
    (defun conf-locate (conf) (let ((path (mapcar '(lambda (x) (concat (file-name-as-directory mars/local-root-dir) x)) mars/local-conf-path))) (locate-library conf nil path))))
(unless (fboundp 'conf-load)
  (defun conf-load (conf)               ; useful in non `*EMACS/NORMAL-STARTUP*' context
                                        ; eg: when loading a specific configuration file
                                        ;     by external call
    (let ((found (conf-locate conf)))
      (when found
        (load-file found)))))           ; normally bound when `USER-INIT-FILE' is loaded
(defmacro mars/force-options (&rest conses)
  "Initialize CDR from the value of CAR if CAR is bound.
`CONSES' is one or many CONS of variables."
  `(progn
     ,@(mapcar (lambda (x)
                 `(when (boundp ',(car x))
                    (setq ,(cdr x) ,(car x))))
               conses)))

;;; FIRST TIME
(unless (boundp '*i-am-a-vim-user*)     ; TODO: choose a definitive place for those vars
  (defvar *i-am-a-vim-user* t))
(unless (boundp '*i-am-a-dvorak-typist*)
  (defvar *i-am-a-dvorak-typist* t))    ; used iff loaded in <confs> context
(unless (boundp '*i-am-a-terminator*)
  (defvar *i-am-a-terminator* t))
(let ((first-file (expand-file-name
                   (concat (file-name-as-directory mars/local-root-dir)
                           (file-name-as-directory mars/personal-data)
                           ".launched"))))
  (if (file-exists-p first-file)
      (load-file first-file)
    (progn
      ;; if you changes the `.emacs' defvar by hand, the `.launched' won't change your setting
      (unless (y-or-n-p "Would you like to type in a Vim-like environment? ")
        (setq *i-am-a-vim-user* nil))
      (unless (y-or-n-p "Do you type with a Dvorak keyboard? ")
        (setq *i-am-a-dvorak-typist* nil))
      (unless (y-or-n-p "C-h & C-w for deletion [new cut-paste]? ")
        (setq *i-am-a-terminator* nil))
      (with-temp-file
          first-file
        (progn
          (insert (concat
                   ";; launched\n"
                   (unless *i-am-a-vim-user*
                       "(when (eq *i-am-a-vim-user* t) (setq *i-am-a-vim-user* nil))\n")
                   (unless *i-am-a-dvorak-typist*
                     "(when (eq *i-am-a-dvorak-typist* t) (setq *i-am-a-dvorak-typist* nil))\n")
                   (unless *i-am-a-terminator*
                     "(when (eq *i-am-a-terminator* t) (setq *i-am-a-terminator* nil))\n"))))))))
;; force vim/dvorak/term options via special vars
(mars/force-options (*vim-now*    . *i-am-a-vim-user*)
                    (*dvorak-now* . *i-am-a-dvorak-typist*)
                    (*term-now*   . *i-am-a-terminator*))

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
     (sleep-for 3))))
;; test with conf-locate / conf-load

;;; MINGW/MSYS COMPATIBILITY
(when (member system-type '(ms-dos windows-nt))
  (defvar mingw-executable-binary-suffixes
    '(".exe" ".com" ".bat" ".cmd" ".btm" ""))
  (defun mingw-executable-find (command)
    "Search for COMMAND in `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'."
    (let ((list exec-path)
          file)
      (while list
        (setq list
              (if (and (setq file (expand-file-name command (car list)))
                       (let ((suffixes mingw-executable-binary-suffixes)
                             candidate)
                         (while suffixes
                           (setq candidate (concat file (car suffixes)))
                           (if (and (file-exists-p candidate) ; `file-executable-p' is not convenient
                                    (not (file-directory-p candidate)))
                               (setq suffixes nil)
                             (setq suffixes (cdr suffixes))
                             (setq candidate nil)))
                         (setq file candidate)))
                  nil
                (setq file nil)
                (cdr list))))
      file))
  (defalias 'executable-find 'mingw-executable-find)
  (setq explicit-shell-file-name "bash")
  (setq shell-file-name "bash"))

;;; DATA PATH
(defvar c-include-path nil "Additional include path for C programs.")
(defvar cpp-include-path nil "Additional include path for C++ programs")

;;; PROGRAM NAMES
(defvar mars/haskell-program-name "ghci"
  "Haskell interpreter fullname.")
(custom-set-variables
 '(tramp-default-method "ssh"))
(if (member system-type '(windows-nt ms-dos))
         (setq ssl-program-name "openssl"
               ssl-program-arguments '("s_client" "-host" host "-port" service)) ; "-verify" "O" "-CApath" "/usr/lib/ssl/certs" "-quiet"
         (setq ssl-program-name "gnutls-cli" 
               ssl-program-arguments '("-p" service host)))
(defvar w3m-program-name "w3m"
  "The current program name of ye goo' olde W3M.")
(setq w3m-command w3m-program-name)     ; required by `anything-config'
(defvar factorcode-source-rep "~/Dynamics/factor/src/factor"
  "The up-to-date factor source repository. The Emacs environment
named FUEL must be found in the `misc/fuel' subdirectory.")
(defvar spelling-tool-name nil
  "The default program for spell checking. May be set to NIL.")

;;; SPECIFICS (<data>/sys/vars-<hostname>.el or <data>/vars-<hostname>.el)
(let ((sys-rep (concat (file-name-as-directory mars/local-root-dir)
                       (file-name-as-directory mars/personal-data)
                       (file-name-as-directory "sys"))))
  (unless (file-exists-p sys-rep)
    (make-directory sys-rep))
  (condition-case nil
      (load-file (concat sys-rep
                         "vars-"
                         (downcase system-name) "-"
                         (symbol-name system-type) ".el"))
    (error nil)))

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

