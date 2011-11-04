;;; vars.el ---
;;
;; Filename: vars.el
;; Description:
;; Author: Martial Boniou
;; Maintainer:
;; Created: Wed Feb 23 11:22:37 2011 (+0100)
;; Version:
;; Last-Updated: Fri Nov  4 22:53:33 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 115
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

;;; *MOOD*
;;
;; create *vim-now*/*dvorak-now*/*term-now* to force new vim/dvorak/term options
;; remove .emacs.d/data/.launched to reset vim/dvorak/term options at startup
;;
(defvar *i-am-a-vim-user* t
  "If true, Emacs will be Vimpyrized. (ViViVi, the beast.)
Set the boolean *vim-now* to shortcut this variable.")
(defvar *i-am-a-dvorak-typist* t
  "If true, additional Dvorak-friendly keybindings.
Set the boolean *dvorak-now* to shortcut this variable.")
(defvar *i-am-a-terminator* t
  "If true, C-h and C-w will be used as in any Un*x terminal.
Unless `*i-am-a-dvorak-typist*', `CUA' is activated in this
case to support additional cut-paste strategy (ie to replace
C-w as the usual 'cut' key binding).
Set the boolean *term-now* to shortcut this variable.")
(defvar *i-am-a-common-lisp-advocate* t
  "If true, require CL extensions (eg. EIEIO). Highly
recommended to boot CEDET faster.")
(defvar *i-am-an-emacsen-dev* t
  "If true, ELISP helpers will be loaded providing tools
to easily visualize ELISP macro expansions.")
(defvar *i-might-be-a-saiki-komon* t
  "If true, display an organizer window at startup.
A `saiki-komon' is a clan administrator inside gang
organization. (See `gtd' file for usage.)")
(defvar *i-can-do-yubitsume-now* t
  "If true, enable pinky-free helpers as STICKY-CONTROL.
A `yubitsume' is a japanese apologies' ritual which generally
consists in cutting off the portion of one's left little finger
above the top knuckle. In no-window-system mode, most of these
helpers is activated to work on most 70's designed VT where the
Ctrl-Shift combination is unknown.")
(defvar *i-like-shoji* t
  "If true, enable transparency in window-system mode. Shoji
are a japanese window divider consisting of translucent paper
over a frame.")

;;; GLOBAL PATH
;;
;; a `path' is a list of relative or absolute directories
;; a `dir'  is a relative directory
;; a `rep'  is an absolute directory
;;
;; DIRECTORY
(defvar mars/local-root-dir (if (boundp 'user-emacs-directory) user-emacs-directory "~/.emacs.d"))
(defvar mars/temporary-dir (file-name-as-directory "/tmp"))
;; DIRECTORY NAME
(defvar mars/personal-data "data")
;; DIRECTORIES
(defvar mars/local-conf-path (list "lisp" "lisp/init"))
(defvar mars/site-lisp-path (list "vendor")) ; subdirs are loaded in 'load-path too
;;; TIMERS
;;
(defvar emacs-load-start (current-time))
(defvar emacs/breaktime-on-error 3
  "Time (in seconds) of the pause on error.")
(defvar mars/fast-kill t)               ; (setq mars/fast-kill nil) to redo 'loaddefs on quit
;;; MISC
;;
(defvar auto-byte-compile nil
  "If true, automatic byte-compile emacs lisp files on file saving.")
(defvar renew-autoloads-at-startup nil) ; (re-)create autoloads (eg. after a change in `lisp/packs.el') FIXME: find a better process

;;; FIRST TIME
;;
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
(eval-after-load "defs"
  '(progn
     (mars/force-options (*vim-now*    . *i-am-a-vim-user*)
             (*dvorak-now* . *i-am-a-dvorak-typist*)
             (*term-now*   . *i-am-a-terminator*))))

;;; MINGW/MSYS COMPATIBILITY
;;
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
;;
(defvar c-include-path nil "Additional include path for C programs.")
(defvar cpp-include-path nil "Additional include path for C++ programs")
(let ((data-dir (concat (file-name-as-directory mars/local-root-dir)
                        (file-name-as-directory mars/personal-data))))
  (mapc (lambda (x) 
          (let ((newdir (file-name-as-directory (concat (file-name-as-directory "~/.emacs.d/data") x))))
        (unless (file-exists-p newdir)
          (make-directory newdir t))))
    '("cache/semanticdb"
      "cache/bookmark"
      "cache/newsticker/cache"
      "cache/newsticker/images"
      "cache/newsticker/groups"
      "cache/eshell"
      "cache/image-dired"
      "Notes" "Insert" "BBDB" "elmo"))
  (let ((data-cache (expand-file-name (file-name-as-directory "~/.emacs.d/data/cache"))))
    (unless (fboundp 'flet) (require 'cl)) ; elisp obviously needs R*RS | CL standard
    (flet ((cachize (file) (expand-file-name (concat (file-name-as-directory data-cache) file))))
      (setq mars-windows-archiver-file (cachize "windows-archiver")
        kiwon/last-window-configuration-file (cachize "last-window-configuration")
        desktop-dir data-cache
        desktop-base-file-name "desktop"
        desktop-base-lock-name (concat (if (memq system-type '(ms-dos windows-nt cygwin)) "_" ".") desktop-base-file-name ".lock") ; TODO: add a fun in defs named `prefix-hidden-file'
        ido-save-directory-list-file (cachize "ido-last")
        recentf-save-file (cachize "recentf")
        anything-c-adaptive-history-file (cachize "anything-c-adaptive-history")
        image-dired-dir (cachize "image-dired")
        bookmark-default-file (cachize "bookmark/emacs.bmk")
        bmkp-bmenu-commands-file (cachize "bookmark/bmenu-commands.el")
        bmkp-bmenu-state-file (cachize "bookmark/bmenu-state.el")
        newsticker-cache-filename (cachize "newsticker/cache")
        newsticker-imagecache-dirname (cachize "newsticker/images")
        newsticker-groups-filename (cachize "newsticker/groups")
        org-diary-agenda-file "~/.emacs.d/data/Notes/Diary.org"
        savehist-file (cachize "history")
        tramp-persistency-file-name (cachize "tramp")
        ac-comphist-file (cachize "ac-comphist.dat")
        semanticdb-default-save-directory (cachize "semanticdb")
        ede-project-placeholder-cache-file (cachize "projects.ede")
        ecb-tip-of-the-day-file (cachize "ecb-tip-of-day.el")
        auto-insert-directory "~/.emacs.d/data/Insert"
        bbdb-file (concat (file-name-as-directory "~/.emacs.d/data/BBDB")
                  (user-login-name)
                  ".bbdb")
        elmo-msgdb-directory "~/.emacs.d/data/elmo"
        wl-temporary-file-directory (expand-file-name "~/Downloads")
        eshell-directory-name (file-name-as-directory (cachize "eshell")) ; may need a final `slash'
        emms-cache-file (cachize "emms-cache")))))


;;; PROGRAM NAMES
;;
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
;;
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

(provide 'vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vars.el ends here

