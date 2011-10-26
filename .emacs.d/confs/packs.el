;;; packs.el --- 
;; 
;; Filename: packs.el
;; Description: Maintain/install packages
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 12:33:51 2011 (+0100)
;; Version: 0.4
;; Last-Updated: Wed Oct 26 10:46:02 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 335
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
;; IMPORTANT: required programs are:
;; - bash, touch; 
;; - make, autoconf, rake (need ruby; `rvm' must be installed on Un*k-like);
;; - curl, cvs, svn, git, darcs (best installed with `cabal' coming with haskell-platform);
;; - hg/mercurial, bzr (both best installed with python egg manager `pip' (or `easy_install' via setuptools));
;; - tar, gzip, unzip.
;; Windows users, install mingw/msys/gnuwin and complete installation with ruby/gem, python/setuptools/bzr and haskell/cabal/darcs:
;; NOTE: try el-get to replace pases and all this
(defvar mars/site-lisp-package-tree nil
  "A package tree to get / install / tag additional packages in `mars/site-lisp-path'")
(setq mars/site-lisp-package-tree '((vimpulse     . ((get . "git clone git://gitorious.org/vimpulse/vimpulse")
                                                     (install . "make")))
                                    (autopair       . ((get . "svn checkout http://autopair.googlecode.com/svn/trunk/ autopair")
                                                       (install . "emacs-compile-directory")))
                                    (auto-pair-plus . ((get . "git clone git://github.com/emacsmirror/auto-pair-plus.git")))
                                    ;; emms main branch on git://git.sv.gnu.org/emms.git
                                    (emms         . ((get . "git clone git://github.com/martialboniou/emms.git")
                                                     (install . "make; make emms-print-metadata")
                                                     (nosearch . ("bin" "doc" "src"))))
                                    (anything-config . ((get . "git clone git://repo.or.cz/anything-config.git")
                                                        (install . "make")
                                                        (nosearch . ("developer-tools" "doc"))))
                                    (undo-tree    . ((get . "git clone http://www.dr-qubit.org/git/undo-tree.git")
                                                     (install . "emacs-compile-directory") ; do it from emacs
                                                     ))
                                    (yaml-mode    . ((get . "git clone git://github.com/yoshiki/yaml-mode.git")
                                                     (install . "make")))
                                    (keats        . ((get . "git clone git://github.com/rejeep/keats.git")
                                                     (install . "emacs-compile-directory")))
                                    (howm-1.3.9.1 . ((get . "curl http://howm.sourceforge.jp/a/howm-1.3.9.1.tar.gz | tar zx")
                                                     (install . "./configure; make")
                                                     (nosearch . ("doc" "en" "ext" "ja" "sample"))))
                                    (haskellmode-emacs . ((get . "darcs get http://code.haskell.org/haskellmode-emacs")
                                                          (install . "make"))) ; TODO: compile
                                    (git-emacs     . ((get . "git clone git://github.com/tsgates/git-emacs.git")
                                                      (install . "make")
                                                      (nosearch . "docs")
                                                      (noauto . ".")))
                                    (magit         . ((get . "git clone git://github.com/philjackson/magit.git")
                                                      (install . "./configure; make")))
                                    (markdown-mode . ((get . "git clone git://jblevins.org/git/markdown-mode.git")
                                                      (install . "emacs-compile-directory")))
                                    (yasnippet     . ((get . "svn checkout http://yasnippet.googlecode.com/svn/trunk/ yasnippet")
                                                      (install . "rake compile")
                                                      (nosearch . ("doc" "extras" "pkg" "snippets"))))
                                    (mailcrypt-3.5.9 . ((get . "curl -L http://sourceforge.net/projects/mailcrypt/files/mailcrypt/3.5.9/mailcrypt-3.5.9.tar.gz/download | tar zx") ; -L b/c URL redirect
                                                        (install . "autoconf; ./configure; make")
                                                        (nosearch . ("autom4te.cache" "tests"))))
                                    (mhc           . ((get . "git clone git://github.com/yoshinari-nomura/mhc.git")
                                                      (install . "emacs-compile-directory emacs") ; ruby configure.rb; ruby make.rb is OBSOLETE (ftools dependencies)
                                                      (nosearch . ("icons" "ruby-ext" "samples" "xpm"))))
                                    (mu-cite-201006212322 . ((get . "curl http://www.jpl.org/elips/mu/snapshots/mu-cite-201006212322.tar.gz | tar zx")
                                                             ;; no compilation yet / need apel10.8/poem
                                                             ))
                                    (newsticker-1.99 . ((get . "curl -L http://download.savannah.gnu.org/releases/newsticker/newsticker-1.99.tar.gz | tar zx")
                                                        (install . "emacs-compile-directory")))
                                    (color-theme-6.6.0 . ((get . "curl -L http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.tar.gz | tar zx")
                                                          (install . "emacs-compile-directory; emacs-compile-directory -eval \"(add-to-list 'load-path \\\"..\\\")\" themes"))) ; FIXME: not in path so error in `themes' (not important!)
                                    (darcsum           . ((get . "darcs get --lazy http://joyful.com/repos/darcsum")
                                                          (install . "emacs-compile-directory")))
                                    (shen-mode         . ((get . "git clone git://github.com/eschulte/shen-mode.git")
                                                          (install . "emacs-compile-directory")))
                                    (remember          . ((get . "git clone git://repo.or.cz/remember-el.git remember")
                                                          (install . "make")))
                                    (rinari            . ((get . "git clone git://github.com/eschulte/rinari.git; cd rinari; git submodule init; git submodule update; cd ..") ; FIXME: no compilation yet!
                                                          (nosearch . ("doc" "test" "util/jump/test" "util/test"))
                                                          ))
                                    ;; TODO: fetch all the Python/Rope install process
                                    ;; WARNING: `python-mode' is *not* compatible with `wisent-python' in SEMANTIC => use gallina-python.el (provided here)
                                    ;; (python-mode        . ((get . "curl -L http://launchpad.net/python-mode/trunk/5.2.0/+download/python-mode-5.2.0.tgz | tar zx")
                                    ;;                        (install . "emacs-compile-directory .")
                                    ;;                        (nosearch . "website")))
                                    ;; WARNING: python.el is installable but `generate-file-autoloads' function complains about the naming form
                                    ;; (python.el          . ((get . "git clone git://github.com/fgallina/python.el")
                                    ;;                        (install . "emacs-compile-directory")))
                                    (pylookup           . ((get . "git clone git://github.com/tsgates/pylookup.git") ; TODO: Issue 10 to merge for Python 2
                                                           (install . "emacs-compile-directory")
                                                           (nosearch . "python-2.7.1-docs-html"))) ; wtf this archive here / need a fork?
                                    (Pymacs             . ((get . "git clone git://github.com/pinard/Pymacs.Git")
                                                           (install . "make install; emacs-compile-directory") ; install Pymacs.egg in your python site-packges TODO: may require `eshell' for root install and su/sudo
                                                           (alert . "You may install PYMACS python part by running:\n\tpip install Pymacs\n\nThen don't forget to install rope and ropemode using pip (or easy_install)")
                                                           (nosearch . ("build" "contrib" "Pymacs" "tests"))))
                                    (ropemacs           . ((get . "hg clone https://bitbucket.org/agr/ropemacs")
                                                           (install . "python setup.py install")
                                                           ))
                                    (bbdb               . ((get . "cvs -d \":pserver:anonymous:@bbdb.cvs.sourceforge.net:/cvsroot/bbdb\" checkout bbdb")
                                                           (install . "autoconf;./configure;cd lisp;make autoloadsc;cd ..; make") ; soon DEPRECATED / IMPORTANT: problem in configure on Windows: `emacs' path with spaces
                                                           (nosearch . ("autom4te.cache" "bits/bbdb-filters/doc" "html" "tex" "texinfo" "utils"))))
                                    (cedet              . ((get . "bzr checkout bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk cedet")
                                                           (install . "make")
                                                           (nosearch . ("cogre/templates" "cogre/tests" "ede/templates" "semantic/doc" "semantic/tests" "srecode/templates" "testprojects" "www"))
                                                           (cedet . ".")))
                                    (ecb                . ((get . "cvs -z3 -d:pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb checkout -P ecb")
                                                           (install . "make CEDET=`echo $PWD/../cedet`;make autoloads EBATCH=\"emacs -batch -no-site-file -eval \\\"(add-to-list 'load-path \\\\\\\".\\\\\\\")\\\"\"") ; FIXME: assume cedet is in the same directory / windows users should use GNU bash TODO: test it on Win32/64
                                                           (nosearch . ("ecb-images" "html"))
                                                           (noauto . ".")))
                                    (apel               . ((get . "git clone git://github.com/wanderlust/apel.git")
                                                           (install . "make")))
                                    (flim               . ((get . "git clone git://github.com/wanderlust/flim.git")
                                                           (install . "make LISPDIR=..")
                                                           (nosearch . "tests")))
                                    (semi               . ((get . "git clone git://github.com/wanderlust/semi.git")
                                                           (install . "make LISPDIR=..")))
                                    (wanderlust         . ((get     . "cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/root checkout wanderlust")
                                                           ;; (get . "git clone git://github.com/wanderlust/wanderlust.git") ; is out of sync and induces error on emacs 24
                                                           (install . "echo \"(setq load-path (append (list \\\"$PWD/../apel\\\" \\\"$PWD/../flim\\\" \\\"$PWD/../semi\\\") load-path))(setq wl-install-utils t)\" > WL-CFG;make") ; let wl, elmo & utils *uninstalled*
                                                           (nosearch . ("doc" "etc" "samples" "tests"))))
                                    (org-mode           . ((get . "git clone git://orgmode.org/org-mode.git")
                                                           (install . "make; emacs-compile-directory contrib/lisp") ; compile contrib too
                                                           (nosearch . ("BUGFIXING" "EXPERIMENTAL" "UTILITIES" "contrib/babel" "contrib/doc" "contrib/odt" "contrib/scripts" "doc" "testing"))))
                                    (multi-web-mode     . ((get . "git clone git://github.com/fgallina/multi-web-mode.git")
                                                           (install . "emacs-compile-directory")))
                                    ;; TODO: nxhtml may be not used anymore (installer will be maintained to use `ert2' in `confs/elisp' and configuration will be available if required)
                                    (nxhtml             . ((get . "bzr branch lp:nxhtml")
                                                           ;; the following version is not fully compatible with 23 due to deprecated face + a bug I found about remove-hooking Viper in `util/mumamo'
                                                           ;; (get . "curl http://ourcomments.org/Emacs/DL/elisp/nxhtml/zip/nxhtml-2.08-100425.zip > _nxhtml.zip; unzip _nxhtml.zip; rm _nxhtml.zip") ; `unzip' is not pipe-friendly
                                                           (install . "cd util; sed \"/remove-hook 'text-mode-hook 'viper-mode/d\" mumamo.el > mumamo_tmp.el; mv mumamo_tmp.el mumamo.el; cd ..; emacs-compile-directory;cd nxhtml;emacs-compile-directory -eval \"(add-to-list 'load-path \\\"..\\\")\";cd ../related;emacs-compile-directory -eval \"(add-to-list 'load-path \\\"..\\\")\";cd ../util;emacs-compile-directory -eval \"(add-to-list 'load-path \\\"..\\\")\";")
                                                           (nosearch . ("alts" "etc" "nxhtml" "related" "tests" "util" ".bzr"))
                                                           (noauto . ".")))
                                    (emacs-w3m          . ((get . "cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot checkout emacs-w3m")
                                                           (install . "emacs -batch -q -no-site-file -l w3mhack.el NONE -f w3mhack-nonunix-install")
                                                           (nosearch . ("attic" "autom4te.cache" "doc" "icons" "icons30" "patches" "shimbun"))))
                                    (auctex-11.86       . ((get . "curl -L http://ftp.gnu.org/gnu/auctex/auctex-11.86.tar.gz | tar zx")
                                                           ;; TODO: (install . "emacs -Q -eval \"\" ;./configure --with-lispdir=. --with-texmf-dir=/usr/local/texlive/texmf-local;make")
                                                           (nosearch . ("doc" "images" "preview" "style"))))
                                    ;; (xwl-elisp          . ((get     . "git clone git://github.com/xwl/xwl-elisp.git")
                                    ;;                        (install . "make byte-compile"))) ; TODO: http://xwl.appspot.com/ (william xu) / include smart-operator
                                    ))             ; TODO: verify sig on get ?
(defun trim-string (string)
  "Remove white spaces in beginning and end of STRING. --xah"
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))
(defun mars/check-command (command &optional commands)
  "Check if command as executable in the `EXEC-PATH'. Throw an error by showing the
faulty COMMAND if COMMANDS is NIL or by listing all non-executable COMMAND and COMMANDS.
"
  (if (executable-find command)
      t
    (let ((error-sentence "Please install %s on your machine"))
      (if commands
          (let (renegades)
            (mapcar (lambda (exe)
                      (condition-case err
                          (mars/check-command exe)
                        (error (push exe renegades))))
                    (delete command (delete-dups commands)))
            (let ((rng-string (if (null renegades)
                                  ""
                                (concat
                                 (mapconcat 'identity renegades ", ") " and "))))
              (error (format error-sentence (concat rng-string command)))))
        (error (format error-sentence command))))))
(defun mars/fetch-exec-in-command (command)
  "Fetch executables in a command. The executables are the list of all binary inside the shell
commands except local binary.
"
  (delete-dups
   (mapcar '(lambda (sent)
              (car (split-string sent)))
           (remove-if '(lambda (s) 
                         (or (= 0 (length s))
                             (not (null (string-match "^./" s)))))
                                        ; remove empty strings and local binaries from the list
                                        ; TODO: test using bash on windows to check whether slash is enough
                      (mapcar (lambda (x)
                                (trim-string x))
                              (split-string command "[;&|]")))))) ; shell-command-separator-regexp
(defun mars/execute-commands (sentence &optional additional-commands)
  "Execute sentence as a shell script after checking executables are in EXEC-PATH. Check
`additional-commands' on error.
"
  (mapcar (lambda (com)
            (mars/check-command com additional-commands))
          (mars/fetch-exec-in-command sentence))
  (shell-command-to-string sentence))
(defun mars/fetch-exec-in-package-tree (package-tree &optional phase-list)
  "Fetch executables in a package tree. PHASE-LIST is the assoc keys to get shell commands in
a well-formed package tree. '(GET INSTALL) is the default PHASE-LIST. The executables are the
list of all binary inside the shell commands except local binary.
"
  (unless (fboundp 'remove-if)
    (require 'cl))
  (let (execs)
    (mapcar
     (lambda (x)
       (mapcar (lambda (zone)
                 (let ((elt (cdr (assoc zone (cdr x)))))
                   (when (and elt (stringp elt))
                     (let ((bins (mars/fetch-exec-in-command elt)))
                       (unless (null bins)
                         (if execs
                             (nconc execs bins)
                           (setq execs bins)))))))
               (or phase-list (list 'get 'install))))
     package-tree)
    (delete-dups execs)))
(defun mars/populate-site-lisp (&optional only-mark)
  "Check if a package exists. If no package found, fetch it, install it
and add it and its subdirs to load-path. If `only-mark' then `renew-autoloads-at-startup'
becomes true when an installation is required. So it will re-create AUTOLOADS later
in `.emacs'. Otherwise AUTOLOADS are generated immediately."
    (when mars/site-lisp-path
      (setq renew-autoloads-at-startup nil)
      (let ((site-lisp-path (mapcar (lambda (x)
                                      (expand-file-name
                                       (concat (file-name-as-directory mars/local-root-dir)
                                               (file-name-as-directory x)))) mars/site-lisp-path))
            executables-in-play)      ; list of binaries used in the current package
        (mapc (lambda (x)
                (let ((path site-lisp-path) (found nil) pending)
                  (while (and (not found) path)
                    (setq pending (pop path))
                    (when (file-directory-p (concat pending (symbol-name (car x))))
                      (setq found t)        ; FIXME: try to check thru 'load-path too (eg. 'vimpulse[/]' member)
                      ))
                  (unless found
                    (let ((get-method (assoc 'get (cdr x))))
                      (if (not get-method)
                          (error "No method to get the package named %s" (symbol-name (car x)))
                        (with-temp-buffer
                          (save-excursion
                            (save-restriction
                              (message "packs: %s installing..." (car x))
                              (cd (car site-lisp-path)) ; install package in the first site-lisp
                              (let ((get-command (cdr get-method)))
                                (when (null executables-in-play)
                                  (setq executables-in-play (mars/fetch-exec-in-package-tree mars/site-lisp-package-tree)))
                                (mars/execute-commands get-command executables-in-play))
                              (let ((install-method (assoc 'install (cdr x))))
                                (when install-method
                                  (cd (symbol-name (car x)))
                                  (let ((install-command (cdr install-method)))
                                    (mars/execute-commands install-command executables-in-play))))
                              (let (tag-alerted)
                                (dolist (tag '(nosearch noauto cedet))
                                  (let ((tag-method (assoc tag (cdr x))))
                                    (when tag-method
                                      (unless tag-alerted
                                        (setq tag-alerted t)
                                        (message "packs: %s tagging..." (car x)))
                                      (let ((tag-dirs (cdr tag-method)))
                                        (when (stringp tag-dirs)
                                          (setq tag-dirs (list tag-dirs)))
                                        (mapc '(lambda (dir)
                                                 (let ((dirname (concat (car site-lisp-path)
                                                                        (file-name-as-directory (symbol-name (car x)))
                                                                        (file-name-as-directory dir))))
                                                   (when (file-directory-p dirname)
                                                     (condition-case err
                                                         (with-temp-file
                                                             (concat dirname "." (symbol-name tag))
                                                           nil)
                                                       (error
                                                        (message "packs: unable to tag `%s' as %s: %s"
                                                                 dirname
                                                                 (symbol-name tag)
                                                                 err))))))
                                              tag-dirs))))))
                              ;; add new directory tree to `load-path'
                              (mars/add-to-load-path (directory-file-name
                                                      (concat
                                                       (car site-lisp-path)
                                                       (symbol-name (car x)))))
                              (setq renew-autoloads-at-startup t)
                              (message "packs: %s installed" (car x))
                              ))))))))
              mars/site-lisp-package-tree)
        (when (and (not only-mark)
                   renew-autoloads-at-startup) ; generate AUTOLOADS
          (let ((mars/loaddefs
                 (concat (file-name-as-directory mars/local-root-dir)
                         (file-name-as-directory (car mars/site-lisp-path))
                         "loaddefs.el")))
            (load "update-auto-loads")
            (update-autoloads-in-package-area)
            (safe-autoloads-load mars/loaddefs)))
        (setq renew-autoloads-at-startup nil))))
(defun mars/renew-site-lisp ()
  "Renew PATH and AUTOLOADS."
  (interactive)
  (setq renew-autoloads-at-startup t)   ; to force AUTOLOADS' regeneration
                                        ; for files at the root of the first
                                        ; `mars/site-lisp-path' directory
  (mars/populate-site-lisp))
(mars/populate-site-lisp t)             ; populate now!

;;; ELPA
;; nothing

;;; PASES
;; no more
;; Erik Hetzner's Pases (http://e6h.org/pases/)
(defun pases/wl-org-install ()
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
             (message (format "packs: bootstrap pases"))
             (let* ((pases-url (concat "http://launchpad.net/" pases-name "/trunk/" pases-version "/+download/"))
                    (buffer (condition-case err (url-retrieve-synchronously
                                                 (concat pases-url
                                                         "pases-bootstrap.el"))
                              (error (progn
                                       (message (format "packs: %s" err))
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
                 (error "packs: bootstraping error: no PASES:INSTALL-PACKAGE function")
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
                 (delete-other-windows)))))))))))
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
