;;; packs.el --- 
;; 
;; Filename: packs.el
;; Description: Maintain/install packages
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 12:33:51 2011 (+0100)
;; Version: 0.4
;; Last-Updated: Tue Mar 22 01:16:06 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 144
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
;; TODO: write a list of required cli program (especially for Windows users; for them, a recommended GNU binutils should be found):
;; tar, gzip, autoconf, make, svn, git, darcs, wget (curl if not?), bash, rake (rvm must be installed on Un*x-like)
(defvar mars/site-lisp-packages nil)
(setq mars/site-lisp-packages '((vimpulse     . ((get . "git clone git://gitorious.org/vimpulse/vimpulse")
                                             (install . "make")))
                                (emms         . ((get . "git clone git://git.sv.gnu.org/emms.git")
                                              (install . "make; make emms-print-metadata")
                                              (nosearch . ("bin" "doc" "src"))))
                                (undo-tree    . ((get . "git clone http://www.dr-qubit.org/git/undo-tree.git")
                                              (install . "emacs-compile-directory") ; do it from emacs
                                              ))
                                (yaml-mode    . ((get . "git clone git://github.com/yoshiki/yaml-mode.git")
                                              (install . "make")))
                                (keats        . ((get . "git clone git://github.com/rejeep/keats.git")
                                                 (install . "emacs-compile-directory")))
                                (howm-1.3.9.1 . ((get . "wget http://howm.sourceforge.jp/a/howm-1.3.9.1.tar.gz; tar xzvf howm-1.3.9.1.tar.gz; rm howm-1.3.9.1.tar.gz")
                                                 (install . "./configure; make")
                                                 (nosearch . ("doc" "en" "ext" "ja" "sample"))))
                                (haskellmode-emacs . ((get . "darcs get http://code.haskell.org/haskellmode-emacs")
                                                      (install . "make"))) ; TODO: compile
                                (magit         . ((get . "git clone git://github.com/philjackson/magit.git")
                                                  (install . "./configure; make")))
                                (markdown-mode . ((get . "git clone git://jblevins.org/git/markdown-mode.git")
                                                  (install . "emacs-compile-directory")))
                                (yasnippet     . ((get . "svn checkout http://yasnippet.googlecode.com/svn/trunk/ yasnippet")
                                                  (install . "rake compile")
                                                  (nosearch . ("doc" "extras" "pkg" "snippets"))))
                                (mailcrypt-3.5.9 . ((get . "wget http://sourceforge.net/projects/mailcrypt/files/mailcrypt/3.5.9/mailcrypt-3.5.9.tar.gz/download; tar xzvf mailcrypt-3.5.9.tar.gz; rm mailcrypt-3.5.9.tar.gz")
                                                    (install . "autoconf; ./configure; make")
                                                    (nosearch . ("autom4te.cache" "tests"))))
                                (mhc           . ((get . "git clone git://github.com/yoshinari-nomura/mhc.git")
                                                  (install . "emacs-compile-directory emacs") ; ruby configure.rb; ruby make.rb is OBSOLETE (ftools dependencies)
                                                  (nosearch . ("icons" "ruby-ext" "samples" "xpm"))))
                                (mu-cite-201006212322 . ((get . "wget http://www.jpl.org/elips/mu/snapshots/mu-cite-201006212322.tar.gz; tar xzvf mu-cite-201006212322.tar.gz; rm mu-cite-201006212322.tar.gz")
                                                         ;; no compilation yet / need apel10.8/poem
                                                         ))
                                (newsticker-1.99 . ((get . "wget http://download.savannah.gnu.org/releases/newsticker/newsticker-1.99.tar.gz; tar xzvf newsticker-1.99.tar.gz; rm newsticker-1.99.tar.gz")
                                                    (install . "emacs-compile-directory")))
                                ;; (circe          . ((get . "cvs -z3 -d:pserver:anonymous@cvs.savannah.nongnu.org:/sources/circe co circe")
                                ;;                    (install . "make")))
                                (color-theme-6.6.0 . ((get . "wget http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.tar.gz; tar xzvf color-theme-6.6.0.tar.gz; rm color-theme-6.6.0.tar.gz")
                                                      (install . "emacs-compile-directory; emacs-compile-directory -eval \"(add-to-list 'load-path \\\"..\\\")\" themes"))) ; FIXME: not in path so error in `themes' (not important!)
                                (darcsum           . ((get . "darcs get --lazy http://joyful.com/repos/darcsum")
                                                      (install . "emacs-compile-directory")))
                                (remember          . ((get . "git clone git://repo.or.cz/remember-el.git remember")
                                                      (install . "make")))
                                (rinari            . ((get . "git clone git://github.com/eschulte/rinari.git; cd rinari; git submodule init; git submodule update; cd ..") ; FIXME: no compilation yet!
                                                      (nosearch . ("doc" "test" "util/jump/test" "util/test"))
                                                      ))
                                ;; TODO: fetch all the Python/Rope install process
                                (python-mode        . ((get . "wget http://launchpad.net/python-mode/trunk/5.2.0/+download/python-mode-5.2.0.tgz; tar xzvf python-mode-5.2.0.tgz; rm python-mode-5.2.0.tgz")
                                                       (nosearch . "website")))
                                (Pymacs             . ((get . "git clone git://github.com/pinard/Pymacs.git")
                                                       (install . "make")
                                                       (nosearch . ("build" "contrib" "Pymacs" "temp" "tests"))))
                                
                         ))             ; TODO: verify sig on get ?
;; (setq mars/site-lisp-path '("../Desktop/test")) ; for test/debug
(when mars/site-lisp-path
  (let ((site-lisp-path (mapcar (lambda (x)
                                  (expand-file-name
                                   (concat (file-name-as-directory mars/local-root-dir)
                                           (file-name-as-directory x)))) mars/site-lisp-path)))
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
                      (error "No method to install %s" (symbol-name (car x)))
                    (with-temp-buffer
                      (save-excursion
                        (save-restriction
                          (cd (car site-lisp-path)) ; install package in the first site-lisp
                          (shell-command-to-string (cdr get-method))
                          (let ((install-method (assoc 'install (cdr x))))
                            (when install-method
                              (cd (symbol-name (car x)))
                              (shell-command-to-string (cdr install-method))))
                          (dolist (tag '(nosearch noauto cedet))
                            (let ((tag-method (assoc tag (cdr x))))
                              (when tag-method
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
                                        tag-dirs)))))
                          (message "packs: %s installed" (car x))
                          ))))))))
          mars/site-lisp-packages)))

;; (error "check")

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
