;;; TEMP
(add-to-list 'load-path (expand-file-name "lisp"
                          (file-name-directory load-file-name)))
(delete-dups load-path)
(provide 'emacs-normal-startup)
(provide 'booting)
(require 'appearance)
(require 'noaccess)
(require 'vars)
(require 'defs)
;; el-get
(add-to-list 'load-path (expand-file-name "el-get"
                          (expand-file-name "el-get"
                            (expand-file-name mars/local-root-dir))))
(require 'el-get nil 'noerror)
(add-to-list 'el-get-recipe-path (expand-file-name "Recipes"
                                                   (expand-file-name mars/personal-data
                                                                     mars/local-root-dir)))
(setq el-get-sources '((:name evil
                              :features nil)
                       (:name evil-leader
                              :features nil)
                       (:name evil-surround
                              :features nil
                              :post-init nil)
                       (:name evil-numbers
                              :features nil) ; let 'VIM-EVERYWHERE configure 'EVIL
                       (:name highlight-parentheses-mode
                              :features nil) ; HIGHLIGHT-PARENTHESES-MODE is autoloaded
                       (:name bbdb           ; BBDB for wanderlust
                              :branch "v2.x"
                              :build `("autoconf" ,(concat "./configure --with-emacs=" el-get-emacs)
                                       "make clean" "rm -f lisp/bbdb-autoloads.el"
                                       "make autoloadsc info")
                              :features bbdb-autoloads
                              :info "texinfo")
                       (:name auctex
                              :branch "release_11_87"
                              :build `(("./autogen.sh")
                                       ("./configure"
                                        ,(if (or (string= "" mars/texmf-dir)
                                                 (null (file-accessible-directory-p mars/texmf-dir)))
                                             "--without-texmf-dir"
                                           (concat "--with-texmf-dir=" (expand-file-name mars/texmf-dir)))
                                        "--with-lispdir=`pwd`"
                                        ,(concat "--with-emacs=" el-get-emacs))
                                       "make lisp docs"
                                       "cd preview && make lisp && cd ..")
                              :load nil) ; let 'WIKI-WIKI launch 'TEX-SITE
                       (:name emms
                              :info nil ; TODO: 2013-05-23 fix info file
                              :build `(("make" ,(format "EMACS=%s" el-get-emacs)
                                        ,(format "SITEFLAG=\\\"--no-site-file -L %s/emacs-w3m/ \\\"" el-get-dir)
                                        "autoloads" "lisp" "emms-print-metadata"))
                              :features nil) ; let 'MEDIA configure 'EMMS
                       (:name nxhtml
                              :load nil))) ; choose 'NXHTML or 'MULTI-WEB-MODE
                                        ;    in 'WEB-PROGRAMMING
(condition-case nil
    (progn
      (el-get-executable-find "rake")
      (add-to-list 'el-get-sources '(:name yasnippet
                       :compile "yasnippet.el")))
  (error (message "yasnippet need rake to compile.")))
(el-get 'sync)
;; packs requires walker that adds vendor to path
(require 'packs)

(require 'adapter)

(require 'crypto)
(require 'vim-everywhere)
(require 'behave)

(when custom-file
  (load custom-file 'noerror))
(defadvice custom-buffer-create (before my-advice-custom-buffer-create activate)
  "Exit the current Customize buffer before creating a new one, unless there are modified widgets."
  (if (eq major-mode 'Custom-mode)
      (let ((custom-buffer-done-kill t)
            (custom-buffer-modified nil))
        (mapc (lambda (widget)
                (and (not custom-buffer-modified)
                     (eq (widget-get widget :custom-state) 'modified)
                     (setq custom-buffer-modified t)))
              custom-options)
        (if (not custom-buffer-modified)
            (Custom-buffer-done)))))

(require 'formats)
(require 'crypto)
(require 'window-manager)
(require 'file-manager)
(require 'shortcuts)

(put 'auto-byte-compile 'safe-local-variable #'booleanp)
(add-hook 'emacs-lisp-mode-hook #'auto-byte-compile-save-hook)
(add-lambda-hook 'kill-emacs-hook
  (byte-compile-new-files-in-directories-maybe
   (mapcar #'(lambda (dir)
               (expand-file-name dir
                                 mars/local-root-dir))
           mars/local-conf-path))
  (when user-init-file
    (byte-compile-new-files-maybe user-init-file)))
(when (fboundp 'update-autoloads-in-package-area)
  (defadvice update-autoloads-in-package-area (around mars/fast-kill-version activate)
    (unless mars/fast-kill
      (progn
        ad-do-it))))
(setq confirm-kill-emacs 'y-or-n-p)

;; confirm deleting frame iff multiple windows or buffer property in `confirm-frame-action-buffer-alist'
(defadvice delete-frame (around confirm-delete-frame
                (&optional frame force) activate)
  (if (< (length (frame-list)) 2)
      (kill-emacs)
    (let ((windows (window-list frame)))
      (if (> (length windows) 1)
          (if (y-or-n-p (format "Multiple windows in this frame detected. Close anyway? ")) (progn ad-do-it) nil)
        ;; one window case
        (let ((pending confirm-frame-action-buffer-alist)
              (buf (window-buffer (car windows)))
              found)
          (while (and (null found) pending)
            (let ((property (pop pending)))
              (when (member (with-current-buffer
                                buf
                              (symbol-value (car property))) (cdr property))
                (setq found t))))
          (if (and found (not (y-or-n-p (format "Are you sure you want to delete this frame? "))))
              nil
            ad-do-it))))))
(defun reload-emacs ()          ; TODO: check it prolly might not work
  (let ((memo (featurep 'emacs-normal-startup)))
    (unless memo
      (provide 'emacs-normal-startup))
    (load user-init-file)
    (unless memo
      (unintern 'emacs-normal-startup obarray))))
(let* ((load-time (destructuring-bind (hi lo ms &optional ps) (current-time)
                    (- (+ hi lo) (+ (first emacs-load-start)
                                    (second emacs-load-start)))))
       (load-time-msg (format "Emacs loaded in %d s" load-time)))
  (display-external-pop-up "Emacs startup" load-time-msg))
(put 'narrow-to-region 'disabled nil)
(unintern 'booting obarray)


(savehist-mode 1)

;; DEVELOPMENT
(eval-after-load "header2"
  '(progn
     (add-hook 'write-file-functions 'auto-update-file-header)
     (mars/add-hooks '(c-mode-common-hook emacs-lisp-hook) 'auto-make-header)))

;; MAILING DURING TEST PHASE
(require 'mail)

;; (wl)
