;;; EL-GET SELECT
;;
(require 'noaccess)

(add-to-list 'load-path (expand-file-name "el-get"
                          (expand-file-name "el-get"
                            (expand-file-name mars/local-root-dir))))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously ; MUST USE BEFORE 'FLIM REQUIREMENT
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(unless (boundp 'el-get-recipe-path)
 (error "el-get is not installed"))

(add-to-list 'el-get-recipe-path (expand-file-name "Recipes"
                                                   (expand-file-name mars/personal-data
                                                                     mars/local-root-dir)))

(setq el-get-sources '((:name evil
                              :features nil)
                       (:name evil-leader ; installs evil & undo-tree
                              :features nil)
                       (:name evil-surround
                              :features nil
                              :post-init nil)
                       (:name evil-numbers
                              :features nil) ; let 'VIM-EVERYWHERE configure 'EVIL
                       (:name ace-jump-mode
                              :after (progn
                                       (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
                                       (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
                                       (eval-after-load "evil-mode"
                                         '(define-key evil-normal-state-map
                                            (kbd "SPC") 'ace-jump-mode))
                                       (eval-after-load "viper"
                                         '(define-key viper-vi-global-user-map
                                            (kbd "SPC") 'ace-jump-mode)))
                              :features nil) ; let 'BEHAVE configure 'ACE-JUMP-MODE
                       (:name highlight-parentheses
                              :features nil) ; HIGHLIGHT-PARENTHESES-MODE is autoloaded
                       (:name bbdb           ; BBDB for wanderlust
                              :branch "v2.x"
                              :build `("autoconf" ,(concat "./configure --with-emacs=" el-get-emacs)
                                       "make clean" "rm -f lisp/bbdb-autoloads.el"
                                       "make autoloadsc info")
                              :features bbdb-autoloads
                              :info "texinfo")
                       (:name emms      ; installs emacs-w3m
                              :info nil ; TODO: 2013-05-23 fix info file
                              :build `(("make" ,(format "EMACS=%s" el-get-emacs)
                                        ,(format "SITEFLAG=\\\"--no-site-file -L %s/emacs-w3m/ \\\"" el-get-dir)
                                        "autoloads" "lisp" "emms-print-metadata"))
                              :features nil) ; let 'MEDIA configure 'EMMS
                       (:name git-emacs
                              :features nil)
;                       (:name nxhtml
;                              :load nil)
)) ; choose 'NXHTML or 'MULTI-WEB-MODE in 'WEB-PROGRAMMING
;; :name ropemacs :build '(("python" "setup.py" "install" "||" "sudo" "python" "setup.py" "install"))

(defvar mars/packages '(el-get
                        color-theme
                        escreen
                        keats
                        shen-mode
                        ;; haskellmode-emacs
                        pylookup
                        pymacs
                        mailcrypt
                        auto-complete
                        ac-slime
                        redshank
                        smex
                        anything
                        bookmark+
                        auto-pair-plus
                        dired-details
                        dired-plus
                        org-mode
                        howm
                        remember
                        multi-web-mode
                        markdown-mode
                        magit
                        ;; darcsum
                        header2
                        filladapt
                        hideshowvis
                        gist
                        switch-window
                        cycle-buffer
                        js-comint
                        textile-mode
                        yaml-mode
                        haml-mode
                        sunrise-commander
                        wanderlust
                        ;; from .emacs.d/data/Recipes
                        yas-jit         ; installs yasnippet
                        revive-plus
                        haskell-mode))

(condition-case nil
    (progn
      ;; TODO: find a way to install RINARI correctly
      (el-get-executable-find "rake")
      ;; YASNIPPET install is ugly (requires python *and* ruby)
      ;; check pygments module is installed
      (add-to-list 'el-get-sources '(:name yasnippet
                                           :build '("rake compile")))
      ;;(add-to-list 'el-get-sources '(:name rinari
      ;;                                     :build '(("git" "submodule" "init")
      ;;                                              ("git" "submodule" "update")
      ;;                                              ))) ; doc:install_info OBSOLETE b/c no ginstall-info revision
      ;(add-to-list 'el-get-sources '(:name inf-ruby-bond
      ;                                     :depends nil)
      ; ) ; use RINARI's INF-RUBY
      ;; TODO: restore ruby-electric or create a new rcp
      ;; (setq mars/packages (nconc mars/packages '(ruby-electric)))
      )
  (error (message "el-select: yasnippet won't be compiled and rinari and other packages for ruby won't be installed without rake, a simple ruby build program.")))

(condition-case nil
    (progn
      (el-get-executable-find "latex")
      (add-to-list 'el-get-sources '(:name auctex
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
                                           :load nil))) ; let 'WIKI-WIKI launch 'TEX-SITE
  (error (message "el-select: AUCTeX won't be automatically installed without TeXLive or any modern LaTeX distribution.")))

(setq mars/packages
  (nconc mars/packages
         (mapcar #'el-get-source-name el-get-sources)))
(setq mars/packages nil)
(el-get 'sync mars/packages)
(el-get 'wait)

(provide 'el-select)            ; required by 'ADAPTER
