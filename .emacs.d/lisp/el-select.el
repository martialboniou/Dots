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
                              :features nil) ; let 'VIM-EVERYWHERE do the loading
                       (:name nxhtml
                              :load nil))) ; choose 'NXHTML or 'MULTI-WEB-MODE
                                           ; in 'WEB-PROGRAMMING
;; :name ropemacs :build '(("python" "setup.py" "install" "||" "sudo" "python" "setup.py" "install"))

(defvar mars/packages '(el-get
                        color-theme
                        escreen
                        evil-leader     ; install evil & undo-tree
                        evil-surround
                        keats
                        shen-mode
                        haskellmode-emacs
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
                        revive-plus
                        multi-web-mode
                        markdown-mode
                        magit
                        ;; darcsum
                        header2
                        filladapt
                        hideshowvis
                        git-emacs
                        gist
                        switch-window
                        cycle-buffer
                        js-comint
                        yaml-mode
                        emms            ; install emacs-w3m
                        sunrise-commander
                        bbdb
                        wanderlust))

(defvar mars/personal-recipes '(yas-jit) ; my version loads 'YASNIPPET
  )

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
      (setq mars/packages (nconc mars/packages '(ruby-electric))))
  (error (message "el-select: yasnippet won't be compiled and rinari and other packages for ruby won't be installed without rake, a simple ruby build program.")))

(condition-case nil
    (progn
      (el-get-executable-find "latex")
      (add-to-list 'el-get-sources '(:name "auctex"
                                           :build `("./autogen.sh" ,(concat "./configure --with-lispdir=`pwd` --with-emacs=" el-get-emacs " --with-texmf-dir=" (let ((texmf-dir (getenv "TEXMF_DIR"))) (when (or (null texmf-dir) (string= texmf-dir "")) (setq texmf-dir (if (eq system-type 'gnu/linux) "/usr/share/texmf-texlive" "/usr/local/texlive/texmf-local"))) texmf-dir)) "make"))))
  (error (message "el-select: AUCTeX won't be installed without TeXLive or any modern LaTeX distribution.")))

(setq mars/packages
  (nconc mars/packages
         (mapcar #'el-get-source-name el-get-sources)))

(el-get 'sync mars/packages)
;; (el-get 'sync mars/personal-recipes)
(el-get 'wait)

(provide 'el-select)            ; required by 'ADAPTER

