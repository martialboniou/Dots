;;; EL-GET SELECT
;;
(require 'noaccess)

(add-to-list 'load-path (expand-file-name "el-get"
                          (expand-file-name "el-get"
                            (expand-file-name mars/local-root-dir))))

(unless (require 'el-get nil t)
  (url-retrieve                         ; MUST USE BEFORE 'FLIM REQUIREMENT
   "https://raw.github.com/martialboniou/el-get/master/el-get-install.el"
   (lambda (s)
     (let ((el-get-install-branch "master"))
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(add-to-list 'el-get-recipe-path (expand-file-name "Recipes"
                                                   (expand-file-name mars/personal-data 
                                                                     mars/local-root-dir)))
(setq el-get-sources '((:name nxhtml
                              :load nil) ; choose 'NXHTML or 'MULTI-WEB-MODE
                                        ;  in 'WEB-PROGRAMMING
                       (:name ropemacs
                              :build '("python setup.py install"))))

(defvar mars/packages '(el-get
                        color-theme
                        escreen
                        vimpulse
                        keats
                        shen-mode
                        haskellmode-emacs
                        pylookup
                        pymacs
                        mailcrypt
                        auto-complete
                        auto-pair-plus
                        org-mode
                        howm
                        remember
                        revive-plus
                        multi-web-mode
                        markdown-mode
                        magit
                        undo-tree
                        git-emacs
                        switch-window
                        emacs-w3m
                        yaml-mode
                        yasnippet
                        sunrise-commander
                        wanderlust))
(locate-library "inf-ruby")
(condition-case nil
    (progn
      (el-get-executable-find "rake")
      (add-to-list 'el-get-sources '(:name yasnippet
                                           :build '("rake compile")))
      (add-to-list 'el-get-sources '(:name inf-ruby-bond
                                           :depends nil)) ; use RINARI's INF-RUBY
      (setq mars/packages (nconc mars/packages '(rinari
                                                 ruby-electric))))
  (error (message "el-select: yasnippet won't be compiled and rinari and other packages for ruby won't be installed without rake, a simple ruby build program.")))

(setq mars/packages
  (nconc mars/packages
         (mapcar #'el-get-source-name el-get-sources)))

(condition-case nil
    (progn
      (el-get-executable-find "latex")
      (add-to-list 'mars/packages "auctex"))
  (error (message "el-select: AUCTeX won't be installed without TeXLive or any modern LaTeX distribution.")))

(el-get 'sync mars/packages)
(el-get 'wait)

(provide 'el-select)            ; required by 'ADAPTER

