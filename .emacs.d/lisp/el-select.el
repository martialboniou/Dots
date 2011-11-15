;; -*- no-byte-compile: t -*-
;;; EL-GET SELECT
;;
(require 'noaccess)

(add-to-list 'load-path (expand-file-name "el-get"
                          (expand-file-name "el-get"
                            (expand-file-name mars/local-root-dir))))

(unless (require 'el-get nil t)
  (url-retrieve                         ; MUST USE BEFORE 'FLIM REQUIREMENT
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path (expand-file-name "Recipes"
                                                   (expand-file-name mars/personal-data 
                                                                     mars/local-root-dir)))
(setq el-get-sources '((:name nxhtml
                              :load nil) ; choose 'NXHTML or 'MULTI-WEB-MODE
                                        ;  in 'WEB-PROGRAMMING
                       (:name anything
                              :features nil)
                       (:name emms
                              :features nil)
                       (:name ropemacs
                              :build '("python setup.py install"))
                       (:name darcsum
                              :compile ("."))))

(condition-case nil
    (progn
      (el-get-executable-find "rake")
      (add-to-list 'el-get-sources '(:name yasnippet
                                           :build '("rake compile")))
      (add-to-list 'el-get-sources '(:name rinari
                                           :url "https://github.com/eschulte/rinari.git"
                                           :features nil)))
  (error (message "el-select: yasnippet won't be compiled and rinari won't be installed without rake, a simple ruby build program.")))

(defvar mars/packages
  (append '(el-get
            color-theme
            vimpulse
            keats
            shen-mode
            haskellmode-emacs
            pylookup
            pymacs
            mailcrypt
            auto-pair-plus
            auto-complete
            org-mode
            howm
            remember
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
            wanderlust)
          (mapcar #'el-get-source-name el-get-sources)))

(condition-case nil
    (progn
      (el-get-executable-find "latex")
      (add-to-list 'mars/packages "auctex"))
  (error (message "el-select: AUCTeX won't be installed without TeXLive or any modern LaTeX distribution.")))

(el-get 'sync mars/packages)
(el-get 'wait)

(provide 'el-select)            ; required by 'ADAPTER
