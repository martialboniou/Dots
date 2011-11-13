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

(setq el-get-sources '((:name bbdb
                        :url "git://github.com/emacsmirror/bbdb.git"
                        :build `(,(concat "autoconf; ./configure EMACS=" el-get-emacs "; make autoloadsc; make")))))

(defvar mars/packages
  (append '(el-get
            switch-window
            nxhtml
            bbdb
            wanderlust)
          (mapcar #'el-get-source-name el-get-sources)))

(el-get 'sync mars/packages)
(el-get 'wait)

(provide 'el-select)            ; required by 'ADAPTER
