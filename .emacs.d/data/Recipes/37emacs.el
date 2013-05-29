(:name 37emacs
       :description "Majop client libraries for 37signals' various product APIs"
       :type github
       :build `(("make" ,(concat "EMACS=" el-get-emacs)))
       :features (backpack highrise)
       :pkgname "hober/37emacs")
