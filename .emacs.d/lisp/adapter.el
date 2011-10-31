(add-to-list 'load-path (expand-file-name (file-name-directory load-file-name)))

(require 'walker)

;; automagic
(generate-loaddefs)

(setq renew-autoloads-at-startup nil)   ; reset to prevent slow reloading

(defun update-autoloads-on-kill ()
  "Update autoloads on kill iff emacs boots correctly."
  (when (require 'kernel)
    (update-autoloads-in-package-area)))
(add-hook 'kill-emacs-hook #'update-autoloads-on-kill)

;; handmade
(mars/autoload '(("unbound"                   describe-unbound-keys)
                 ("tiling"                    tiling-cycle tiling-master)
                 ("buffer-move"               buf-move-down buf-move-up
                                      buf-move-left buf-move-right)
                 ("iswitchb"                  iswitchb-minibuffer-setup)
                 ("sunrise-commander"         sunrise sr-virtual-mode)
                 ("anything"                  anything anything-config)
                 ("anything-show-completion"  use-anything-show-completion)
                 ("autopair"                  autopair-mode)
                 ("header2"                   auto-make-header auto-update-file-header)
                 ("psvn"                      psvn)
                 ("hippie-exp"                hippie-expand he-init-string 
                                      he-substitute-string)
                 ("calc-ext"                  calc-do-calc-eval)
                 ("hexview-mode"              hexview-find-file)
                 ("simple-call-tree"          simple-call-tree-analyze simple-call-tree-alist)
                 ("inf-shen"                  shen-mode)
                 ("pymacs"                    pymacs-apply pymacs-call pymacs-eval 
                                              pymacs-exec pymacs-load)
                 ("markdown-mode"             markdown-mode)
                 ("yaml-mode"                 yaml-mode)
                 ("newsticker"                newsticker-start newsticker-show-news)
                 ("wl-mailto"                 wl-mailto-compose-message-from-mailto-url)
                 ("emms-source-file"          emms-dired emms-add-directory-tree 
                                      emms-add-directory emms-add-file)
                 ("emms"                      emms-history-load emms-playlist-buffer-list 
                                      emms)
                 ("emms-streams"              emms-streams emms-stream-init)))
(when window-system
  (mars/autoload
   '(("hideshowvis"     hideshowvis-enable hideshowvis-minor-mode))))

(provide 'adapter)
