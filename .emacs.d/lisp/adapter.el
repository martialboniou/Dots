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
(mars/autoload '(("iswitchb"                  iswitchb-minibuffer-setup)
                 ("anything-show-completion"  use-anything-show-completion)
                 ("autopair"                  autopair-mode)
                 ("header2"                   auto-make-header auto-update-file-header)
                 ("hippie-exp"                hippie-expand he-init-string 
                                      he-substitute-string)
                 ("inf-shen"                  shen-mode)
                 ("pymacs"                    pymacs-apply pymacs-call pymacs-eval 
                                              pymacs-exec pymacs-load)
                 ("yaml-mode"                 yaml-mode)
                 ;; ("emms-source-file"          emms-dired emms-add-directory emms-add-file)
                 ("wl-mailto"                 wl-mailto-compose-message-from-mailto-url)))


(provide 'adapter)
