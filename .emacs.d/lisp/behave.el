(require 'register)

;;; MAIN
;;
(setq standard-indent 4
      tab-width 4
      dired-use-ls-dired nil
      autosave-interval 50
      undo-limit 50000
      auto-compression-mode t
      backup-by-copying t
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t
      completion-ignore-case t
      custom-buffer-done-kill t
      newsticker-display-interval 15.3
      linum+-dynamic-format "%%%dd "
      linum+-smart-format linum+-dynamic-format)
(when (featurep 'ns)
  (setq ns-alternate-modifier nil
        ns-command-modifier 'meta       ; NeXT/Apple computers got real Meta keys
        ns-antialias-text t))

;;; ERASING/UNDOING
;;
(bind-key "<kp-delete>" 'delete-char)
(if (locate-library "undo-tree")
    (require 'undo-tree)              ; display tree by using C-x u
  (progn
    (require 'redo+)
    (eval-after-load "redo"
      '(progn (setq undo-no-redo t)))))

;;; MODAL EDITING incl. COLOR-THEME & PARENS
;;
(when *i-am-a-vim-user*
  (require 'vim-everywhere))		; otherwise see 'APPEARANCE

;;; DESKTOP & AUTOSAVE & SESSION
;;
;; - vars
(defvar the-desktop-file nil)           ; desktop
(defvar the-desktop-lock nil)
(defvar desktop-dir nil)
(defvar autosave-dir nil                ; autosave
  "Temporary variable use to make autosaves directory name.
That's where #foo# goes. It should normally be nil if
`user-init-file' is compiled.")
(defvar session-dir nil                 ; session
  "Temporary variable use to record interrupted sessions
for latter recovery. That's where .saves-<pid>-<hostname>
goes. It should normally be nil if `user-init-file' is
compiled. This directory is known as `auto-save-list'.")
(defvar backup-dir   nil                ; backup
  "Temporary variable use to make backups directory name.
That's where foo~ goes. It should normally be nil if
`user-init-file' is compiled.")
(defvar confirm-frame-action-buffer-alist nil ; kill frame alert
  "Associated list of buffer properties in order to get a confirmation
alert during action on the frame containing this buffer. A property
is a CONS formed by an information and a LIST of parameters for
this information.
Example: (MAJOR-MODE . (CHESS-MASTER-MODE MAIL-DRAFT-MODE).
See the advised `delete-frame' at the end of this file as a use case.")
;; - loading
(require 'desktop)
;; - filtering
(setq desktop-buffers-not-to-save "\\(^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|.*_flymake.*\\|^tags\\|^TAGS\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|.*\\.bbdb\\)$"
      desktop-files-not-to-save "^$"
      desktop-minor-mode-table '((auto-fill-function auto-fill-mode)
                                 (vc-mode nil)
                                 (vc-dired-mode nil)
                                 (flymake-mode nil)
                                 (ecb-minor-mode nil)
                                 (semantic-show-unmatched-syntax-mode nil)
                                 (semantic-stickyfunc-mode nil)
                                 (senator-minor-mode nil)
                                 (semantic-idle-scheduler-mode nil)))
(mapc #'(lambda (x)
          (add-to-list 'desktop-modes-not-to-save x))
      '(Info-mode 'dired-mode))
;; - window configuration case
(add-hook 'desktop-save-hook
          #'kiwon/save-window-configuration)
(add-hook 'desktop-after-read-hook
          #'kiwon/restore-window-configuration)  ; save/restore the last window
                                                 ; configuration with `DESKTOP'
;; - path
(unless desktop-dir
  (setq desktop-dir (expand-file-name
                     (concat
                      (file-name-as-directory mars/local-root-dir)
                      (file-name-as-directory mars/personal-data)
                      "desktop"))))
(unless (file-exists-p desktop-dir)
  (make-directory desktop-dir t))
(setq desktop-path (list desktop-dir)
      history-length 250)
;; - directories
(defmacro define-local-temporary-directory (local-work-directory-symbol)
  "Define the best temporary directory for registering files and sessions."
  (let ((local-tmp-dir (concat (file-name-as-directory mars/local-root-dir)
                               (file-name-as-directory mars/personal-data)
                               (file-name-as-directory "Temporary"))))
    (let ((dir-symbol (intern (concat (symbol-name local-work-directory-symbol) "-dir"))))
      (unless (symbol-value dir-symbol) ; creates directory iff unset (eg. `vars' may override)
                                        ; this <symbol>-dir must be 'NIL before compiling this macro
        (if (file-exists-p local-tmp-dir)
            `(setq ,dir-symbol ,(concat local-tmp-dir
                                        (file-name-as-directory
                                         (capitalize (concat
                                                      (symbol-name local-work-directory-symbol) "s")))))
          (let ((name (concat (file-name-as-directory mars/temporary-dir)
                              "emacs_" (symbol-name local-work-directory-symbol) "s/"
                              (file-name-as-directory (user-login-name)))))
            `(progn
               (setq ,dir-symbol ,name)
               (message ,(concat "Beware: your autosave directory named `" name
                                 "' may be publicly accessed. Be sure to make it hidden to other users.")))))))))
(define-local-temporary-directory autosave) ; #<files>#
(define-local-temporary-directory session)  ; .saves-<pid>-<hostname>
(define-local-temporary-directory backup)   ; !<backup-directory>!<backup-file>!.~<index>~
(setq auto-save-file-name-transforms `(("\\([^/]*/\\)*\\(.*\\)" ,(concat autosave-dir "\\2") nil))
      auto-save-list-file-prefix (concat (file-name-as-directory session-dir) ".saves-")
      backup-directory-alist (list (cons "." backup-dir)))
;; - desktop load
(when *emacs/normal-startup*
  (desktop-save-mode 1))
(make-directory autosave-dir t)         ; be sure it exists
(setq the-desktop-file (concat (file-name-as-directory desktop-dir)
                               desktop-base-file-name)
      the-desktop-lock (concat (file-name-as-directory desktop-dir)
                               desktop-base-lock-name))
(defun desktop-in-use-p ()
  (and (file-exists-p the-desktop-file) (file-exists-p the-desktop-lock)))
(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;; - desktop autosave
(when *emacs/normal-startup*
  (add-lambda-hook 'auto-save-hook
    (when (desktop-in-use-p)            ; desktop-save-in-desktop-dir w/o alert
      (if desktop-dirname
          (desktop-save desktop-dirname)
        (call-interactively #'desktop-save)))))
;; - unset temporary directory names
(setq autosave-dir nil
      session-dir nil
      backup-dir nil)                   ; otherwise `define-local-temporary-directory' compilation
                                        ; of the symbol test (in UNLESS clause) doesn't work

;;; BUFFERS
;;
;; - uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets ; name<foo/bar> | name<quux/bar>
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*"
      global-auto-revert-mode t)
;; - ibuffer
(eval-after-load "ibuffer"
  '(define-key ibuffer-mode-map (kbd "'") 'kill-all-dired-buffers))
;; - anything
(require 'anything-match-plugin)
(require 'anything-config)
(eval-after-load "anything-config"
  '(progn
     (defvar mars/anything-pers-action-binding "C-."
       "New binding for `anything-execute-persistent-action'. Was originally
the should-be-forbidden C-z.")
     (define-key anything-map (kbd "C-l") 'anything-find-files-down-one-level) ; originally C-. in 'window-system
     (define-key anything-map (kbd "C-z") nil)
     (define-key anything-map (read-kbd-macro mars/anything-pers-action-binding)
       'anything-execute-persistent-action)
     (setcdr (assoc 'persistent-help anything-c-source-advice)
             (concat "Describe function / C-u "
                     mars/anything-pers-action-binding
                     ": Toggle advice"))
     ;; ido case
     (eval-after-load "ido"
       '(progn
          nil
          ;; (anything-lisp-complete-symbol-set-timer 150) ; collect by 150 sec
          ;; (define-key emacs-lisp-mode-map "\C-\M-i"
          ;;   'anything-lisp-complete-symbol-partial-match)
          ;; (define-key lisp-interaction-mode-map "\C-\M-i"
          ;;   'anything-lisp-complete-symbol-partial-match)
          ;; (anything-read-string-mode 0)))))
          ))))

;;; MINIBUFFER
;;
;; - ido
(eval-after-load "ido"
  '(progn
     (setq confirm-nonexistent-file-or-buffer nil)
     (ido-mode 1)
     (setq ido-enable-tramp-completion nil
           ido-enable-flex-matching t
           ido-everywhere t
           ido-max-directory-size 100000
           ido-create-new-buffer 'always
           ido-enable-last-directory-history nil
           ido-confirm-unique-completion nil ; wait for RET, even for unique
           ido-show-dot-for-dired t         ; put . as the first item
           ido-use-filename-at-point 'guess ; ido guesses the context
           ido-use-url-at-point nil
           ido-default-file-method 'raise-frame ; you may ask if it should be displayed in the current
                                        ; window via `maybe-frame'. Let `ido-switch-buffer' do this.
           ido-default-buffer-method 'selected-window
           ido-ignore-extensions t)))   ; `completion-ignored-extensions'
;; - smex
(condition-case err
    (smex-initialize)
  (error "emacs: smex disabled: %s" err))

;;; FILES
;;
(recentf-mode 1)
(eval-after-load "recentf"
  '(progn
     (setq recentf-max-saved-items 50
           recentf-max-menu-items 30
           recentf-keep '(file-remote-p file-readable-p))
     (defun undo-kill-buffer (arg)
       "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
       (interactive "p")
       (let ((recently-killed-list (copy-sequence recentf-list))
             (buffer-files-list
              (delq nil (mapcar (lambda (buf)
                                  (when (buffer-file-name buf)
                                    (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
         (mapc
          (lambda (buf-file)
            (setq recently-killed-list
                  (delq buf-file recently-killed-list)))
          buffer-files-list)
         ;; (message "echo: %s" (prin1-to-string
         ;;                      (reduce 'cons recently-killed-list
         ;;                              :start 0 :end 10)))
         (find-file
          (if arg (nth arg recently-killed-list)
            (car recently-killed-list)))))
     ;; ido case
     (eval-after-load "ido"
       '(progn
	  ;; open recent files according to history of mini-buffer (incl. files search
	  ;; and management) or according to the list of recently loaded ones.
	  (defun ido-recentf-file-name-history ()
	    "Find a file in the `file-name-history' using ido for completion. Written by Markus Gattol."
	    (interactive)
	    (let* ((all-files 
		    (remove-duplicates
		     (mapcar 'expand-file-name
			     file-name-history) :test 'string=)) ; remove dups after expanding
		   (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
		   (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
		   (ido-make-buffer-list-hook
		    (lambda ()
		      (setq ido-temp-list filename-list)))
		   (filename (ido-read-buffer "File History: "))
		   (result-list (delq nil (mapcar 
					   (lambda (x) 
					     (if (string= (car x) 
							  filename) 
						 (cdr x))) 
					   file-assoc-list)))
		   (result-length (length result-list)))
	      (find-file
	       (cond
		((= result-length 0) filename)
		((= result-length 1) (car result-list))
		(t (let ((ido-make-buffer-list-hook
			  (lambda () (setq ido-temp-list result-list))))
		     (ido-read-buffer (format "%d matches:" result-length))))))))
	  (defun ido-recentf ()
	    "Use ido to select a recently opened file from the `recentf-list'. Written by xsteve."
	    (interactive)
	    (let ((home (expand-file-name (getenv "HOME"))))
	      (find-file (ido-completing-read "Recent File: "
					      (mapcar
					       (lambda (path)
						 (replace-regexp-in-string 
						  home "~" path)) 
					       recentf-list) nil t))))
	  (defmacro mars/recentf/override-keys (map)
	    "Force the keys overriding in some modes."
	    (list 'lambda nil
		  (list 'define-key map (list 'kbd '"C-c C-f") ''ido-recentf-file-name-history)
		  (list 'define-key map (list 'kbd '"C-c F") ''ido-recentf)
		  (list 'define-key map (list 'kbd '"C-c C-m") ''make-directory)))
	  ;; (add-lambda-hook 'emacs-startup-hook (mars/recentf/override-keys global-map))
	  ))))

;;; BOOKMARKS
;;
(require 'bookmark+)


(provide 'behave)
