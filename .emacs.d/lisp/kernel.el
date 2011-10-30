;; require this file in any configuration file to load `.emacs' in another mode

(unless (boundp '*emacs/normal-startup*)
  (defvar *emacs/normal-startup* nil))

(require 'appearance)
(require 'packs)
(when *i-am-a-common-lisp-advocate*
  (unless (fboundp 'eieio-defclass)     ; `cedet' if no CLOS
    (safe-load-cedet)))
(require 'register)
(error "tada!")

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

(require 'formats)			; emacs as an universal typewriter (format + encodings)
(require 'crypto)			; emacs as a secret agent
(require 'window-manager)               ; emacs as a window-manager
(require 'shortcuts)                    ; emacs as a key commander

(when *i-am-an-emacsen-dev*
  (require 'ladybug))                   ; emacs as an elisp developer tool


;; bytecompile .emacs and files in `conf-path' on change
(when (functionp #'byte-compile-user-init-file)
  (defun mars/byte-compile-user-init-hook ()
    (when (equal buffer-file-name user-init-file)
      (add-hook 'after-save-hook #'byte-compile-user-init-file t t)))
  (add-hook 'emacs-lisp-mode-hook #'mars/byte-compile-user-init-hook))
;; (when (functionp 'byte-compile-emacs-config)
;;  (defun mars/byte-compile-emacs-config-hook ()
;;    (when (member (file-name-directory buffer-file-name)
;;                  (mapcar #'(lambda (x)
;;                             (expand-file-name
;;                              (file-name-as-directory
;;                               (concat (file-name-as-directory mars/local-root-dir) x))))
;;                          mars/local-conf-path))
;;      (add-hook 'after-save-hook #'byte-compile-emacs-config t t)))
;;  (add-hook 'emacs-lisp-mode-hook #'mars/byte-compile-emacs-config-hook))

;; restore windows archiver at startup
(add-hook 'emacs-startup-hook
          #'mars-windows-archiver-load-in-session)

;; fast kill emacs or not but confirm anyway
(defadvice update-autoloads-in-package-area (around mars/fast-kill-version activate)
  (unless mars/fast-kill
    (progn
      ad-do-it)))
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
		(message (prin1-to-string property))
		(setq found t))))
	  (if (and found (not (y-or-n-p (format "Are you sure you want to delete this frame? "))))
	      nil
	    (progn ad-do-it)))))))

;; reload emacs
(defun reload-emacs ()			; TODO: check it prolly might not work
  (let ((memo *emacs/normal-startup*))
    (setq *emacs/normal-startup* t)
    (load user-init-file)
    (setq *emacs/normal-startup* memo)))

;; load time
(let ((load-time (destructuring-bind (hi lo ms) (current-time)
                   (- (+ hi lo) (+ (first emacs-load-start)
                                   (second emacs-load-start))))))
  (message "Emacs loaded in %ds" load-time)
  (when (and *i-am-an-emacsen-dev* (fboundp 'display-external-pop-up))
    (display-external-pop-up "Emacs startup"
                             (concat "Emacs loaded in "
                                     (number-to-string load-time) "s"))))
(put 'narrow-to-region 'disabled nil)

;; flag boot w/o error
(defvar *emacs/boot-without-error*)

;;; DEPRECATED
;; (when (and (not *emacs/normal-startup*) (not user-init-file))
;;   (let ((msg "init file post-load error: %s"))
;;     (condition-case err
;; 	(load "~/.emacs.d/init")
;;       (error 
;;        (progn
;; 	 (message (format msg err))
;; 	 (condition-case err
;; 	     (load "~/.emacs")
;; 	  (error
;; 	   (progn
;; 	     (message (format msg err))
;; 	     (condition-case err
;; 		 (load "~/_emacs")
;;                  (error
;; 		  (message (format
;; 			    (concat msg
;; 				    " no candidate found: ")
;; 			    err))))))))))))

(provide 'kernel)
