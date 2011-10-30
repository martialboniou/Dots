;; require this file in any configuration file to load `.emacs' in another mode

(unless (boundp '*emacs/normal-startup*)
  (defvar *emacs/normal-startup* nil))
(when (and (not *emacs/normal-startup*) (not user-init-file))
  (let ((msg "init file post-load error: %s"))
    (condition-case err
	(load "~/.emacs.d/init")
      (error 
       (progn
	 (message (format msg err))
	 (condition-case err
	     (load "~/.emacs")
	  (error
	   (progn
	     (message (format msg err))
	     (condition-case err
		 (load "~/_emacs")
                 (error
		  (message (format
			    (concat msg
				    " no candidate found: ")
			    err))))))))))))

(provide 'kernel)
