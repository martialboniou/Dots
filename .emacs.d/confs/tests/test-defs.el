;;; VARS
(defvar mars/local-root-dir (if (boundp 'user-emacs-directory) user-emacs-directory "~/.emacs.d")) ; setting this variable avoids .emacs to be loaded from the file to test
(add-to-list 'load-path (expand-file-name "."))
(set 'message-log-max nil)              ; no *Messages* > faster
(progn                                  ; set the frame size  wide enough to test windows
                                        ; manipulation
  (set-frame-size (selected-frame) 48 48)
  (add-to-list 'default-frame-alist '(height . 48) '(width . 48)))

;;; FILE
(load "defs")

;;; UNIT TEST
;;
(require 'ert)

;; UTILS
(defun trap-messages (body)
  "Don't display messages."
   (require 'cl)
   (flet ((message (format-string &rest args) nil))
     (funcall body)))
(defun safe-funcall (function)
  (condition-case nil
      (funcall function)
    (error nil)))
(defun safe-kill-buffer (buffer-name)
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name)))
(defun safe-unintern (symbol)
  (when (boundp symbol)
    (unintern symbol)))

;;; TESTS
;;
(ert-deftest mars/generate-mode-hook-list-simple-test ()
  "Tests the composition of a list of hooks from a list of mode prefixes."
  (should (equal (mars/generate-mode-hook-list '(emacs-lisp)) '(emacs-lisp-mode-hook)))
  (should (equal (mars/generate-mode-hook-list '(cc lisp scheme)) '(cc-mode-hook
                                                                    lisp-mode-hook
                                                                    scheme-mode-hook))))

(ert-deftest mars/add-lambda-hook-simple-test ()
  "Tests the generation of LAMBDA from a list of sexps for hooks."
  (should (equal (macroexpand '(add-lambda-hook foo-hook (message "bar") (+ 1 2) (baz quux)))
                 '(add-hook foo-hook (function (lambda nil (message "bar") (+ 1 2) (baz quux)))))))

(defun foo-bar-mode-hook-context (body)
  (unwind-protect
      (progn
        (defvar foo-mode-hook nil)
        (defvar bar-mode-hook nil)
        (funcall body))
    (safe-unintern 'foo-mode-hook)
    (safe-unintern 'bar-mode-hook)))

(ert-deftest mars/add-hooks-simple-test ()
  "Tests the hook bindings."
  (foo-bar-mode-hook-context
   #'(lambda ()
       (mars/add-hooks '(foo-mode-hook bar-mode-hook) #'(lambda () (message "baz")))
       (should (equal (car foo-mode-hook) '(lambda nil (message "baz"))))
       (should (equal (car foo-mode-hook) '(lambda nil (message "baz")))))))

(defun quux-fundamental-buffer-context (body)
  (unwind-protect
      (progn
        (generate-new-buffer "quux")
        (switch-to-buffer "quux")
        (progn (insert "foo bar baz quux")
               (newline)
               (insert "zork gork bork")
               (newline) (newline))
        (funcall body))
    (safe-kill-buffer "quux")))

(ert-deftest first-line-of-buffer-simple-test ()
  (quux-fundamental-buffer-context
   #'(lambda ()
       (should (string= (first-line-of-buffer) "foo bar baz quux")))))

(ert-deftest count-buffers-simple-test ()
  (unwind-protect
      (let ((nbuf (count-buffers)))
        (generate-new-buffer "quux")
        (should (= (count-buffers) (1+ nbuf)))
        (generate-new-buffer "quuux")
        (should (= (count-buffers) (+ 2 nbuf))))
    (mapc #'(lambda (x)
              (safe-kill-buffer x))
          '("quux" "quuux"))))

(defun foo-dired-ibuffer-context (body)
  (unwind-protect
      (progn
        (defvar count-buffers)
        (set 'dired-use-ls-dired nil)
        (dired (expand-file-name "~"))
        (set 'ibuffer-expert t)
        (trap-messages #'ibuffer)
        (set 'count-buffers (count-buffers))
        (funcall body))
    (safe-unintern 'count-buffers)
    (safe-funcall #'ibuffer-quit)))

(ert-deftest kill-all-dired-buffers-simple-test ()
  (foo-dired-ibuffer-context
   #'(lambda ()
       (trap-messages #'kill-all-dired-buffers)
       (should (=  (1- count-buffers) (count-buffers))))))

(defun vsort-window-list (&optional frame)
  (unless frame
    (selected-frame))
  (let ((w (frame-first-window frame))
        l
        top)
  (while (and w (not (eq w top)))
    (unless top (set 'top w))
    (push w l)
    (setq w (next-window w)))
  (reverse l)))

(defun window-buffer-name ()
  (mapcar #'(lambda (w)
              (and (windowp w)
                   (buffer-name (window-buffer w))))
          (vsort-window-list (selected-frame))))

(defun three-windows-context (body)
  (unwind-protect
      (progn
        (mapc #'(lambda (x)
                  (generate-new-buffer (symbol-name x)))
              '(foo bar baz))        
        (switch-to-buffer "foo")
        (delete-other-windows)
        (mapc #'(lambda (x)
                  (split-window-above-each-other)
                  (switch-to-buffer (symbol-name x)))
              '(bar baz))               ; windows are (BAZ BAR FOO)        
        (funcall body))
    (mapc #'(lambda (x) (safe-kill-buffer (symbol-name x)))
          '(foo bar baz))
    (delete-other-windows)))

(ert-deftest transpose-buffers-complete-test ()
  (three-windows-context
   #'(lambda ()
       (transpose-buffers 1)
       (should (equal (window-buffer-name) '("bar" "baz" "foo"))) ; invert windows
       (should (equal (buffer-name
                       (window-buffer
                        (selected-window))) "baz")) ; stay in the same buffer
       (transpose-buffers 1)
       (should (equal (window-buffer-name) '("bar" "foo" "baz"))) ; invert windows
       (should (equal (buffer-name
                       (window-buffer
                        (selected-window))) "baz")) ; stay in the same buffer
       (select-window (previous-window))
       (transpose-buffers -1)
       (should (equal (window-buffer-name) '("foo" "bar" "baz"))) ; revert windows from 'FOO
       (should (equal (buffer-name
                       (window-buffer
                        (selected-window))) "foo")))))

(ert-deftest listify-simple-test ()
  (should (equal (listify 'var) '(var)))
  (should (equal (listify 1) '(1)))
  (should (equal (listify '(var)) '(var))))

;;; TEST RUNNER
(ert-run-tests-batch-and-exit)

(provide 'test-defs)
