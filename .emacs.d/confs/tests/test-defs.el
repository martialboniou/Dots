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

(defun generate-foo-list (&optional num symbol-flag)
  "Generates a list of *foo*. NUM defines the size and
SYMBOL-FLAG whether it's a list of strings or symbols."
  (unless (and (integerp num)
               (> num 0))
    (setq num 1))
  (let (list)
    (while (> num 0)
      (add-to-list 
       'list 
       (cond ((= num 1) (if symbol-flag 'foo "foo"))
             ((= num 2) (if symbol-flag 'bar "bar"))
             ((= num 3) (if symbol-flag 'baz "baz"))
             (t  (let ((quux (concat "q" (make-string (- num 3) ?u) "ux")))
                   (if symbol-flag (intern quux) quux)))))
      (setq num (1- num)))
    list))

(defun windows-context (num body)
  "create NUM split windows, executes the BODY lambda and destroy buffers
by restoring single window frame."
  (let ((foos (reverse (generate-foo-list num))))
    (unwind-protect
        (progn
          (mapc #'(lambda (x)
                    (generate-new-buffer x))
                foos)        
          (switch-to-buffer (car foos))
          (delete-other-windows)
          (mapc #'(lambda (x)
                    (split-window-above-each-other)
                    (switch-to-buffer x))
                (cdr foos))               ; hsplitted windows contains FOO, BAR, BAZ,..
          (funcall body))
      (mapc #'(lambda (x) (safe-kill-buffer x))
            foos)
      (delete-other-windows))))

(ert-deftest transpose-buffers-complete-test ()
  (windows-context
   3                                    ; 3 panes FOO / BAR / BAZ (focused on FOO)
   #'(lambda ()
       (transpose-buffers 1)
       (should (equal (window-buffer-name) '("bar" "foo" "baz"))) ; invert windows
       (should (eq (intern
                    (buffer-name
                     (window-buffer
                      (selected-window)))) 'foo)) ; stay in the same buffer
       (transpose-buffers 1)
       (should (equal (window-buffer-name) '("bar" "baz" "foo"))) ; invert windows
       (should (eq (intern
                    (buffer-name
                     (window-buffer
                      (selected-window)))) 'foo)) ; stay in the same buffer
       (select-window (previous-window))
       (transpose-buffers -1)
       (should (equal (window-buffer-name) '("baz" "bar" "foo"))) ; revert windows from 'FOO
       (should (eq (intern
                    (buffer-name
                     (window-buffer
                      (selected-window)))) 'baz))))) ; gone to the previous window

(ert-deftest listify-simple-test ()
  (should (equal (listify 'var) '(var)))
  (should (equal (listify 1) '(1)))
  (should (equal (listify '(var)) '(var))))

(ert-deftest swap-windows-simple-test ()
  (windows-context
   2                                    ; 2 panes FOO / BAR (focused on FOO)
   #'(lambda ()
       (swap-windows)
       (should (equal (window-buffer-name) '("bar" "foo")))
       (should (eq (intern
                    (buffer-name
                     (window-buffer
                      (selected-window)))) 'bar)))))

;; (ert-deftest the-the-simple-test ()
;;   )

;;; TEST RUNNER
(ert-run-tests-batch-and-exit)

(provide 'test-defs)
