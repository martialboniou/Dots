;;; python-357.el --- 
;; 
;; Filename: python-357.el
;; Description: Python
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sun Mar  6 23:42:52 2011 (+0100)
;; Version: 
;; Last-Updated: Mon Oct 24 10:40:54 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 54
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: python-mode + pylookup [ + Pymacs (+ Rope) + pyflakes ]
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;;; PYTHON-MODE
;; WARNING: this mode has *no* python-mode-map / you must use hook for
;; key bindings too (py-mode-map is for python.el provided by GNU Emacs)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(eval-after-load "python-mode"
  '(progn
     (custom-set-variables
      '(py-indent-offset 4)            ; PEP 8
      '(indent-tabs-mode nil))
     (define-key py-mode-map (kbd "RET") 'newline-and-indent)
     (when (executable-find "ipython")
       (require 'ipython))
     (eval-after-load "ipython"
       '(progn
          (use-anything-show-completion 'anything-ipython-complete
                                        '(length initial-pattern))))))
(add-hook 'python-mode-hook
          #'(lambda ()
              (smart-operator-mode-on)  ; `pretty-lambda' in `confs/formats'
              ;; NOTE: autopairs' enhancements in `confs/code'
              ))

;;; PYLOOKUP
(setq pylookup-dir
      (condition-case nil
          (file-name-directory (locate-library "pylookup"))
        (error nil)))
(unless (null pylookup-dir)
  (let ((py-py (concat pylookup-dir "pylookup.py"))
        (py-db (concat pylookup-dir "pylookup.db")))
    (setq pylookup-program
           (and (file-exists-p py-py) py-py))
    (setq pylookup-db-file
          (and (file-exists-p py-db) py-db))))
;; <C-c h> to display python lookup in python-mode buffer
(add-hook 'python-mode-hook #'(lambda ()
                                (local-set-key "\C-ch" 'pylookup-lookup)))

;; PYMACS + ROPE TODO: eval-after-load'ing correctly
;; (eval-after-load "python-mode"
;;   '(progn
;;      (pymacs-load "ropemacs" "rope-")
;;      (setq ropemacs-enable-autoimport t)
;;      ;; yasnippet hack
;;      (eval-after-load "yasnippet"
;;        '(progn
;;           (defun prefix-list-elements (list prefix)
;;             (let (value)
;;               (nreverse
;;                (dolist (element list value)
;;                  (setq value (cons (format "%s%s" prefix element) value))))))
;;           (defvar ac-source-rope
;;             '((candidates
;;                . (lambda ()
;;                    (prefix-list-elements (rope-completions) ac-target))))
;;             "Source for Rope")
;;           (defun ac-python-find ()
;;             "Python `ac-find-function'."
;;             (require 'thingatpt)
;;             (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
;;               (if (null symbol)
;;                   (if (string= "." (buffer-substring (- (point) 1) (point)))
;;                       (point) nil) symbol)))
;;           (defun ac-python-candidate ()
;;             "Python `ac-candidates-function'."
;;             (let (candidates)
;;               (dolist (source ac-sources)
;;                 (if (symbolp source)
;;                     (setq source (symbol-value source)))
;;                 (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
;;                        (requires (cdr-safe (assq 'requires source)))
;;                        cand)
;;                   (if (or (null requires)
;;                           (>= (length ac-target) requires))
;;                       (setq cand
;;                             (delq nil
;;                                   (mapcar (lambda (candidate)
;;                                             (propertize candidate 'source source))
;;                                           (funcall (cdr (assq 'candidates source)))))))
;;                   (if (and (> ac-limit 1)
;;                            (> (length cand) ac-limit))
;;                       (setcdr (nthcdr (1- ac-limit) cand) nil))
;;                   (setq candidates (append candidates cand))))
;;               (delete-dups candidates)))
;;           (add-hook 'python-mode-hook
;;                     (lambda ()
;;                       (auto-complete-mode 1)
;;                       (set (make-local-variable 'ac-sources)
;;                            (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
;;                       (set (make-local-variable 'ac-find-function) 'ac-python-find)
;;                       (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
;;                       (set (make-local-variable 'ac-auto-start) nil)))
;;           ;;Ryan's python specific tab completion
;;           (defun ryan-python-tab ()
;;             (interactive)
;;             (if (eql (ac-start) 0)
;;                 (indent-for-tab-command)))
;;           (defadvice ac-start (before advice-turn-on-auto-start activate)
;;             (set (make-local-variable 'ac-auto-start) t))
;;           (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;;             (set (make-local-variable 'ac-auto-start) nil))
;;           (define-key py-mode-map "\t" 'ryan-python-tab)

;;           )) ))

;; ;; FLYMAKE
;; (add-hook 'python-mode-hook '(lambda () (flymake-mode-on)))
;; (eval-after-load "flymake"
;;   (when (load "flymake" t)
;;     (defun flymake-pyflakes-init ()
;;       (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                          'flymake-create-temp-inplace))
;;              (local-file (file-relative-name
;;                           temp-file
;;                           (file-name-directory buffer-file-name))))
;;         (list "pyflakes" (list local-file))))
;;     (add-to-list 'flymake-allowed-file-name-masks
;;                  '("\\.py\\'" flymake-pyflakes-init))))

(provide 'python-357)

;;; MAINTENANCE
;; (executable-find "hg")
;; mkdir /tmp/rope && cd /tmp/rope
;; hg clone http://bitbucket.org/agr/rope
;; hg clone http://bitbucket.org/agr/ropemacs
;; hg clone http://bitbucket.org/agr/ropemode
;; sudo easy_install rope
;; ln -s ../ropemode/ropemode ropemacs/
;; sudo easy_install ropemacs
;; (executable-find "python") to compile Pymacs with 'python setup.py install'
;; in (directory-file-name (locate-library "pymacs"))
;; (executable-find "easy_install") to compile and install rope in a convenient python path
;;+ pyflakes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-357.el ends here

