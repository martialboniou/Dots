;;; code.el ---
;;
;; Filename: code.el
;; Description:
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 11:11:10 2011 (+0100)
;; Version: 
;; Last-Updated: Sat Mar 26 18:03:35 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 305
;; URL: 
;; Keywords: 
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: header2 + auto-insert (skeleton) / hideshow + hideshowvis /
;;              cedet & ecb-autoloads / paredit + highlight-parentheses /
;;              eldoc / cheat / simple-call-tree / `lang'
;;
;;
;;; Ideas: CEDET: https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-cedet.el
;;
;;; Notes: cheat needs rubygems' cheat installed
;;         `lang' is the sub-directory for programming languages
;;         bundle packages' configuration and requirements
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
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

(unless (boundp 'mars/local-root-dir) (condition-case nil (load (concat (file-name-directory load-file-name) "vars")) (error "Unable to get custom variables")))

;;; LANGUAGES' CONFIGURATION PATH
(defvar lang-rep (condition-case nil (expand-file-name
                                      (concat
                                       (file-name-directory load-file-name)
                                       "lang"))
                   (error (progn
                            (message "code: unable to locate a lang directory")
                            nil)))
  "Programming languages' configuration repository.")

;;; HEADER2 + AUTO-INSERT
(add-hook 'write-file-functions 'auto-update-file-header)
(add-hook 'emacs-lisp-hook 'auto-make-header)
(add-hook 'c-mode-common-hook 'auto-make-header)
(defun mars/create-header2-if-none (&optional update-me)
  "Creates a header if none. Updates header if UPDATE-ME is T."
  (interactive "P")
  (require 'header2)                    ; for header-max FIXME: find smthg else
  (save-excursion
    (save-restriction
      (narrow-to-region 1 (min header-max (1- (buffer-size))))
      (let ((patterns file-header-update-alist))
        (setq last-command  nil)
        (let (pattern-found stop)
          (while (and (null stop) patterns)
            (goto-char (point-min))
            (when (re-search-forward (car (car patterns)) nil t)
              (goto-char (match-end 0))
              (if update-me
                  (progn
                    (when (null pattern-found)
                      (setq pattern-found t)
                      (message "Header updated"))
                    (funcall (cdr (car patterns))))
                (setq stop t)))
            (setq patterns  (cdr patterns)))
          (unless (or stop pattern-found)
            (when (y-or-n-p "Would you like to make a file header? ")
              (widen)
              (goto-char (point-max))
              (let ((end (point)))      ; remove extra lines
                (backward-word 1)
                (end-of-line)
                (delete-region (point) end))
              (newline 2)               ; add two lines to symmetrize
              (make-header))))))))
(defalias 'mars/create-header-if-none 'mars/create-header2-if-none)

;;; HIDESHOW + HIDESHOWVIS
;; Note: `vimpulse-compatibility' adds:
;; - `zm' to minimize (hs-hide-all)
;; - `zr' to restore  (hs-show-all)
;; if `*i-am-a-vim-user*'
;;
;; globalize hideshow
(require 'hideshow)
(define-globalized-minor-mode global-hs-mode
  hs-minor-mode turn-on-hs-if-desired
  :initialize 'custom-initialize-delay
  :init-value t
  :group 'hideshow
  :version "23.0")
(defun turn-on-hs-if-desired ()
  (when (cond ((eq hs-global-modes t)
               t)
              ((eq (car-safe hs-global-modes) 'not)
               (not (memq major-mode (cdr hs-global-modes))))
              (t (memq major-mode hs-global-modes)))
    (let (inhibit-quit)
      (unless hs-minor-mode
        (hs-minor-mode 1)
        ;; hideshowvis case
        (hideshowvis-enable)))))
(defcustom hs-global-modes nil
  "All modes that need hideshow minor mode activated"
  :type '(choice (const :tag "none" nil)
                 (const :tag "all" t)
                 (set :menu-tag "mode specific" :tag "modes"
                      :value (not)
                      (const :tag "Except" not)
                      (repeat :inline t (symbol :tag "mode"))))
  :group 'hideshow)
(custom-set-variables
 '(hs-global-modes (quote (c-mode
                           objc-mode
                           c++-mode
                           java-mode
                           perl-mode
                           php-mode
                           js-mode ; espresso indeed
                           emacs-lisp-mode
                           lisp-mode
                           scheme-mode
                           qi-mode
                           clojure-mode
                           ruby-mode    ; rinari ?
                           python-mode))))
(global-hs-mode t)
;; ruby case
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))
;; hs-hide-all-comments
(defun hs-hide-all-comments ()
  "Hide all top level blocks, if they are comments, displaying only first line.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (goto-char (point-min))
     (let ((spew (make-progress-reporter "Hiding all comment blocks..."
                                         (point-min) (point-max)))
           (re (concat "\\(" hs-c-start-regexp "\\)")))
       (while (re-search-forward re (point-max) t)
         (if (match-beginning 1)
             ;; found a comment, probably
             (let ((c-reg (hs-inside-comment-p)))
               (when (and c-reg (car c-reg))
                 (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                     (hs-hide-block-at-point t c-reg)
                   (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))
;; toggle hiding even w/o hideshow
(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))
(global-set-key (kbd "C-+") 'toggle-hiding)

;;; CEDET
(eval-after-load "cedet"
  '(progn
     (global-ede-mode 1)                     ; project management
     ;; (semantic-load-enable-minimum-features) ; code helpers
     (if (boundp 'semantic-load-enable-excessive-code-helpers)
         (progn
           (semantic-load-enable-excessive-code-helpers)
           (global-semantic-tag-folding-mode))
       (setq semantic-default-submodes  ; 4 first are recommended
             '(global-semanticdb-minor-mode
               global-semantic-idle-scheduler-mode
               global-semantic-idle-summary-mode
               global-semantic-mru-bookmark-mode
               global-semantic-idle-completions-mode
               global-semantic-decoration-mode
               global-semantic-highlight-func-mode
               global-semantic-stickyfunc-mode)))
     (setq senator-minor-mode-name "SN"
           semantic-imenu-auto-rebuild-directory-indexes nil)
     (global-srecode-minor-mode 1)           ; template insertion menu
     (require 'semantic-decorate-include)
     ;; smart completions
     (require 'semantic-ia)
     (setq-mode-local c-mode semanticdb-find-default-throttle
                      '(project unloaded system recursive))
     (setq-mode-local c++-mode semanticdb-find-default-throttle
                      '(project unloaded system recursive))
     (setq-mode-local erlang-mode semanticdb-find-default-throttle
                      '(project unloaded system recursive))
     ;; gcc
     (require 'semantic-gcc)
     (defun mars/semantic-add-system-include (list symbol)
       (mapc '(lambda (x)
                (when (stringp x)
                  (semantic-add-system-include x symbol)))
             list))
     (unless (null c-include-path)      ; defined in <conf>/vars
       (mars/semantic-add-system-include c-include-path 'c-mode))
     (unless (null cpp-include-path)    ; defined in <conf>/vars
       (mars/semantic-add-system-include c-include-path 'c++-mode))
     (require 'eassist)
     ;; general bindings [semantic-ia]
     (defun alexott/cedet-hook ()
       (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
       (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
       ;;
       (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
       (local-set-key "\C-c=" 'semantic-decoration-include-visit)

       (local-set-key "\C-cj" 'semantic-ia-fast-jump)
       (local-set-key "\C-cq" 'semantic-ia-show-doc)
       (local-set-key "\C-cs" 'semantic-ia-show-summary)
       (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
     (mapc '(lambda (x)
              (add-hook x 'alexott/cedet-hook))
           '(c-mode-common-hook lisp-mode-hook emacs-lisp-mode-hook scheme-mode-hook erlang-mode-hook))
     ;; c-mode bindings [eassist]
     (defun alexott/c-mode-cedet-hook ()
       ;; (local-set-key "." 'semantic-complete-self-insert)
       ;; (local-set-key ">" 'semantic-complete-self-insert)
       (local-set-key "\C-ct" 'eassist-switch-h-cpp)
       (local-set-key "\C-xt" 'eassist-switch-h-cpp)
       (local-set-key "\C-ce" 'eassist-list-methods)
       (local-set-key "\C-c\C-r" 'semantic-symref))
     (add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)
     ;; hook & customs
     (add-hook 'semantic-init-hooks '(lambda () (imenu-add-to-menubar "TAGS"))) ; FIXME: BUG??
     (custom-set-variables
      '(semantic-idle-scheduler-idle-time 3)
      '(semantic-self-insert-show-completion-function (lambda () (semantic-ia-complete-symbol-menu (point))))
      '(global-semantic-tag-folding-mode t nil (semantic-util-modes)))
     ;;     (global-semantic-folding-mode 1) ; FIXME: no more
     (defun mars/Qt ()                  ; TODO: temp var for test => to move to vars-<specific>.el
       (setq qt4-base-dir "/Library/Frameworks/QtCore.framework/Headers")) 
     (defun mars/semantic-tags ()
       ;; GNU global support
       (require 'semanticdb-global)
       (semanticdb-enable-gnu-global-databases 'c-mode)
       (semanticdb-enable-gnu-global-databases 'c++-mode)
       ;; ectags
       (require 'semanticdb-ectag)
       (let ((c-includes '("/opt/local/include" "/usr/local/include"))) ; FIXME: go to vars.el
         (mapc (lambda (x)
                 (semantic-add-system-include x 'c-mode)
                 (semantic-add-system-include x 'c++-mode))
               c-includes)))
     ;; ecb or build autoloads via Makefile
     (condition-case err
         (require 'ecb-autoloads)
       (error (progn
                (message "code: ecb autoloads error: %s" err)
                (let ((ecb-dir (locate-library "ecb")))
                  (if ecb-dir
                      (with-temp-buffer
                        (condition-case errare
                            (progn
                              (cd (file-name-directory ecb-dir))
                              (execvp "make" "autoloads")
                              (require 'ecb-autoloads))
                          (error
                           (message "code: make autoloads error: %s" errare))))
                    (message "code: ecb not found")))))))) ; ecb is out of <site-lisp>/loaddefs

;;; ECB
(defun ecb-activated ()
  (let ((activated nil))
    (when (boundp 'ecb-activated-window-configuration)
      (setq activated ecb-activated-window-configuration))
    activated))
(eval-after-load "ecb"
  '(progn
     (setq ecb-tip-of-the-day nil)
     (push '(ecb-minor-mode nil) desktop-minor-mode-table) ; compatibility with DESKTOP
     ;; ECB version of mars/toggle-single-window defined in <confs>/box.el
     (if (fboundp 'mars/toggle-single-window)
         (progn
           (defvar mars/ecb-previously-running nil
             "Previous state of ECB.")
           (defadvice mars/toggle-single-window (around ecb-active activate)
             (interactive)
             (if (cdr (window-list nil 0))
                 (if (ecb-activated)
                     (when (y-or-n-p "ECB is running. Do you want to deactivate ECB? ")
                       (ecb-deactivate)
                       (setq mars/ecb-previously-running t)
                       (delete-other-windows))
                   (progn
                     (setq *mars/previous-window-configuration* (current-window-configuration))
                     (setq mars/ecb-previously-running nil)
                     (delete-other-windows)))
               (if mars/ecb-previously-running
                   (progn
                     (ecb-activate)
                     (setq mars/ecb-previously-running nil))
                 (when *mars/previous-window-configuration*
                   (set-window-configuration *mars/previous-window-configuration*)
                   (setq *mars/ecb-in-previous-window-configuration* nil)))))))
     ;; ECB version of mars-windows-archiver-save
     (defadvice mars-windows-archiver-save (around ecb-active activate)
       (interactive)
       (if (ecb-activated)
           (when (y-or-n-p "BEWARE: you should deactivate ecb first. Archive the current window configuration anyway? ")
             (let ((dont-alert t))
               ad-do-it))
         (progn
           ad-do-it)))
     ;; ECB version of kiwon/save-window-configuration
     (defadvice kiwon/save-window-configuration (around ecb-active (&optional ecb-manage) activate)
       (let ((ecb-active (ecb-activated)))
         (when (and ecb-manage ecb-active) ; you cannot deactivate ecb when desktop is autosaved so
           (ecb-deactivate))               ; `ecb-manage' is here for the `kill-emacs-hook' case
         (progn
           ad-do-it
           (when ecb-active
             (append-to-file "(ecb-activate)" nil kiwon/last-window-configuration-file)))))
     (add-hook 'kill-emacs-hook '(lambda ()
                                   (kiwon/save-window-configuration t)) 'append)))
(defun mars/toggle-ecb ()
  (interactive)
  (if (ecb-activated)
      (when (y-or-n-p "Stop ecb? ")
        (ecb-deactivate))
    (when (y-or-n-p "Start ecb? ")
      (ecb-activate))))

;; PAREDIT + HIGHLIGHT-PARENTHESES
(mapc '(lambda (x)
         (add-hook x
                   (lambda ()
                     (paredit-mode +1)
                     (highlight-parentheses-mode t))))
      '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook
                             clojure-mode-hook scheme-mode-hook qi-mode-hook
                             slime-repl-mode-hook inferior-lisp-mode-hook inferior-qi-mode-hook))
(eval-after-load "paredit"
  '(progn
     (when (assoc 'paredit-mode minor-mode-alist)
       (setcdr (assoc 'paredit-mode minor-mode-alist) '(" PE"))) ; `PE' in mode-line (short name for `Paredit')
     ;; IMPORTANT: free arrow bindings (`windmove' may use them)
     (define-key paredit-mode-map (kbd "C-<left>")    nil) ; use C-S-) instead
     (define-key paredit-mode-map (kbd "C-<right>")   nil)
     (define-key paredit-mode-map (kbd "M-<left>")    nil)
     (define-key paredit-mode-map (kbd "M-<right>")   nil)
     (define-key paredit-mode-map (kbd "C-M-<left>")  nil)
     (define-key paredit-mode-map (kbd "C-M-<right>") nil)
     ;; slime case
     (defun override-slime-repl-bindings-with-paredit ()
       (define-key slime-repl-mode-map
         (read-kbd-macro paredit-backward-delete-key) nil))
     (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
     ;; vimpulse case
     ;; (when vimpulse-version
     ;;   (require 'paredit-viper-compat)
     ;;   (paredit-viper-compat))
     ;; (define-key paredit-mode-map [?\)] 'paredit-close-parenthesis)
     ;; (define-key paredit-mode-map [(meta ?\))]
     ;;   'paredit-close-parenthesis-and-newline)
     ))
(eval-after-load "highlight-parentheses"
  '(progn
     (when (assoc 'highlight-parentheses-mode minor-mode-alist)
       (setcdr (assoc 'highlight-parentheses-mode minor-mode-alist) '(""))) ; IMPORTANT: don't display name in mode-line
     (setq autopair-handle-action-fns
           (list 'autopair-default-handle-action
                 '(lambda (action pair pos-before)
                    (hl-paren-color-update))))))

;; ELDOC
(mapc '(lambda (x)
         (add-hook x' turn-on-eldoc-mode)) ; document emacs-lisp code
      '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))
;; (require 'c-eldoc)
;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; CHEAT
(eval-after-load "anything"
  '(progn
     (when (executable-find "cheat")    ; gem install cheat
       (defvar anything-c-source-cheat
         '((name . "Cheat Sheets")
           (init . (lambda ()
                     (unless (anything-candidate-buffer)
                       (with-current-buffer (anything-candidate-buffer 'global)
                         (call-process-shell-command
                          "cheat sheets" nil  (current-buffer))
                         (goto-char (point-min))
                         (forward-line 1)
                         (delete-region (point-min) (point))
                         (indent-region (point) (point-max) -2)))))
           (candidates-in-buffer)
           (action . (lambda (entry)
                       (let ((buf (format "*cheat sheet:%s*" entry)))
                         (unless (get-buffer buf)
                           (call-process "cheat" nil (get-buffer-create buf) t entry))
                         (display-buffer buf)
                         (set-window-start (get-buffer-window buf) 1))))))
       (defun anything-cheat ()
         "Preconfigured `anything' for cheat sheets."
         (interactive)
         (anything-other-buffer 'anything-c-source-cheat "*Anything cheat*")))))

;;; VISUALIZE CALL TREE
(defun simple-call-tree-list-functions-and-callers ()
  "List functions and callers in `simple-call-tree-alist'."
  (interactive)
  (let ((list (simple-call-tree-invert simple-call-tree-alist)))
    (switch-to-buffer (get-buffer-create "*simple-call-tree*"))
    (erase-buffer)
    (dolist (entry list)
      (let ((callers (mapconcat #'identity (cdr entry) ", ")))
        (insert (car entry) " is called by "
                (if (string= callers "")
                    "no functions."
                  callers)
                ".\n")))))
(defun simple-call-tree-list-callers-and-functions ()
  "List callers and functions in `simple-call-tree-alist'."
  (interactive)
  (let ((list simple-call-tree-alist))
    (switch-to-buffer (get-buffer-create "*simple-call-tree*"))
    (erase-buffer)
    (dolist (entry list)
      (let ((functions (mapconcat #'identity (cdr entry) ", ")))
        (insert (car entry) " calls "
                (if (string= functions "")
                    "no functions"
                  functions)
                ".\n")))))
;; sct-dot
(defun sct-dot ()
  "Generate dot file for graphviz from `simple-call-tree-alist'.
After calling `simple-call-tree-analyze', use `sct-dot' in an
empty buffer via `(insert (sct-dot))'.
Then save the file as \"my-file.dot\" and run
\"dot -Tjpg /path/to/my-file.dot -o result.jpg\" from command line."
  (concat "digraph G {\n" ;; default beginning of a dot file
          (mapconcat 'identity ;; end each line with a ";"
                     (mapcar
                      (lambda (defun-list)
                        "Called for each elemet (list) of `simple-call-tree-alist',
                         create all the 'caller -> callee;' strings."
                        (let ((caller (car defun-list))
                              (callees (cdr defun-list)))
                          (if (null callees)
                              (concat "\"" caller "\"")
                            (mapconcat (lambda (callee)
                                         "Called with each callee, create 'caller -> callee' pairs."
                                         (concat "\"" caller "\"" " -> " "\"" callee "\""))
                                       callees
                                       ";\n"))))
                      simple-call-tree-alist)
                     ";\n")
          ";\n}"))
;; sct-graphviz (iff graphviz)
(let ((graphviz-command (executable-find "dot")))
  (when graphviz-command
    (defvar sct-graphviz-dir (expand-file-name
                              (concat
                               (file-name-as-directory mars/local-root-dir)
                               (file-name-as-directory mars/personal-data)
                               (file-name-as-directory "Temporary")
                               (file-name-as-directory "Graphviz"))))
    (when (not (file-exists-p sct-graphviz-dir))
      (if (y-or-n-p (format "code: Would you like to create a default directory to extend `simple-call-tree' function with visual capability? "))
          (make-directory sct-graphviz-dir t)
        (setq sct-graphviz-dir
              (expand-file-name (file-name-as-directory (read-directory-name "Directory for simple call tree generated image: " nil nil t))))))
    (defun sct-graphviz ()
      (interactive)
      (let ((tmp-file (make-temp-file (expand-file-name ".tmp" sct-graphviz-dir) nil ".dot"))
            (viz-file (expand-file-name (concat sct-graphviz-dir (buffer-name (current-buffer)) ".png"))))
        (with-temp-file tmp-file
          (insert (sct-dot)))
        (let ((cmd-return (execvp "dot" "-Tpng" tmp-file "-o" viz-file)))
          (if (zerop (length cmd-return))
              (let ((vct (get-buffer-create "*Visual Call Tree*")))
                (with-current-buffer
                    vct
                  (insert-image viz-file))
                (display-buffer vct))
            (error "graphivz: error during external command: %s" cmd-return)))
        (delete-file tmp-file)))))
;;(car-safe "/Users/mars/.emacs.d/data/Temporary/Graphviz/code.el.png")

;;; LANGUAGES
(when lang-rep
  (add-to-list 'load-path lang-rep)
  (require 'pure-object)                ; for smalltalk / factor / io
  (require 'wiki-wiki)                  ; for markup languages as mediawiki or markdown
  ;; (require 'church-inspired)            ; for lisp (including scheme)
  (require 'peyton-jones-family)        ; for ML family and QiII
  (require 'web-programming)            ; for web languages (nxhtml/espresso)
  ;; (require 'python-357)                 ; for python
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code.el ends here
