;;; defs.el ---
;;
;; Filename: defs.el
;; Description: utility functions
;; Author: Martial Boniou
;; Maintainer:
;; Created: Sat Feb 19 18:12:37 2011 (+0100)
;; Version: 0.9.2
;; Last-Updated: Tue Oct 11 16:07:14 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 53
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: Most useful functions (should be loaded at the
;;              beginning of .emacs)
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

;;; The Most Useful Interactive Funcs:
;;;
;;; /// TEXT \\\
;;; M-x show-trailing-whitespace : toggle spacing appearance
;;; M-x dired-2unix-marked-files : repair dired files to Unix encoding
;;; M-x the-the : remove duplicata words in the current file
;;; M-x insert-special : insert special characters
;;; /// FILE/BUFFER \\\
;;; M-x rename-file-and-buffer : rename file/buffer
;;; M-x move-buffer-file : move file/buffer to another directory
;;;
;;; Changelog: 2010-04-22: introduce execvp function

(unless (boundp 'mars/local-root-dir) (condition-case nil (load (concat (file-name-directory load-file-name) "vars")) (error "Unable to get custom variables")))

;;; INITIALIZATIONS
;;
(defvar *emacs/init-path* nil
  "Lisp files to compile.")
;;(defvar calc-command-flags)

(unless (boundp 'mars/eternal-buffer-list)
    (setq mars/eternal-buffer-list '("*scratch*")))
;;(if (and (boundp 'mars/local-root-dir) (boundp 'mars/local-conf-path))
(setq *emacs/init-path* (cond ((and (boundp 'mars/local-root-dir)
                    (boundp 'mars/local-conf-path))
                   (mapcar '(lambda (x) (concat (file-name-as-directory mars/local-root-dir) x))
                       mars/local-conf-path))
                  (t "~/.emacs.d/confs")))

;;; ESSENTIAL UTILITIES
;;;

(defmacro mars/add-hook (hk &rest funs)
  "Create hooker (mars/add-hook my-hk my-fun1 my-fun2)"
  `(progn
     ,@(mapcar (lambda (arg)
                 `(add-hook (quote ,hk) (function ,arg)))
               funs)))

(defmacro mars/add-hook-from-list (hk fun) ;; FIXME: should a add-hook too
  "Create hooker (mars/add-hook-to-list '(hk1 hk2 hk3) fun)"
  `(progn
     ,@(mapcar (lambda (arg)
                 `(add-hook (quote ,arg) (function ,fun)))
               (eval hk))))

(defun mars/generate-mode-hook-list (list)
  "Create a quoted list of hooks"
  (mapcar (lambda (arg)
            (make-symbol (concat (symbol-name arg) "-mode-hook")))
          list))

(defun dont-kill-emacs ()
  "Disallow emacs to kill on the dangerous C-x C-c command"
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(defun byte-compile-user-init-file ()
  "Compiles .emacs."
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (let ((byte-init-file (concat user-init-file ".elc")))
      (when (file-exists-p byte-init-file)
        (delete-file byte-init-file))
      (byte-compile-file user-init-file))))

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
the minibuffer. Else, if mark is active, indents region. Else if
point is at the end of a symbol, expands it. Else indents the
current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (hippie-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (hippie-expand nil)
        (indent-for-tab-command)))))

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
      (indent-according-to-mode)))

(defun name-conf-file (name)            ; OBSOLETE
  "Generates a complete name for a configuration file according to the `Emacs' version."
  (let ((root-filename (concat "~/.emacs-" name))
        (subdir        "~/.emacs.d/")
        (x-subdir      "~/.xemacs/"))
    (cond
     ((file-exists-p root-filename) root-filename)
     ((and (not (featurep 'xemacs))
           (file-directory-p (if (boundp 'user-emacs-directory)
                                 user-emacs-directory
                               subdir)))
      (concat (if (boundp 'user-emacs-directory)
                  user-emacs-directory
                subdir) name))
     ((and (featurep 'xemacs) (file-directory-p x-subdir))
      (concat x-subdir name))
     (t root-filename))))

(defmacro defun-dummy (configuration-case &rest source-funs)
  "Creates dummy functions if not bound and associates a possible
file to load it. Useful for keybindings referring to functions without
autoloads (generally non site-lisp files like `configuration' files
aren't autoloaded). `fun-source' is a suite of alists where CAR is a
source file and CDR is a function name or a list of function name.
If `configuration-case' is T, try to load the file as a configuration
file; display a message otherwise."
  (if configuration-case                ; FIXME: refactor it
      `(progn
         ,@(mapcar '(lambda (x)
                      (setq z (listify (cdr x)))
                      `(progn
                         ,@(mapcar '(lambda (y)
                                      `(unless (fboundp ',y) (defun ,y () (interactive) (when (y-or-n-p ,(concat (symbol-name y) ": function missing. Load the configuration file `" (car x) "'? ")) (conf-load ,(car x)))))) z)))
                   source-funs))
    `(progn
       ,@(mapcar '(lambda (x)
                    `(unless (fboundp ',x) (defun ,x () (interactive) (message "%s: function missing." ,(symbol-name x)))))
                source-funs))))

(defun crazycode/indent-and-complete ()
  "Indent line and Complete if point is at end of left a leave word."
  (interactive)
  (cond
   ;; hippie-expand
   ((looking-at "\\_>")
    ;; skip message output
    (flet ((message (format-string &rest args) nil))
      (hippie-expand nil))))
  ;; always indent line
  (indent-for-tab-command)) ; for example, for Ruby indent issues

(defun my-tab-expansion-switcher ()
  (local-set-key [tab] 'indent-or-expand))

(defun mars/kill-this-buffer ()
    "Kill the current buffer now"
    (interactive)
    ;; issue with 'frame-live and eternal buffers managed
    (if (member (buffer-name (current-buffer)) mars/eternal-buffer-list)
        (bury-buffer)
        (kill-buffer (current-buffer))))

(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  ;; deprecated 'save-excursion+'set-buffer => 'with-current-buffer
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))

(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;;; 'previous-line invokes a new line when we reached the top of the buffer
(defadvice previous-line (before next-line-at-end)
  "Insert an empty line when moving up from the top line."
  (when (and next-line-add-newlines (= arg 1)
             (save-excursion (beginning-of-line) (bobp)))
    (beginning-of-line)
    (newline)))
(ad-activate 'previous-line)

;;; NOTIFIERS
(defun display-external-pop-up (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played.
   libnotifyd version: djcb@http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html"
  (interactive)
  (if (eq system-type 'darwin)
       (shell-command
        (concat "growlnotify -a Emacs -t '" title "' -m '" msg "' 2> /dev/null"))
    (progn
      (when sound (shell-command
                   (concat "mplayer -really-quiet " sound " 2> /dev/null")))
      (if (eq window-system 'x)
          (shell-command (concat "notify-send "
                                 (if icon (concat "-i " icon) "")
                                 " '" title "' '" msg "'"))
        (message (concat title ": " msg))))))
(defun output-to-growl (msg)
  (let ((fname (make-temp-file "/tmp/growlXXXXXX")))
    (with-temp-file fname
      (let ((coding-system-for-write 'utf-16))
        (insert (format "tell application \"GrowlHelperApp\" notify with name \"Emacs\" title \"Emacs alert\" description «data utxt%s» as Unicode text application name \"Emacs\" end tell"
                        (osd-text-to-utf-16-hex msg))))
      (shell-command (format "osascript %s" fname)))
    (delete-file fname)))
(defun osd-text-to-utf-16-hex (text)
  (let* ((utext (encode-coding-string text 'utf-16))
         (ltext (string-to-list utext)))
    (apply #'concat
           (mapcar (lambda (x) (format "%02x" x)) ltext))))
;; (output-to-growl "toto")

;;; MISCELLANEOUS UTILITIES
;;;

(defun listify (l)
  (if (listp l) l (list l)))

(defun make-file-executable-if-script ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (let* ((current-mode (file-modes (buffer-file-name)))
              (add-mode (logand ?\111 (default-file-modes))))
         (or (/= (logand ?\111 current-mode) 0)
             (zerop add-mode)
             (set-file-modes (buffer-file-name)
                             (logior current-mode add-mode))))))

(defun byte-compile-emacs-config ()     ; FIXME: should work for confs
  "Byte compile the current file, when saved, if the file is part of
the personal Emacs Lisp configuration directory."
  (let ((current-file (buffer-file-name)))
    (let ((config-path (listify mars/local-conf-path)))
      (dolist (config-dir
               (mapcar '(lambda (x)
                          (concat
                           (file-name-as-directory mars/local-root-dir)
                           x)) config-path))
        (let ((string-length (length config-dir)))
          (when (and (eq (compare-strings config-dir 0 string-length
                                          current-file 0 string-length) 1)
                     (string-match "\\.el\\'" current-file))
            (byte-compile-file current-file)))))))

(defun byte-recompile-home ()
  (interactive)
  (let ((path (listify mars/local-conf-path)))
    (mapcar '(lambda (x) (progn
                           (message (prin1-to-string path))
                           (byte-recompile-directory
                            (concat
                             (file-name-as-directory mars/local-root-dir) x))))
            path)))

(defun swap-windows ()
 "If you have 2 windows, it swaps them."
 (interactive)
 (cond ((not (= (count-windows) 2))
        (message "You need exactly 2 windows to do this."))
       (t
        (let* ((w1 (first (window-list)))
               (w2 (second (window-list)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file name new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
          (filename (buffer-file-name))
          (dir
             (if (string-match dir "\\(?:/\\|\\\\)$")
                       (substring dir 0 -1) dir))
          (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

(defun the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
      (message "End of buffer")))

(unless (fboundp 'trim-string)		; defined in `confs/packs' TODO: merge
  (defun trim-string (string)
    "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10). --xah"
    (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))))

(defun to-unix-eol (fpath)
  "Change file's line ending to unix convention."
  (let (mybuffer)
    (setq mybuffer (find-file fpath))
    (set-buffer-file-coding-system 'unix) ; or 'mac or 'dos
    (save-buffer)
    (kill-buffer mybuffer)))

(defun execvp (&rest args)
  "Simulate C's execvp() function.
Quote each argument separately, join with spaces and call shell-command-to-string to run in a shell."
  (let ((cmd (mapconcat 'shell-quote-argument args " ")))
    (shell-command-to-string cmd)))

(defun dired-2unix-marked-files ()
  "Change to unix line ending for marked (or next arg) files."
  (interactive)
  (mapc 'to-unix-eol (dired-get-marked-files)))

(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t)
          pid)))))

(defun elisp-files-in-below-directory (directory)
  "Fetch all emacs lisp files in `directory' and all its subdirectories.
Known as FILES-IN-BELOW-DIRECTORY seen in `http://www.rattlesnake.com/intro/Files-List.html'."
  (interactive "DDirectory name: ")
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    (while current-directory-list
      (cond
       ((equal ".el" (substring (car (car current-directory-list)) -3))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       ((eq t (car (cdr (car current-directory-list))))
        (unless
            (equal "."
                   (substring (car (car current-directory-list)) -1))
          (setq el-files-list
                (append
                 (elisp-files-in-below-directory
                  (car (car current-directory-list)))
                 el-files-list)))))
      (setq current-directory-list (cdr current-directory-list)))
    el-files-list))

(defun start-named-server (name)
  "Start a server named 'name' - ensure only one server of that
`NAME' is running"
  (interactive "sServer Name: ")
  (require 'server)
  (setq server-name name)
  (setq mk-server-socket-file (concat server-socket-dir "/" name))
  (unless (file-exists-p mk-server-socket-file)
    (server-start)
    (add-hook 'kill-emacs-hook
              (lambda ()
                (when (file-exists-p mk-server-socket-file)
                  (delete-file mk-server-socket-file))))))

(defun add-hook-once (hook function &optional append local)
  "Same as `add-hook', but FUN is only run once.
   Also contrary to `add-hook', this is not idempotent."
  ;; FIXME: need to check if `function' was already added to the hook.
  (let ((code (list 'lambda)))
    (setcdr code `(() (,function) (remove-hook ',hook ',code ',local)))
    (add-hook hook code append local))) ; XEmacs compatible hook management

;;; find-file with automake directory DEPRECATED (b/c ido M-m)
;; (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
;;   "Create parent directory if not exists while visiting file."
;;   (unless (file-exists-p filename)
;;     (let ((dir (file-name-directory filename)))
;;       (unless (file-exists-p dir)
;;         (make-directory dir)))))

;;; Three functions using mars/title-as-markdown-title
(defun mars/markdown-header-first ()
  (interactive)
  (mars/title-as-markdown-title 61))    ; (string-to-char "=")

(defun mars/markdown-header-second ()
  (interactive)
  (mars/title-as-markdown-title 45))    ; (string-to-char "-")

(defun mars/underline-with-char (char)
  (interactive (list (read-from-minibuffer "Char: ")))
  (when (= 0 (length char))
    (error "Need a character"))
  (setq char (aref char 0))
  (mars/title-as-markdown-title char))

;;; create a markdown title and remove trailing space at the end
;;; [hondana@gmx.com: the same on vim/shortkeys -- Vi bindings are
;;; __+ ('+' is shifted equal) for '=' first level header and ___
;;; ('_' is shifted minus) for '-' second level header]
(defun mars/title-as-markdown-title (char)
  (save-excursion
    (goto-char (point-at-eol))
    (backward-delete-char-hungry 1)       ; delete trailing space between the eol and the last word
    (insert "\n"
            (make-string (- (point-at-eol)
                            (point-at-bol))
                         char)))
  (forward-line)                           ; go to our new underline like on Vim (use vimpulse's 'o' or emacsen's 'Ctrl-e Enter' to open new line below)
)

;;; delete trailing whitespace backward and forward
(defun backward-delete-char-hungry (arg &optional killp)
      "*Delete characters backward in \"hungry\" mode.
    See the documentation of `backward-delete-char-untabify' and `backward-delete-char-untabify-method' for details."
      (interactive "*p\nP")
      (let ((backward-delete-char-untabify-method 'hungry))
        (backward-delete-char-untabify arg killp)))

(defun delete-horizontal-space-forward () ; adapted from `delete-horizontal-space'
      "*Delete all spaces and tabs after point."
      (interactive "*")
      (delete-region (point) (progn (skip-chars-forward " \t") (point))))

;;; whack whitespace until the next word
(defun whack-whitespace (arg)
      "Delete all white space from point to the next word.  With prefix ARG delete across newlines as well.  The only danger in this is that you don't have to actually be at the end of a word to make it work.  It skips over to the next whitespace and then whacks it all to the next word."
      (interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
        (re-search-forward regexp nil t)
        (replace-match "" nil nil)))

;;; no line wrap this buffer (for special buffer like MIME-VIEW)
(defun no-line-wrap-this-buffer-internal ()
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows nil)
  (setq truncate-lines nil))
(defun no-line-wrap-this-buffer ()
  (lexical-let ((buf (current-buffer)))
    (add-hook-once                      ; defined here
     'post-command-hook
     (lambda ()
       (when (buffer-name buf)          ; avoid bad usage
           (with-current-buffer buf
             (no-line-wrap-this-buffer-internal)))))))

;;; fix amazon URL
(defun fix-amazon-url ()
  "Minimizes the Amazon URL under the point.  You can paste an Amazon
URL out of your browser, put the cursor in it somewhere, and invoke
this method to convert it."
  (interactive)
  (and (search-backward "http://www.amazon.com" (point-at-bol) t)
       (search-forward-regexp
        ".+/\\([A-Z0-9]\\{10\\}\\)/[^[:space:]\"]+" (point-at-eol) t)
       (replace-match
        (concat "http://www.amazon.com/o/asin/"
                (match-string 1)
                (match-string 3)))))

(defun fix-google-search-url ()
  "Minimizes a Google search URL under the point."
  (interactive)
  (and (search-backward-regexp "http://www\\.google\\.[a-z]\\{2,3\\}/search" (point-at-bol) t)
       (search-forward-regexp
        ".+[&?]\\(q=[a-zA-Z0-9%+]+\\)\\(&.+\\)*" (point-at-eol) t)
       (replace-match
        (concat "http://www.google.com/search?"
                (match-string 1)))))

(defun compile-adjust-variable ()
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (concat "gcc -O2 -Wall -o " (file-name-sans-extension file)
                   " " file)))))

(defmacro define-hash-region (name hash-type)
  `(defun ,name (start end)
     (interactive "r")
     (save-excursion
       (let ((s (,hash-type (buffer-substring start end))))
         (delete-region start end)
         (insert s)))))

;; (define-hash-region sha1-region sha1) ; deprecated: in sha1-el
;; (define-hash-region md5-region md5)

;; From http://www.tbray.org/ongoing/When/200x/2003/09/27/UniEmacs
(defun one-quote () "" (interactive) (insert ?'))
(defvar sq-state 'nil "In single-quotes?")
(defvar dq-state 'nil "In double quotes?")
(defun insert-special (c)
  "Insert special characters, like so:
   s => open/close single quotes
   d => open/close double quotes
   ' => apostrophe
   a => <a href=
   i => <img src=
   & => &amp;
   < => &lt;
   - => mdash
   . => center-dot"
  (interactive "cInsert special (s d ' a i & < - .)")
  (cond
   ((= c ?s)
    (if sq-state
    (progn
      (ucs-insert #x2019)
      (setq sq-state 'nil))
      (ucs-insert #x2018)
      (setq sq-state 't)))
   ((= c ?d)
    (if dq-state
    (progn
      (ucs-insert #x201d)
      (setq dq-state 'nil))
      (ucs-insert #x201c)
      (setq dq-state 't)))
   ((= c ?') (ucs-insert #x2019))
   ((= c ?a)
    (progn
      (if (> (current-column) 0) (newline-and-indent))
      (insert "<a href=\"\">")
      (backward-char 2)))
   ((= c ?i)
    (progn
      (if (> (current-column) 0) (newline-and-indent))
      (insert "<img src=\"\" alt=\"\" />")
      (backward-char 11)))
   ((= c ?&) (insert "&amp;"))
   ((= c ?<) (insert "&lt;"))
   ((= c ?-) (ucs-insert #x2014))
   ((= c ?.) (ucs-insert #xb7))))

(defun fix-and-indent ()
  "Clean up the code"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; scroll one line at a time
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))
(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive "p")
  (scroll-down (or arg 1)))

(defun fix-code-region (from to)
  "Indent by 4 spaces the selected code region for blog."
  (interactive
   (list (region-beginning) (region-end)))
  (goto-char from)
  (while (< (point) to)
    (beginning-of-line)
    (indent-to-column 4)
    (forward-line))
  (indent-to-column 4))

;; From http://sami.samhuri.net/2007/6/23/emacs-for-textmate-junkies
(defun wrap-region (left right beg end)
  "Wrap the region in arbitrary text, LEFT goes to the left and
RIGHT goes to the right."
  (save-excursion
    (goto-char beg)
    (insert left)
    (goto-char (+ end (length left)))
    (insert right)))

(defun wrap-region-with-tag (tag beg end)
  "Wrap the region in the given HTML/XML tag using `wrap-region'. If any
attributes are specified then they are only included in the opening tag."
  (interactive "*sTag (including attributes): \nr")
  (let* ((elems (split-string tag " "))
         (tag-name (car elems))
         (right (concat "</" tag-name ">")))
    (if (= 1 (length elems))
        (wrap-region (concat "<" tag-name ">") right beg end)
      (wrap-region (concat "<" tag ">") right beg end))))

(defun wrap-region-with-tag-or-insert ()
  (interactive)
  (if (and mark-active transient-mark-mode)
      (call-interactively 'wrap-region-with-tag)
    (insert "<")))

(defun wrap-region-or-insert (left right)
  "Wrap the region with `wrap-region' if an active region is
marked, otherwise insert LEFT at point."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (wrap-region left right (region-beginning) (region-end))
    (insert left)))

(defmacro wrap-region-with (left &optional right)
  "Returns a function which, when called, will interactively
wrap-region-or-insert using left and right."
  (if right
      `(lambda ()
         (interactive)
         (wrap-region-or-insert ,left ,right))
      `(lambda ()
         (interactive)
         (wrap-region-or-insert ,left ,left))))

;;;======================================================================
;;; From: Jim Janney <jjanney@xmission.xmission.com>
;;; in comp.emacs
;;; show and hide comments in program code.
;; TODO: test it
(defun overlay-comments(beg end attrs)
  (save-excursion
    (goto-char beg)
    (let (state comment-start comment-end overlay)
      (while (nth 4 (setq state
                          (parse-partial-sexp (point) end nil nil nil t)))
        (goto-char (nth 8 state))
        (setq comment-start (point))
        (forward-comment 1)
        (setq comment-end (point))
        (while (= (char-before comment-end) ?\n)
          (setq comment-end (1- comment-end)))
        (setq overlay (make-overlay comment-start comment-end))
        (mapc #'(lambda (attr)
                (overlay-put overlay (car attr) (cdr attr)))
              attrs)))))

(defun hide-comments()
  (interactive)
  (overlay-comments (point-min)
                    (point-max)
                    '((category . comment) (invisible . comment))))

(defun show-comments()
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (if (eq (overlay-get ov 'category) 'comment)
        (delete-overlay ov))))

;;; OTHER TIPS
;;;

;; http://homepages.inf.ed.ac.uk/s0243221/emacs/ - delete current line a la vi with `dd'
(defun nuke-line ()
  "Kill an entire line, including the trailing newline character"
  (interactive)
  ;; Store the current column position, so it can later be restored for a more
  ;; natural feel to the deletion
  (let ((previous-column (current-column)))
    ;; Now move to the end of the current line
    (end-of-line)
    ;; Test the length of the line. If it is 0, there is no need for a
    ;; kill-line. All that happens in this case is that the new-line character
    ;; is deleted.
    (if (= (current-column) 0)
        (delete-char 1)
        ;; This is the 'else' clause. The current line being deleted is not zero
        ;; in length. First remove the line by moving to its start and then
        ;; killing, followed by deletion of the newline character, and then
        ;; finally restoration of the column position.
        (progn
          (beginning-of-line)
          (kill-line)
          (delete-char 1)
          (move-to-column previous-column)))))

(defun remove-control-m ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

(defun toggle-narrow()
  "Narrow to region, iff region is marked, otherwise widen"
  (interactive)
  (if mark-active
      (narrow-to-region (region-beginning) (region-end))
    (widen)))                           ; may replace C-x n n / C-x n w

;; Copyright (C) 1997, 1998 Thien-Thi Nguyen
(defun another-line ()
  "Copy line, preserving cursor column, and increment any numbers found.
This should probably be generalized in the future."
  (interactive)
  (let* ((col (current-column))
     (bol (progn (beginning-of-line) (point)))
     (eol (progn (end-of-line) (point)))
     (line (buffer-substring bol eol)))
    (beginning-of-line)
    (while (re-search-forward "[0-9]+" eol 1)
      (let ((num (string-to-number (buffer-substring
                  (match-beginning 0) (match-end 0)))))
    (replace-match (int-to-string (1+ num)))))
    (beginning-of-line)
    (insert line "\n")
    (move-to-column col)))

(defun get-posix-username ()
  "May be use instead of 'USER-LOGIN-NAME if security is needed."
  (let ((me (execvp "whoami")))
    (if (stringp me)
        (replace-regexp-in-string "\n" "" me)
      nil)))

(defun mars/valid-string (clause basename substitute &optional prefix suffix)
  "Returns a string if the clause on it is true. Or returns the substitute
which is not affected by suffix optional argument."
  (let ((alternate (if prefix (concat prefix substitute) substitute)))
    (if (stringp basename)
        (let* ((name (if prefix (concat prefix basename)
                       basename))
               (complete-name (if suffix (concat name suffix)
                                name)))
          (if (funcall clause complete-name)
              complete-name
            alternate))
      alternate)))

(defun mars/username-file-if-any (root post default)
  (mars/valid-string 'file-exists-p
                     (user-login-name)
                     default
                     root
                     post))

;; eshell
(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; lennart-borgman libraries' loaders
(defun nxhtml-loader ()
  "load NXHTML library and all lennart-borgman libraries (including Unit Test additions named ERT2)"
  (unless (fboundp 'nxhtml-list-loaded-features)
    (let ((name       "nxhtml")
          (nxhtml-dir (locate-library "autostart")))
      (when nxhtml-dir
        (when (and (eq (string-match (concat ".*\\/" name ".*") nxhtml-dir) 0) ; check "/nxhtml" pattern is in the path
                   (file-exists-p (concat (file-name-directory nxhtml-dir)
                                          (concat name "/" name ".el")))) ; check nxhtml.el exists
          (load nxhtml-dir))))))
(defalias 'ert2-loader 'nxhtml-loader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defs.el ends here

