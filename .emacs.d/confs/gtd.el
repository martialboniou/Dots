;;; gtd.el ---
;;
;; Filename: gtd.el
;; Description:
;; Author: Martial Boniou
;; Maintainer:
;; Created: Tue Feb 22 11:31:42 2011 (+0100)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 30
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: OrgMode / Remember /
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

(unless (boundp 'mars/local-root-dir) (condition-case nil (load (concat (file-name-directory load-file-name) "vars")) (error "Unable to get custom variables")))

;;; ORG MODE
;; (from 'Using Org Mode as a Day Planner' by John Wiegley)
(defvar *notes-dir* (concat (file-name-as-directory
                 (concat (file-name-as-directory mars/local-root-dir)
                     mars/personal-data))
                "Notes"))

(require 'org-install)
(require 'org-protocol)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key mode-specific-map [?a] 'org-agenda)

(eval-after-load "org"
  '(progn
     (defvar wiegley/org-archive-expiry-days 2
       "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")
     ;; common setup
     (setq org-log-done 'time
           safe-local-variable-values '((after-save-hook archive-done-tasks))
           org-todo-keywords
           '((sequence "TODO(t)" "STARTED(s@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "APPT(a@!)" "|" "DONE(d!)" "DEFERRED(f)" "CANCELLED(c@)")))
     (add-hook 'org-mode-hook
               (lambda ()
                 (setq truncate-lines nil))) ; turn on soft wrapping mode for org mode
     ;; original C-n/C-p behavior kept
     (add-hook 'org-agenda-mode-hook
               (lambda ()
                 (define-key org-agenda-mode-map "\C-n" 'next-line)
                 (define-key org-agenda-keymap "\C-n" 'next-line)
                 (define-key org-agenda-mode-map "\C-p" 'previous-line)
                 (define-key org-agenda-keymap "\C-p" 'previous-line)))
     ;; auto archiving
     (defun wiegley/org-archive-done-tasks ()
       (interactive)
       (save-excursion
         (goto-char (point-min))
         (let ((done-regexp
                (concat "\\* \\(" (regexp-opt org-done-keywords) "\\) "))
               (state-regexp
                (concat "- State \"\\(" (regexp-opt org-done-keywords)
                        "\\)\"\\s-*\\[\\([^]\n]+\\)\\]")))
           (while (re-search-forward done-regexp nil t)
             (let ((end (save-excursion
                          (outline-next-heading)
                          (point)))
                   begin)
               (goto-char (line-beginning-position))
               (setq begin (point))
               (when (re-search-forward state-regexp end t)
                 (let* ((time-string (match-string 2))
                        (when-closed (org-parse-time-string time-string)))
                   (if (>= (time-to-number-of-days
                            (time-subtract (current-time)
                                           (apply #'encode-time when-closed)))
                           wiegley/org-archive-expiry-days)
                       (org-archive-subtree)))))))))
     (defalias 'archive-done-tasks 'wiegley/org-archive-done-tasks)))
;; viper compatibility
;; (if (boundp 'viper-version)
;;     (define-key viper-vi-global-user-map "C-c /" 'org-sparse-tree))
;; windmove compatibility
;;(add-hook 'org-shiftup-final-hook    'windmove-up)
;;(add-hook 'org-shiftleft-final-hook  'windmove-left)
;;(add-hook 'org-shiftdown-final-hook  'windmove-down)
;;(add-hook 'org-shiftright-final-hook 'windmove-right)
;; remember
(org-remember-insinuate)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(custom-set-variables
 '(org-directory *notes-dir*)
 '(org-default-notes-file (concat org-directory "/Notes.org"))
 '(org-agenda-files (mapcar (lambda (item)
                              (concat org-directory "/" item ".org"))
                            '("Todo")))
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
           ("c" todo "DONE|DEFERRED|CANCELLED" nil)
           ("w" todo "WAITING" nil)
           ("W" agenda "" ((org-agenda-ndays 21)))
           ("A" agenda ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
             (org-agenda-ndays 1)
             (org-agenda-overriding-header "Today's Priority #A tasks: ")))
           ("u" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheluded) (quote deadline)
                                          (quote regexp) "<[^\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote ((116 "* TODO %?\n  %u" "~/.emacs.d/data/Notes/Todo.org" "Tasks") ; 116 => ?t
           (110 "* %u %?" "~/.emacs.d/data/Notes/Notes.org" "Notes")        ; 110 => ?n
           (115 "* %u %?" "~/.emacs.d/data/Notes/Iris.gpg"  "Notes"))))     ; 115 => ?s [encrypted]
                                                                            ; 105 => ?i
 '(org-capture-templates
   (quote (("tname" "Link" entry
            (file+headline org-default-notes-file "Links to Read")
            "* %a\n %?\n %i"))))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))

(eval-after-load "remember"
  '(progn
     ;; http://metajack.im/2008/12/30/gtd-capture-with-emacs-orgmode
     (defadvice remember-finalize (after delete-remember-frame activate)
       "Advise remember-finalize to close the frame if it is the remember frame"
       (if (equal "remember" (frame-parameter nil 'name))
           (delete-frame)))
     (defadvice remember-destroy (after delete-remember-frame activate)
       "Advise remember-destroy to close the frame if it is the rememeber frame"
       (if (equal "remember" (frame-parameter nil 'name))
           (delete-frame)))
     ;; single window
     (add-hook 'remember-mode-hook
               'delete-other-windows)))

(defun make-remember-frame ()
  "Create a new frame and run org-remember."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 10)))
  (select-frame-by-name "remember")
  (org-remember))

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
    wind)
    (if buf
    (if (setq wind (get-buffer-window buf))
        (select-window wind)
      (if (called-interactively-p)
          (progn
        (select-window (display-buffer buf t t))
        (org-fit-window-to-buffer))
        (with-selected-window (display-buffer buf)
          (org-fit-window-to-buffer))))
      (call-interactively 'org-agenda-list))))

(defun mars/today-calendar ()
  "Open `org-agenda' on today."
  (interactive)
  (org-agenda-list nil nil 'day))

(defun mars/unscheduled-tasks ()
  "Open `org-agenda' on unscheduled tasks."
  (interactive)
  (let ((org-agenda-skip-function (lambda () (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "<[^\n]+>")))
        (org-agenda-overriding-header "Unscheduled TODO entries: "))
    (org-todo-list t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gtd.el ends here

