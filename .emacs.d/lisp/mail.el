;;; mail.el --- 
;; 
;; Filename: mail.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 18:23:21 2011 (+0100)
;; Version: 
;; Last-Updated: Tue Nov  8 12:58:05 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 99
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: Wanderlust
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

(require 'www)
(require 'gtd)
(require 'preamble)

(eval-when-compile (require 'sendmail)
                   (require 'wl))

;; TODO: REPLACE #'BBDB-VCARD-IMPORT BY #'trebb/BBDB-VCARD
(let* ((init-file-name (format "%s.wl" user-login-name))
       (init (or (conf-locate init-file-name)
                 (conf-locate "default.wl")))
       (folder-directory (expand-file-name
                          "wl"
                          (expand-file-name mars/personal-data
                                            mars/local-root-dir)))
       (folder (convert-standard-filename (expand-file-name
                                           (format "%s.folders" user-login-name)
                                           folder-directory))))
  (when init
    (when (file-exists-p init)
      (setq wl-init-file (convert-standard-filename init))))
  (if (file-exists-p folder)
      (setq wl-folders-file (convert-standard-filename folder))
    (let ((default-wl-fldr (expand-file-name "default.folders" folder-directory)))
      (when (file-exists-p default-wl-fldr)
        (setq wl-folders-file (convert-standard-filename default-wl-fldr)))))
  (let ((wl-lib (locate-library "wl")) wl-resource-rep)
    (unless (null wl-lib)
      (setq wl-resource-rep (file-name-directory wl-lib)))
    (when (and (not (null wl-resource-rep))
               (file-exists-p wl-resource-rep))
      (setq wl-icon-directory (expand-file-name "etc/icons" wl-resource-rep)))))

(eval-after-load "wl-draft"
  '(progn
     (when (boundp 'mail-user-agent)
       (setq mail-user-agent 'wl-user-agent))
     (when (fboundp 'define-mail-user-agent)
       (define-mail-user-agent
         'wl-user-agent
         'wl-user-agent-compose
         'wl-draft-send
         'wl-draft-kill
         'mail-send-hook))
     (setq confirm-frame-action-buffer-alist 
           (append '((major-mode . (wl-draft-mode)))
                   confirm-frame-action-buffer-alist))))

(eval-after-load "wl"
  '(progn
     (unless window-system
       (setq wl-demo-display-logo nil))
     ;; Maildir => <~Mail>/Maildir
     (when elmo-localdir-folder-path
       (setq elmo-maildir-folder-path 
             (expand-file-name "Maildir" elmo-localdir-folder-path))) 
     ;; imapfilter
     (when (and (file-readable-p "~/.imapfilter/config.lua")
                (executable-find "imapfilter"))
       (add-to-list 'wl-folder-check-entity-pre-hook
                    (lambda ()
                      (message "Calling imapfilter...")
                      (if (eq (call-process "imapfilter") 0)
                          (message "imapfilter ran fine.")
                        (message "error running imapfilter!")))))
     ;; w3m
     (when (executable-find w3m-program-name)
       (require 'octet)                 ; w3m octet for handling attachments
       (octet-mime-setup))
     (require 'filladapt)
     (load "mime-setup")
     (require 'bbdb-wl)
     ;; PGP
     ;; (mars/autoload "pgg"
     ;;                pgg-encrypt-region
     ;;                pgg-encrypt-symmetric-region
     ;;                pgg-decrypt-region
     ;;                pgg-verify-region
     ;;                pgg-insert-key
     ;;                pgg-snarf-keys-region)
     (bbdb-wl-setup)
     ;; (defun bbdb-wl-exit-2 ()
     ;;   (let ((bbdb-buf (get-buffer bbdb-buffer-name)))
     ;;     (when bbdb-buf
     ;;       (kill-buffer bbdb-buf)))
     ;;   (bbdb-save-db nil))
     
     ;; (remove-hook 'wl-exit-hook 'bbdb-wl-exit-2) ; kill BBDB buffer on wl quit

     ;; IMPORTANT: remove wl-biff from local-modeline and set it globally
     (setq wl-mode-line-display-priority-list '(plug title))
     (defun wl-mode-line-buffer-identification (&optional id)
       (let ((priorities '(plug title)))
         (let ((items (reverse wl-mode-line-display-priority-list))
               item)
           (while items
             (setq item (car items)
                   items (cdr items))
             (unless (memq item '(plug))
               (setq item 'title))
             (setq priorities (cons item (delq item priorities)))))
         (let (priority result)
           (while priorities
             (setq priority (car priorities)
                   priorities (cdr priorities))
             (cond
              ((eq 'plug priority)
               (when wl-show-plug-status-on-modeline
                 (setq result (append result '((wl-modeline-plug-status
                                                wl-modeline-plug-state-on
                                                wl-modeline-plug-state-off))))))
              (t
               (setq result (append result (or id '("WL: %12b")))))))
           (prog1
               (setq mode-line-buffer-identification result)
             (force-mode-line-update t)))))
     (let ((biff-states '(wl-modeline-biff-status
                          wl-modeline-biff-state-on
                          wl-modeline-biff-state-off))))
     (add-lambda-hook 'wl-init-hook
       (let ((biff-states '(wl-modeline-biff-status
                            wl-modeline-biff-state-on
                            wl-modeline-biff-state-off)))
         (unless (member biff-states global-mode-string)
           (setq global-mode-string
                 (cons biff-states
                       (cons " " global-mode-string))))))
     (add-lambda-hook 'wl-exit-hook
       (unless (fboundp 'position)
         (require 'cl))
       (let ((biff-states '(wl-modeline-biff-status
                            wl-modeline-biff-state-on
                            wl-modeline-biff-state-off))
             (pending global-mode-string)
             stop (pos 0))
         (while (and (null stop) pending)
           (if (equal (car pending) biff-states)
               (progn
                 (setq stop t)
                 ;; remove biff and the post-interval
                 ;; using CDDR and SETF side-effect
                 (if (> pos 0)
                     (setf (nthcdr pos global-mode-string) (cddr (nthcdr pos global-mode-string)))
                   (setq global-mode-string (cddr global-mode-string))))
             (incf pos))
           (setq pending (cdr pending)))))
     ;; MIME-VIEW
     (eval-after-load "mime-view"
       '(progn
          ;; add pdf support
          (ctree-set-calist-strictly
           'mime-acting-condition
           '((mode . "play")
             (type . application)(subtype . pdf)
             (method . my-mime-save-content-find-file)))
          ;; IMPORTANT: hack needed not to truncate lines in MIME-VIEW          
          (add-hook 'mime-view-mode-hook #'no-line-wrap-this-buffer) ; defined in <confs/defs.el>
          (add-hook 'wl-message-redisplay-hook #'no-line-wrap-this-buffer-internal)))
     ;; BBDB
     (remove-hook 'wl-message-redisplay-hook #'bbdb-wl-get-update-record) ; FIXME: temporary to avoid annoying mismatch bugs
     (define-key wl-draft-mode-map "\t" 'bbdb-complete-name) ; now TAB => BBDB
     (setq bbdb-use-pop-up t
           bbdb-electric-p t             ; be disposable with SPC
           signature-use-bbdb t
           bbdb-elided-display t
           bbdb-always-add-address t
           bbdb-wl-folder-regexp "^[^+.]"  ; get addresses anything but local or maildirs
           bbdb-wl-ignore-folder-regexp "^@" ; ignoring `@-' too
           bbdb-use-alternate-names t    ; use AKA
           bbdb-message-caching-enabled t ; be fast
           bbdb-north-american-phone-numbers-p nil
           wl-summary-from-function 'bbdb-wl-from-func
           bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0))
                                   ("X-MailingList" (".*$" ML 0))
                                   ("X-Ml_Name" (".*$" ML 0))
                                   ("X-Mailer" (".*$" User-Agent 0))
                                   ("X-Newsreader" (".*$" User-Agent 0))
                                   ("User-Agent" (".*$" User-Agent 0)) ; spy MUAs for stats
                                   ;; X-Face may be catch here too
                                   )
           bbdb-file-coding-system 'utf-8-unix ; utf-8 encoding
           file-coding-system-alist (cons '("\\.bbdb" utf-8 . utf-8)
                                          file-coding-system-alist)
           bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
           bbdb-ignore-some-messages-alist '(("From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))
     (add-hook 'bbdb-notice-hook #'bbdb-auto-notes-hook)
     (add-hook 'wl-mail-setup-hook #'bbdb-insinuate-sendmail)
     ;; citation
     ;; (add-hook 'mail-citation-hook #'cite-cite)
     ;; vCard case
     (load "ch6-bbdb-import-csv-buffer") ; Outlook vCard conversion
     (require 'bbdb-vcard-import) ; vCard import + the 2 following funs
     ;; fix silent errors of multi-line address entry
     (defun wicked/vcard-parse-region (beg end &optional filter)
       "Parse the raw vcard data in region, and return an alist representing data. This function is just like `vcard-parse-string' except that it operates on a region of the current buffer rather than taking a string as an argument. Note: this function modifies the buffer!"
       (or filter
           (setq filter 'vcard-standard-filter))
       (let ((case-fold-search t)
             (vcard-data nil)
             (pos (make-marker))
             (newpos (make-marker))
             properties value)
         (save-restriction
           (narrow-to-region beg end)
           (save-match-data
             ;; Unfold folded lines and delete naked carriage returns
             (goto-char (point-min))
             (while (re-search-forward "\r$\\|\n[ \t]" nil t)
               (goto-char (match-beginning 0))
               (delete-char 1))
             (goto-char (point-min))
             (re-search-forward "^begin:[ \t]*vcard[ \t]*\n")
             (set-marker pos (point))
             (while (and (not (looking-at "^end[ \t]*:[ \t]*vcard[ \t]*$"))
                         (re-search-forward ":[ \t]*" nil t))
               (set-marker newpos (match-end 0))
               (setq properties
                     (vcard-parse-region-properties pos (match-beginning 0)))
               (set-marker pos (marker-position newpos))
               (re-search-forward "\n[-A-Z0-9;=]+:")   ;; change to deal with multiline
               (set-marker newpos (1+ (match-beginning 0))) ;; change to deal with multiline
               (setq value
                     (vcard-parse-region-value properties pos (match-beginning 0)))
               (set-marker pos (marker-position newpos))
               (goto-char pos)
               (funcall filter properties value)
               (setq vcard-data (cons (cons properties value) vcard-data)))))
         (nreverse vcard-data)))

     ;; import phone number from Gmail or Linkedln vCards
     (defun wicked/bbdb-vcard-merge (record)
       "Merge data from vcard interactively into bbdb."
       (let* ((name (bbdb-vcard-values record "fn"))
              (company (bbdb-vcard-values record "org"))
              (net (bbdb-vcard-get-emails record))
              (addrs (bbdb-vcard-get-addresses record))
              (phones (bbdb-vcard-get-phones record))
              (categories (bbdb-vcard-values record "categories"))
              (notes (and (not (string= "" categories))
                          (list (cons 'categories categories))))
              ;; TODO: addrs are not yet imported.  To do this right,
              ;; figure out a way to map the several labels to
              ;; `bbdb-default-label-list'.  Note, some phone number
              ;; conversion may break the format of numbers.
              (bbdb-north-american-phone-numbers-p nil)
              (new-record (bbdb-vcard-merge-interactively name
                                                          company
                                                          net
                                                          nil ;; Skip addresses
                                                          phones ;; Include phones
                                                          notes)))
         (setq bbdb-vcard-merged-records (append bbdb-vcard-merged-records
                                                 (list new-record)))))
     ;; Replace vcard.el's definition
     (fset 'vcard-parse-region 'wicked/vcard-parse-region)
     ;; Replace bbdb-vcard-import.el's definition
     (fset 'bbdb-vcard-merge 'wicked/bbdb-vcard-merge)

     ;; BBDB db switcher
     ;; (defun bbdb-switch-to-other-bbdb-file (&optional db dont-ask)
     ;;   (interactive)
     ;;   (unless db
     ;;     (setq db (if dont-ask (expand-file-name "~/.bbdb") ; default one
     ;;                (read-file-name "Use bbdb database "))))
     ;;   (setq bbdb-file db
     ;;         bbdb-buffer (get-file-buffer db)))
     ;;  (require 'bbdb-rf)                      ; to export BBDB to Outlook...
     ;; mu-cite (citation/heading/signature) & PGP mailcrypt
     ;; (require 'mu-cite)
     
     ;; mailcrypt // thanks to <http://box.matto.nl/wanderlustgpg.html>
     (add-hook 'wl-summary-mode-hook #'mc-install-read-mode)
     (add-hook 'wl-mail-setup-hook #'mc-install-write-mode)
     (defun mc-wl-verify-signature ()
       (interactive)
       (save-window-excursion
         (wl-summary-jump-to-current-message)
         (mc-verify)))
     (defun mc-wl-decrypt-message ()
       (interactive)
       (save-window-excursion
         (wl-summary-jump-to-current-message)
         (let ((inhibit-read-only t))
           (mc-decrypt))))
     (eval-after-load "mailcrypt"
       '(progn
          (setq mc-modes-alist
                (append
                 (quote
                  ((wl-draft-mode (encrypt . mc-encrypt-message)
                                  (sign    . mc-sign-message))
                   (wl-summary-mode (decrypt . mc-wl-decrypt-message)
                                    (verify  . mc-wl-verify-signature))))
                 mc-modes-alist))))))

(defun mars/draft-email ()
  "Open an email draft on the default Wanderlust posting user."
  (interactive)
  (wl-draft (list (cons 'To "")))
  (run-hooks 'wl-mail-setup-hook)
  (mail-position-on-field "To"))

(defun mars/wl ()
  "Open Wanderlust in another frame."
  (interactive)
  (wl-other-frame))

(provide 'mail)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mail.el ends here

