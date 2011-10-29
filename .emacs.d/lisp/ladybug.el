;;; ladybug.el --- 
;; 
;; Filename: ladybug.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 26 00:15:15 2011 (+0100)
;; Version: 
;; Last-Updated: Wed Oct 19 21:03:16 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 14
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
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

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'elisp)

;;; http://www.emacswiki.org/emacs/DrewsElispLibraries
;; > [new:MartialBoniou:2011-02-25 22:49 UTC]
;; > Hi Drew. I notice a bug in my emacs: Gnu Emacs 23.2.92.1. The 'switch-to-buffer in autofit-frame.el (used by dired-details+) is blocked when I switch from some buffers (for example, from Customize, if I quit I get a 'buffer is nil' message. I did this:
;; > <pre>
;; > (setq buffer (if buffer (get-buffer-create buffer)
;; > (other-buffer))) ; If string arg, convert to buffer.
;; > </pre>
;; > Thanks for your packages and everything you do for free software. -Martial
;; > [new:DrewAdams:2011-02-26 01:36 UTC]
;; > Thanks for your report and fix, Martial. Looks like they allowed a `nil' ##BUFFER## arg starting with Emacs 22! Better late than never. Thx -- DrewAdams
;; > ----
;; (eval-after-load "autofit-frame"
;;   '(progn
;;      (defun switch-to-buffer (buffer &optional norecord)
;;        (interactive
;;         (list (read-buffer "Switch to buffer: "
;;                            (if (fboundp 'another-buffer) ; In `misc-fns.el'.
;;                                (another-buffer nil t)
;;                              (other-buffer (current-buffer))))))
;;        (setq buffer (if buffer (get-buffer-create buffer)
;;                       (other-buffer))) ; If string arg, convert to buffer.
;;        (let ((orig-buf (current-buffer)))
;;          (prog1 (if (window-dedicated-p (selected-window))
;;                     (switch-to-buffer-other-window buffer)
;;                   (old-switch-to-buffer buffer norecord))
;;            (and (one-window-p t)
;;                 (not (eq buffer orig-buf))     ; Don't resize if same buffer.
;;                 autofit-frames-flag
;;            (fit-frame)))))))
;; 
;; (defun my-minibuffer-minor-mode ()
;;   (when (fboundp my-minibuffer-minor-mode)
;;     (funcall (symbol-function my-minibuffer-minor-mode))))
;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-minor-mode)
;; ;;(add-hook 'minibuffer-exit-hook 'my-minibuffer-minor-mode)
;; (defadvice anything (around set-major-mode activate)
;;   (let ((my-minibuffer-minor-mode 'my-anything-minibuffer-mode))
;;     ad-do-it))
;; (easy-mmode-define-minor-mode my-anything-minibuffer-mode
;;                               "Anything MiniBuffer Mode"
;;                               nil
;;                               " Anything MiniBuffer"
;;                               '())
;; (vimpulse-map ";" 'viper-ex)
;; (vimpulse-map ":" 'anything-M-x)
;; (vimpulse-map "?" 'describe-bindings)
;; (vimpulse-map "l" 'forward-char)
;; (vimpulse-map "h" 'backward-char)
;; (vimpulse-map "F" 'find-file)
;; (vimpulse-map "f" 'jaunte)
;; (vimpulse-map "m" 'hs-toggle-hiding)
;; (vimpulse-map "M" 'my-toggle-hideshow-all)
;; (vimpulse-map " " 'anything)
;; (vimpulse-map "\C-r" 'anything-recentf)
;; (vimpulse-map "\C-y" 'yas/insert-snippet)
;; (vimpulse-map "\C-j" 'yafastnav-jump-to-forward)
;; (vimpulse-map "b" '(lambda ()
;;                      (interactive)
;;                      (anything 'anything-c-source-elscreen)
;;                      ))
;; 
;; (vimpulse-map "!" '(lambda ()
;;                      (interactive)
;;                      (anything 'anything-c-source-shell-command)
;;                      ))
;; 
;; (vimpulse-map "\C-s" 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
;; (vimpulse-map "td" 'elscreen-kill)
;; (vimpulse-map "tt" '(lambda ()
;;                      (interactive)
;;           (elscreen-create)
;;           (anything-recentf)
;;  ))
;; (vimpulse-map "H" 'elscreen-previous)
;; (vimpulse-map "L" 'elscreen-next)
;; (define-key viper-minibuffer-map "\C-g" 'keyboard-escape-quit)
;; (define-key viper-insert-global-user-map "\C-g" 'viper-exit-insert-state)
;; (define-key viper-insert-global-user-map "\C-h" 'delete-backward-char)
;; (define-key viper-insert-global-user-map "\C-b" 'backward-char)
;; (define-key viper-insert-global-user-map "\C-f" 'forward-char)
;; (define-key viper-insert-global-user-map "\C-n" 'next-line)
;; (define-key viper-insert-global-user-map "\C-p" 'previous-line)
;; (define-key viper-insert-global-user-map "\C-a" 'move-beginning-of-line)
;; (define-key viper-insert-global-user-map "\C-e" 'end-of-line)
;; (define-key viper-insert-global-user-map "\C-h" 'delete-backward-char)
;; (define-key viper-insert-global-user-map "\C-i" 'yas/expand)
;; (define-key viper-insert-global-user-map "\C-y" 'yas/insert-snippet)
;; (define-key vimpulse-visual-basic-map "v" 'end-of-line)
;; (define-key vimpulse-visual-basic-map ";" 'comment-dwim)
;; 
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'insert-state "\C-n" 'anything-next-line)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'insert-state "\C-p" 'anything-previous-line)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'insert-state "\C-f" 'anything-next-page)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'insert-state "\C-b" 'anything-previous-page)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'insert-state "\C-l" 'anything-force-update)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "o" 'anything-follow-mode)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "j" 'anything-next-line)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "k" 'anything-previous-line)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "\C-f" 'anything-next-page)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "\C-b" 'anything-previous-page)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "\C-l" 'anything-force-update)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "}" 'anything-next-source)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "{" 'anything-previous-source)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "gg" 'anything-beginning-of-buffer)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "G" 'anything-end-of-buffer)
;; (vimpulse-define-key 'my-anything-minibuffer-mode 'vi-state "/" 'anything-isearch)

;; (eval-after-load "highlight-parentheses"
;;   '(progn
;;      (when (assoc 'highlight-parentheses-mode minor-mode-alist)
;;        (setcdr (assoc 'highlight-parentheses-mode minor-mode-alist) '(""))) ; IMPORTANT: don't display name in mode-line
;;      (setq autopair-handle-action-fns
;;            (list 'autopair-default-handle-action
;;                  '(lambda (action pair pos-before)
;;                     (hl-paren-color-update))))))

(provide 'ladybug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ladybug.el ends here
