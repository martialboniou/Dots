;;; common-pre-custom.el --- 
;; 
;; Filename: common-pre-custom.el
;; Description: Global system customization
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Mar 23 15:15:40 2011 (+0100)
;; Version: 0.3
;; Last-Updated: Sun Oct 23 19:19:36 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 94
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: general behavior/convention -- the `custom-file' is
;;              bound to a specific system
;;  
;;              this file is about:
;;                  - personal data related path
;;                  - general desired behavior
;;                  - better default colors
;; 
;; IMPORTANT: DON'T COMPILE
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

;;; DATA TODO: automatize
;; (defmacro set-custom-vars (&rest args)
;;   `(custom-set-variables
;;     ,(while (elt (list (pop args) (pop args)))
;;        `(,(car elt) ,(concat (file-name-as-directory mars/local-root-dir)
;;                              (file-name-as-directory mars/personal-data)
;;                              (cdr elt))))))
;; (set-custom-vars info "tota")
(let ((data-dir (concat (file-name-as-directory mars/local-root-dir)
                        (file-name-as-directory mars/personal-data))))
  (mapc (lambda (x) 
          (let ((newdir (file-name-as-directory (concat (file-name-as-directory "~/.emacs.d/data") x))))
        (unless (file-exists-p newdir)
          (make-directory newdir t))))
    '("cache/semanticdb"
      "cache/bookmark"
      "cache/newsticker/cache"
      "cache/newsticker/images"
      "cache/newsticker/groups"
      "cache/eshell"
      "Notes" "Insert" "BBDB" "elmo"))
  (let ((data-cache (expand-file-name (file-name-as-directory "~/.emacs.d/data/cache"))))
    (unless (fboundp 'flet) (require 'cl)) ; elisp obviously needs R*RS | CL standard
    (flet ((cachize (file) (expand-file-name (concat (file-name-as-directory data-cache) file))))
      (setq mars-windows-archiver-file (cachize "windows-archiver")
        kiwon/last-window-configuration-file (cachize "last-window-configuration")
        desktop-dir data-cache
        desktop-base-file-name "desktop"
        desktop-base-lock-name (concat (if (memq system-type '(ms-dos windows-nt cygwin)) "_" ".") desktop-base-file-name ".lock") ; TODO: add a fun in defs named `prefix-hidden-file'
        ido-save-directory-list-file (cachize "ido-last")
        recentf-save-file (cachize "recentf")
        anything-c-adaptive-history-file (cachize "anything-c-adaptive-history")
        bookmark-default-file (cachize "bookmark/emacs.bmk")
        bmkp-bmenu-commands-file (cachize "bookmark/bmenu-commands.el")
        bmkp-bmenu-state-file (cachize "bookmark/bmenu-state.el")
        newsticker-cache-filename (cachize "newsticker/cache")
        newsticker-imagecache-dirname (cachize "newsticker/images")
        newsticker-groups-filename (cachize "newsticker/groups")
        org-diary-agenda-file "~/.emacs.d/data/Notes/Diary.org"
        savehist-file (cachize "history")
        tramp-persistency-file-name (cachize "tramp")
        ac-comphist-file (cachize "ac-comphist.dat")
        semanticdb-default-save-directory (cachize "semanticdb")
        ede-project-placeholder-cache-file (cachize "projects.ede")
        ecb-tip-of-the-day-file (cachize "ecb-tip-of-day.el")
        auto-insert-directory "~/.emacs.d/data/Insert"
        bbdb-file (concat (file-name-as-directory "~/.emacs.d/data/BBDB")
                  (user-login-name)
                  ".bbdb")
        elmo-msgdb-directory "~/.emacs.d/data/elmo"
        eshell-directory-name (file-name-as-directory (cachize "eshell")) ; may need a final `slash'
        emms-cache-file (cachize "emms-cache")))))

;;; GENERAL BEHAVIOR
(setq standard-indent 4
      tab-width 4
      autosave-interval 50
      undo-limit 50000
      auto-compression-mode t
      backup-by-copying t
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t
      completion-ignore-case t
      custom-buffer-done-kill t
      newsticker-display-interval 15.3
      linum+-dynamic-format "%%%dd "
      linum+-smart-format linum+-dynamic-format)
(when (featurep 'ns)
  (setq ns-alternate-modifier nil
        ns-command-modifier 'meta       ; NeXT/Apple computers got real Meta keys
        ns-antialias-text t))

;;; BETTER DEFAULT COLORS
(unless (member system-type '(windows-nt ms-dos))
    ;(set-face-attribute 'default :height 120)
  (set-face-attribute 'default nil
                      :height 140
                      :family "DejaVu Sans Mono")) ; TODO: force DejaVu_Sans_Mono font install on OSX
(if window-system
  (setq hl-paren-colors
        '("orange1"
          "yellow1"
          "greenyellow"
          "green1"
          "springgreen1"
          "cyan1"
          "slateblue1"
          "magenta1"
          "purple"))
  (setq hl-paren-colors                 ; TODO: yellow1 not found in term / investigate
        '("orange1"
          "yellow"
          "greenyellow"
          "green1"
          "springgreen1"
          "cyan1"
          "slateblue1"
          "magenta1"
          "purple")))                      ; draw rainbows in LISPEM
(custom-set-faces
 '(action-lock-face ((((class color)) (:background "black" :foreground "DeepSkyBlue" :overline "yellow"))))
 ;; default paren-face for matching
 '(paren-face-match ((t (:inherit show-paren-match-face))))
 '(paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
 '(paren-face-no-match ((((class color) (background dark)) (:inherit paren-face :background "IndianRed4"))
                        (((class color) (background light))(:inherit paren-face :background "IndianRedD1"))))
 ;; viper using inheritance
 '(viper-minibuffer-emacs ((((class color)) (:inherit font-lock-warning-face))))
 '(viper-minibuffer-insert ((((class color)) (:inherit default))))
 '(viper-minibuffer-vi ((((class color)) (:inherit font-lock-builtin-face))))
 '(viper-replace-overlay ((t nil)))
 '(viper-search ((((class color)) (:inherit isearch))))
 ;; linum as fringe (like LineNr in Vim)
 '(linum ((((class color)) (:inherit fringe))))
 ;; dark background friend ido colors
 '(ido-only-match ((((class color) (background dark)) (:foreground "#00cd00"))  ; green3
                   (((class color) (background light)) (:foreground "#228b22")))) ; ForestGreen
 '(ido-subdir ((((class color) (background dark)) (:foreground "#cd0000")) 
                   (((class color) (background light)) (:foreground "#8b2222"))))
 ;; anything using inheritance
 '(anything-M-x-key-face ((t (:inherit font-lock-reference-face :underline t))))
 '(anything-dir-heading ((t (:inherit font-lock-doc-face :weight bold))))
 '(anything-dir-priv ((t (:inherit font-lock-doc-face :weight bold))))
 '(anything-dired-symlink-face ((t (:inherit font-lock-string-face :slant italic))))
 '(anything-file-name ((t (:inherit default))))
 '(anything-grep-file ((t (:inherit font-lock-builtin-face :underline t))))
 '(anything-grep-lineno ((t (:inherit font-lock-type-face))))
 '(anything-header ((t (:inherit font-lock-warning-face))))
 '(anything-isearch-match ((t (:inherit isearch))))
 '(anything-match ((t (:inherit match :inverse-video t))))
 '(anything-overlay-line-face ((t (:inherit font-lock-warning-face :foreground "default"))))
 ;; obvious trailing whitespace
 ;; '(trailing-whitespace ((((class color) (background dark))
 ;;                         (:background "gray90" :strike-through "red1"))
 ;;                        (((class color) (background light))   
 ;;                         (:background "gray10" :strike-through "red1"))))
 ;; no more flashy background in MuMaMo
 '(mumamo-background-chunk-major ((((class color) (min-colors 88)
                                    (background dark)) (:background "gray10"))
                                  (((class color) (min-colors 88)
                                    (background light)) (:background "gray90"))))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88)
                                       (background dark)) (:background "gray20"))
                                     (((class color) (min-colors 88)
                                       (background light)) (:background "gray80"))))
 ;; no more pinkish flymake
 '(flymake-errline ((((class color)) (:inherit font-lock-warning-face))))
 '(flymake-warnline ((((class color)) (:inherit font-lock-warning-face :inverse-video t))))
 ;; clearer dired
 '(diredp-dir-priv ((t (:inherit font-lock-doc-face :weight bold))))
 '(diredp-file-name ((t (:inherit default))))
 ;; TODO: clearer org-mode
 ;; '(org-date ((((color class)) (:inherit font-lock-number-face))))
 ;; '(org-agenda ((((color class)) (:inherit font-lock-keyword-face))))
 ;; greyish ediff (NOTE: color-theme may override this)
 '(ediff-even-diff-Ancestor ((((class color) (min-colors 16) (background dark))   (:background "SlateGray4" :foreground "SlateGray1"))
                             (((class color) (min-colors 16) (background light)) (:background "SlateGray1" :foreground "SlateGray4"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 16) (background dark)) (:background "gray40" :foreground "white"))
                     (((class color) (min-colors 16) (background light)) (:background "gray60" :foreground "black"))))
 '(ediff-odd-diff-B ((((class color) (min-colors 16) (background dark)) (:background "gray40" :foreground "white"))
                     (((class color) (min-colors 16) (background light)) (:background "gray60" :foreground "black"))))
 '(ediff-odd-diff-C ((((class color) (min-colors 16) (background dark)) (:background "gray40" :foreground "white"))
                     (((class color) (min-colors 16) (background light)) (:background "gray60" :foreground "black"))))
 '(ediff-even-diff-A ((((class color) (min-colors 16) (background dark)) (:background "gray20" :foreground "white"))
                      (((class color) (min-colors 16) (background light)) (:background "gray80" :foreground "black"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16) (background dark)) (:background "gray20" :foreground "white"))
                      (((class color) (min-colors 16) (background light)) (:background "gray80" :foreground "black"))))
 '(ediff-even-diff-C ((((class color) (min-colors 16) (background dark)) (:background "gray20" :foreground "white"))
                      (((class color) (min-colors 16) (background light)) (:background "gray80" :foreground "black"))))
 ;; readable default faces for emms
 '(emms-playlist-selected-face ((((class color)) (:inherit font-lock-keyword-face))))
 '(emms-playlist-track-face ((((class color)) (:inherit default))))
 ;; readable default faces for wanderlust AKA wl
 ;'(wl-highlight-action-argument-face ((((class color) (background dark)) (:foreground "orange" :slant italic))))
 '(wl-highlight-action-argument-face ((((class color)) (:inherit font-lock-builtin-face :italic t :slant italic))))
 '(wl-highlight-folder-closed-face ((((class color) (background dark)) (:foreground "red"))))
 '(wl-highlight-folder-few-face ((((class color) (background dark)) (:foreground "gold" :slant italic :weight bold))))
 '(wl-highlight-folder-many-face ((((class color) (background dark)) (:foreground "#ddee99" :slant italic :weight bold))))
 '(wl-highlight-folder-unknown-face ((((class color) (background dark)) (:foreground "#14ff45"))))
 '(wl-highlight-folder-unread-face ((((class color) (background dark)) (:foreground "#3486f5"))))
 '(wl-highlight-folder-zero-face ((((class color) (background dark)) (:foreground "white" :weight bold))))
 '(wl-highlight-header-separator-face ((((class color)) (:background "DarkRed" :foreground "Black"))))
 '(wl-highlight-demo-face ((((class color)) (:inherit default))))
 '(wl-highlight-logo-face ((((class color) (background dark)) (:background "#000000" :foreground "SkyBlue"))))
 '(wl-highlight-message-citation-header ((((class color) (background dark)) (:foreground "Yellow" :slant italic))))
 '(wl-highlight-message-header-contents ((t (:foreground "DarkOrange" :weight normal))))
 '(wl-highlight-message-headers ((t (:foreground "grey" :weight bold))))
 '(wl-highlight-message-important-header-contents ((t (:foreground "Green" :weight bold))))
 '(wl-highlight-message-important-header-contents2 ((t (:foreground "GreenYellow" :weight bold))))
 '(wl-highlight-message-signature ((((class color) (background dark)) (:foreground "#7878ee" :slant italic))))
 '(wl-highlight-message-unimportant-header-contents ((t (:foreground "#999" :weight normal))))
 '(wl-highlight-summary-answered-face ((((class color) (background dark)) (:foreground "yellow" :slant italic))))
 '(wl-highlight-summary-deleted-face ((((class color) (background dark)) (:background "DarkRed" :foreground "black" :slant italic :weight bold))))
 '(wl-highlight-summary-disposed-face ((((class color) (background dark)) (:background "orange" :foreground "black" :slant italic :weight bold))))
 '(wl-highlight-summary-forwarded-face ((((class color) (background dark)) (:foreground "orange" :overline t :underline t :slant italic))))
 '(wl-highlight-summary-high-unread-face ((t (:foreground "#3486f5" :weight bold))))
 '(wl-highlight-summary-killed-face ((((class color) (background dark)) (:foreground "DarkRed" :overline t :underline t))))
 '(wl-highlight-summary-new-face ((t (:foreground "#3486f5" :weight bold))))
 '(wl-highlight-summary-prefetch-face ((((class color) (background dark)) (:background "DeepSkyBlue" :foreground "black" :slant italic :weight bold))))
 '(wl-highlight-summary-refiled-face ((((class color) (background dark)) (:background "grey10" :foreground "orange" :slant italic :weight bold))))
 '(wl-highlight-summary-resend-face ((((class color) (background dark)) (:background "orange3" :foreground "black" :slant italic :weight bold))))
 '(wl-highlight-summary-target-face ((((class color)) (:foreground "HotPink1"))))
 '(wl-highlight-summary-thread-top-face ((((class color) (background dark)) (:foreground "GreenYellow"))))
 '(wl-highlight-summary-unread-face ((((class color) (background dark)) (:foreground "#eee"))))
 '(wl-highlight-summary-normal-face ((((class color) (background dark)) (:foreground "PaleGreen"))))
 '(wl-highlight-summary-low-read-face ((((class color) (background dark)) (:inherit wl-highlight-summary-normal-face :slant italic))))
 '(wl-highlight-summary-high-read-face ((((class color) (background dark)) (:inherit wl-highlight-summary-normal-face :weight bold))))
 ;; readable default faces for newsticker
 ;; '(newsticker-enclosure-face ((t (:background "orange" :foreground "black" :weight bold))))
 '(newsticker-treeview-selection-face ((((class color)) (:inherit newsticker-treeview-new-face :inverse-video t))))
 ;; howm needs more sober default colors
 '(howm-menu-list-face ((t (:foreground "green"))))
 '(howm-mode-keyword-face ((((class color)) (:foreground "green"))))
 '(howm-mode-ref-face ((((class color) (background dark)) (:foreground "sandy brown"))))
 '(howm-mode-title-face ((((class color)) (:foreground "grey"))))
 '(howm-reminder-normal-face ((((class color)) (:foreground "khaki"))))
 '(howm-reminder-tomorrow-face ((((class color)) (:background "Cyan" :foreground "black"))))
 '(howm-view-hilit-face ((((class color)) (:foreground "yellow"))))
 '(howm-view-name-face ((((class color)) (:foreground "purple")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common-pre-custom.el ends here
