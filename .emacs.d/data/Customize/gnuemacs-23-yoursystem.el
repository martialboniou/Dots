;;; custom.el ---
;;
;; Filename: custom.el
;; Description:
;; Author: Martial Boniou
;; Maintainer:
;; Created: Sat Jan 12 22:23:49 2008
;; Version:
;; Last-Updated: Thu Mar 10 15:26:42 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 396
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;;
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
;; published by the Free Software Foundation; either version 2, or
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
;; Floor, Boston, MA 02110-1301, USA.(custom-set-variables

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-electric-escape t)
 '(abbrev-mode t)
 '(asm-comment-char 35)
 '(auto-compression-mode t)
 '(auto-insert-directory "~/.emacs.d/data/Insert")
 '(autosave-interval 50)
 '(backup-by-copying t)
 '(backup-by-copying-when-linked t)
 '(backup-by-copying-when-mismatch t)
 '(bongo-enabled-backends (quote (vlc)))
 '(bongo-next-action (quote bongo-play-next-or-stop))
 '(bongo-vlc-program-name "/usr/bin/vlc")
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "cc-mode"))) t)
 '(case-fold-search t)
 '(column-number-mode t)
 '(completion-ignore-case t t)
 '(ctypes-install t nil (ctypes))
 '(cursor-in-non-selected-windows t)
 '(custom-buffer-done-kill t)
 '(custom-file (safe-build-custom-file "Customize"))
 '(default-frame-alist (quote ((tool-bar-lines . 0) (vertical-scroll-bars) (cursor-type . box) (internal-border-width . 0) (left-fringe . 1) (right-fringe) (fringe) (background-mode . dark))))
 '(default-major-mode (quote text-mode) t)
 '(default-tab-width 4 t)
 '(delete-old-versions t)
 '(desktop-buffers-not-to-save "\\(^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|.*_flymake.*\\)$")
 '(desktop-files-not-to-save (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS" "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb" "/[^/:]*:\\|.*_flymake\\..*" "\\)$"))
 '(desktop-minor-mode-table (quote ((auto-fill-function auto-fill-mode) (vc-mode nil) (vc-dired-mode nil) (flymake-mode nil) (ecb-minor-mode nil) (semantic-show-unmatched-syntax-mode nil) (semantic-stickyfunc-mode nil) (senator-minor-mode nil) (semantic-idle-scheduler-mode nil))))
 '(diff-switches "-u")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ecb-options-version "2.40")
 '(ecb-wget-setup (quote cons))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-use-long-help-message nil)
 '(elscreen-default-buffer-initial-message ";; Welcome home, Sir !

")
 '(emms-cache-file "~/.emacs.d/.emms-cache")
 '(emms-info-asynchronously nil)
 '(emms-info-functions (quote (emms-info-mpd)))
 '(emms-player-list (quote (emms-player-mpd)))
 '(emms-player-mpd-music-directory "~/Music")
 '(emms-player-mplayer-command-name "/usr/bin/mplayer")
 '(emms-player-mplayer-playlist-command-name "/usr/bin/mplayer")
 '(emms-playlist-buffer-name "*EMMS Playlist*")
 '(emms-playlist-default-major-mode (quote emms-playlist-mode))
 '(emms-playlist-mode-open-playlists t)
 '(emms-show-format "You're listening to %s")
 '(emms-stream-default-action "play")
 '(emms-volume-change-function (quote emms-volume-mpd-change))
 '(enable-recursive-minibuffers t)
 '(even-window-heights nil)
 '(exec-path (quote ("~/.tools/bin" "/usr/local/bin" "/opt/local/bin" "/usr/local/sbin" "/opt/local/sbin" "/sw/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin")))
 '(face-font-family-alternatives (quote (("Envy Code R" "Monospace" "courier" "fixed") ("courier" "CMU Typewriter Text" "fixed") ("Sans Serif" "helv" "helvetica" "arial" "fixed") ("helv" "helvetica" "arial" "fixed"))))
 '(flymake-gui-warnings-enabled nil)
 '(focus-follows-mouse nil)
 '(fold-dwim-outline-style-default (quote nested))
 '(fortune-dir "/usr/share/games/fortunes/")
 '(frame-background-mode (quote dark))
 '(frame-title-format "(%b)" t)
 '(global-semantic-tag-folding-mode t nil (semantic-util-modes))
 '(hl-paren-colors (quote ("orange1" "yellow1" "greenyellow" "green1" "springgreen1" "cyan1" "slateblue1" "magenta1" "purple")))
 '(howm-menu-refresh-after-save t)
 '(icicle-reminder-prompt-flag 3)
 '(ido-create-new-buffer (quote always))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message "mars")
 '(inhibit-startup-screen t)
 '(inihibit-startup-message t)
 '(initial-scratch-message ";; Welcome home, Sir You !

")
 '(ispell-local-dictionary-alist (quote (("francais" "" "" "" nil nil "~tex" undecided) ("british" "" "" "" nil nil "~tex" undecided))))
 '(ispell-program-name "/opt/local/bin/aspell")
 '(kept-new-versions 6)
 '(line-number-mode t)
 '(mars-windows-archiver-file "~/.emacs.d/data/windows-archiver")
 '(max-mini-window-height 0.1)
 '(menu-bar-mode t)
 '(menu-prompting t)
 '(newsticker-cache-filename "~/.emacs.d/data/newsticker/cache")
 '(newsticker-groups-filename "~/.emacs.d/data/newsticker/groups")
 '(newsticker-html-renderer (quote w3m-region))
 '(newsticker-imagecache-dirname "~/.emacs.d/data/newsticker/images")
 '(newsticker-url-list (quote (("Giles Bowket" "http://gilesbowkett.blogspot.com/feeds/posts/default" nil nil nil) ("MAKE Magazine" "http://blog.makezine.com/index.xml" nil nil nil) ("Slashdot" "http://rss.slashdot.org/Slashdot/slashdot" nil nil nil) ("Beau à la louche" "http://beaualalouche.canalblog.com/rss.xml" nil nil nil) ("Le petit journal de Julia" "http://lepetitjournaldejulia.blogspot.com/feeds/posts/default" nil nil nil) ("Planet Factor" "http://planet.factorcode.org/feed.xml" nil nil nil) ("Neil Mitchell's Haskell Blog" "http://neilmitchell.blogspot.com/feeds/posts/default" nil nil nil) ("Java World" "http://www.javaworld.com/features/index.xml" nil nil nil) ("#doesNotUnderstand" "http://feeds2.feedburner.com/doesnotunderstand" nil nil nil) ("Random Hacks" "http://www.randomhacks.net/xml/rss20/feed.xml" nil nil nil) ("Talking Meta" "http://feeds.feedburner.com/TalkingMeta" nil nil nil) ("On Smalltalk" "http://feeds.feedburner.com/OnSmalltalk" nil nil nil) ("Higher-Order" "http://blog.higher-order.net/feed/" nil nil nil) ("R'lyeh" "http://skrzynia.nerim.net/blog/rss.php" nil nil nil))))
 '(newsticker-url-list-defaults nil)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier nil)
 '(ns-antialias-text t)
 '(ns-command-modifier (quote meta))
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(nxhtml-default-encoding (quote utf-8))
 '(openwith-associations (quote (("\\.pdf\\'" "open" (file)) ("\\.mp3\\'" "/opt/local/bin/mplayer" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "/opt/local/bin/mplayer" ("-idx" file)) ("\\.\\(?:jp?g\\|png\\)\\'" "/usr/bin/f-spot" (file)) ("\\.image\\'" "open" (file)))))
 '(org-agenda-custom-commands (quote (("d" todo "DELEGATED" nil) ("c" todo "DONE|DEFERRED|CANCELLED" nil) ("w" todo "WAITING" nil) ("W" agenda "" ((org-agenda-ndays 21))) ("A" agenda "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))) (org-agenda-ndays 1) (org-agenda-overriding-header "Today's Priority #A tasks: "))) ("u" alltodo "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheluded) (quote deadline) (quote regexp) "<[^
]+>"))) (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (mapcar (lambda (item) (concat org-directory "/" item ".org")) (quote ("Todo"))))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file (concat org-directory "/Notes.org"))
 '(org-diary-agenda-file "~/.emacs.d/data/Notes/Diary.org")
 '(org-directory *notes-dir*)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (quote ((116 "* TODO %?
  %u" "~/.emacs.d/data/Notes/Todo.org" "Tasks") (110 "* %u %?" "~/.emacs.d/data/Notes/Notes.org" "Notes") (115 "* %u %?" "~/.emacs.d/data/Notes/Iris.gpg" "*Secreto*"))))
 '(org-reverse-note-order t)
 '(org-special-ctrl-a/e t)
 '(read-file-name-completion-ignore-case t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(resize-mini-windows (quote grow-only) t)
 '(save-abbrevs nil)
 '(savehist-file "~/.emacs.d/data/history")
 '(scroll-preserve-screen-position (quote keep))
 '(semantic-idle-scheduler-idle-time 3)
 '(semantic-self-insert-show-completion-function (lambda nil (semantic-ia-complete-symbol-menu (point))))
 '(sentence-end "[.?!][]\"')}]*\\($\\| $\\|     \\|  \\| \\)[
]*")
 '(sentence-end-double-space t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil)
 '(standard-indent 4)
 '(svn-status-verbose nil)
 '(tab-width 4)
 '(tabkey2-in-minibuffer t)
 '(tramp-default-method "ssh")
 '(tramp-persistency-file-name "~/.emacs.d/data/tramp")
 '(undo-limit 50000)
 '(user-full-name "Me")
 '(user-mail-address (concat "Me" "@" "yourserver.com"))
 '(version-control t)
 '(visual-line-mode nil t)
 '(volume-backend nil)
 '(weblogger-config-alist (quote (("default" ("user" . "me") ("server-url" . "http://yourserver.net/configuration") ("weblog" . "1")))))
 '(wl-prefetch-threshold 60000)
 '(wl-summary-keep-cursor-command (quote (wl-summary-goto-folder wl-summary-goto-last-visited-folder)))
 '(wl-summary-move-order (quote unread))
 '(x-gtk-use-old-file-dialog nil)
 '(x-gtk-whole-detached-tool-bar t)
 '(x-select-enable-clipboard t)
 '(x-stretch-cursor t))

;; Say that color-theme doesn't manage all packages; we should remove
;; pinky effect and bad pink-blue style (unreadable with light text):
;; action-lock
;; ecb pink effects on directory
;; makefile ugly pink tabs
;; flymake blue-pink selection
;; HOWM unreadable style when background is dark
;; + others like isearch, wl (brown on block is a bad idea) in theme.el
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1e1e27" :foreground "#cebfad" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "DejaVu_Sans_Mono"))))
 '(action-lock-face ((((class color)) (:background "black" :foreground "DeepSkyBlue" :overline "yellow"))))
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
 '(diredp-dir-priv ((((background dark)) (:inherit font-lock-doc-face :weight bold))))
 '(diredp-file-name ((((background dark)) (:inherit default))))
 '(ecb-analyse-face ((t (:inherit ecb-default-highlight-face :background "chocolate1" :foreground "black"))))
 '(ecb-default-highlight-face ((t (:background "chocolate4"))))
 '(ecb-directory-face ((t (:inherit ecb-default-highlight-face :background "chocolate4"))))
 '(ecb-history-face ((t (:inherit ecb-default-highlight-face :background "chocolate1" :foreground "black"))))
 '(ecb-method-face ((t (:inherit ecb-default-highlight-face :background "chocolate4"))))
 '(ecb-source-face ((t (:inherit ecb-default-highlight-face :background "chocolate4"))))
 '(ecb-tag-header-face ((((class color) (background dark)) (:foreground "lightblue1"))))
 '(ediff-even-diff-Ancestor ((((class color) (min-colors 16)) (:background "Grey40" :foreground "White"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "Grey40" :foreground "White"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "Grey40" :foreground "White"))))
 '(ediff-odd-diff-C ((((class color) (min-colors 16)) (:background "Grey40" :foreground "White"))))
 '(emms-playlist-selected-face ((((class color) (background dark)) (:background "GoldenRod" :foreground "black"))))
 '(emms-playlist-track-face ((((class color) (background dark)) (:foreground "#8722c9"))))
 '(flymake-errline ((((class color)) (:background "#dd4400"))))
 '(flymake-warnline ((((class color)) (:background "SlateBlue4"))))
 '(howm-menu-list-face ((t (:foreground "green"))))
 '(howm-mode-keyword-face ((((class color)) (:foreground "green"))))
 '(howm-mode-ref-face ((((class color) (background dark)) (:foreground "sandy brown"))))
 '(howm-mode-title-face ((((class color)) (:foreground "grey"))))
 '(howm-reminder-normal-face ((((class color)) (:foreground "khaki"))))
 '(howm-reminder-tomorrow-face ((((class color)) (:background "Cyan" :foreground "black"))))
 '(howm-view-hilit-face ((((class color)) (:foreground "yellow"))))
 '(howm-view-name-face ((((class color)) (:foreground "purple"))))
 '(js2-builtin-face ((t (:foreground "Violet"))))
 '(js2-comment-face ((t (:foreground "green"))))
 '(js2-external-variable-face ((t (:foreground "tomato1"))))
 '(js2-function-name-face ((t (:foreground "gold"))))
 '(js2-function-param-face ((t (:foreground "chartreuse"))))
 '(js2-instance-member-face ((t (:foreground "magenta1"))))
 '(js2-jsdoc-tag-face ((t (:foreground "DeepSkyBlue1"))))
 '(js2-jsdoc-value-face ((t (:foreground "PeachPuff"))))
 '(js2-keyword-face ((t (:foreground "Orange"))))
 '(js2-private-function-call-face ((t (:foreground "brown1"))))
 '(js2-private-member-face ((t (:foreground "PeachPuff"))))
 '(js2-regexp-face ((t (:foreground "LightBlue"))))
 '(js2-string-face ((t (:foreground "LightBlue"))))
 '(js2-type-face ((t (:foreground "Cyan"))))
 '(js2-variable-name-face ((t (:foreground "chartreuse"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:inverse-video t))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) (:background "gray10"))))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "gray20"))))
 '(newsticker-display-interval 15.3)
 '(newsticker-enclosure-face ((t (:background "orange" :foreground "black" :weight bold))))
 '(newsticker-sort-method (quote sort-by-time))
 '(newsticker-treeview-selection-face ((((class color) (background dark)) (:background "#bbbbff" :foreground "black"))))
 '(newsticker-url-list-defaults (quote (("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss" nil 3600))))
 '(pabbrev-suggestions-face ((((class color) (background dark)) (:foreground "orange"))))
 '(paren-face-match ((t (:background "#8722c9" :foreground "black"))))
 '(paren-face-mismatch ((t (:background "#E67321" :foreground "black"))))
 '(paren-face-no-match ((((class color)) (:background "DarkRed" :foreground "black"))))
 '(secondary-selection ((t (:overline "gold" :underline "gold"))))
 '(speedbar-file-face ((((class color) (background dark)) (:foreground "grey"))))
 '(speedbar-highlight-face ((((class color) (background dark)) (:background "#4542ff" :inverse-video t :overline t :underline t))))
 '(speedbar-separator-face ((((class color) (background dark)) (:background "#456767" :foreground "white" :overline "gray"))))
 '(trailing-whitespace ((t (:background "grey" :strike-through "red"))))
 '(vhdl-speedbar-configuration-face ((((class color) (background dark)) (:foreground "#b83412"))))
 '(viper-minibuffer-emacs ((((class color)) (:inherit font-lock-warning-face))))
 '(viper-minibuffer-insert ((((class color)) (:inherit default))))
 '(viper-minibuffer-vi ((((class color)) (:inherit font-lock-builtin-face))))
 '(viper-replace-overlay ((t nil)))
 '(viper-search ((((class color)) (:inherit isearch))))
 '(wl-highlight-action-argument-face ((((class color) (background dark)) (:foreground "orange" :slant italic))))
 '(wl-highlight-demo-face ((((class color) (background dark)) (:background "#000000" :foreground "#d9ffd9"))))
 '(wl-highlight-folder-closed-face ((((class color) (background dark)) (:foreground "red"))))
 '(wl-highlight-folder-few-face ((((class color) (background dark)) (:foreground "gold" :slant italic :weight bold))))
 '(wl-highlight-folder-many-face ((((class color) (background dark)) (:foreground "#ddee99" :slant italic :weight bold))))
 '(wl-highlight-folder-unknown-face ((((class color) (background dark)) (:foreground "#14ff45"))))
 '(wl-highlight-folder-unread-face ((((class color) (background dark)) (:foreground "#3486f5"))))
 '(wl-highlight-folder-zero-face ((((class color) (background dark)) (:foreground "white" :weight bold))))
 '(wl-highlight-header-separator-face ((((class color)) (:background "DarkRed" :foreground "Black"))))
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
 '(wl-highlight-summary-unread-face ((((class color) (background dark)) (:foreground "#eee")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custom.el ends here


