 ;;; media.el ---
;;
;; Filename: media.el
;; Description:
;; Author: Martial Boniou
;; Maintainer:
;; Created: Sat Jan 19 20:16:06 2008
;; Version:
;; Last-Updated: Mon Oct 24 18:57:57 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 89
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: was emms-init.el initially
;;              2010-03: use all in emms to get the browser
;;                       and display covers
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log: switch from emms2 to emms3 (cvs branches through darcs)
;;
;;  TODO: split `loaddefs' in `loaddefs' + `cedet-loaddefs' autoloads
;;        in order not to load CEDET when media is loaded as standalone
;;        (b/c no need extra autoloads and CEDET in this case)
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
;; Floor, Boston, MA 02110-1301, USA.;;; emms-init.el --- Initialize emms

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'www)
(require 'kernel)

;;; * MPG123 *
;; (autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)

;;; * EMMS 3 *
(when (locate-library "emms")
  (setq emms-path (file-name-directory (locate-library "emms"))
    emms-bin-path (concat (file-name-as-directory emms-path) 
                  "bin/"
                  system-configuration)) ; IMPORTANT: move binary program in bin/<system-configuration>
  (mars/autoload '(("emms-source-file" emms-dired emms-add-directory-tree emms-add-directory emms-add-file)
                   ("emms" emms-playlist-buffer-list emms)
                   ("emms-streams" emms-streams emms-stream-init)))
  (eval-after-load 'emms
    '(progn
       (display-time)
       ;; (setq default-file-name-coding-system 'utf-8) ; maybe not correct
       (require 'emms-setup)

       ;; 'emms-devel         requires 'emms-all
       ;; 'emms-all           requires 'emms-lastfm-client
       ;; 'emms-lastfm-client requires 'w3m
       ;; 'w3m                not always here
       (defun emms-all ()
         "`emms-all' hack not to load w3m a related script."
         (emms-standard)
         (eval-and-compile
           (require 'emms-mode-line)
           (require 'emms-mark)
           (require 'emms-tag-editor)
           (require 'emms-streams)
           (require 'emms-lyrics)
           (require 'emms-playing-time)
           (require 'emms-player-mpd)
           (require 'emms-player-xine)
           (require 'emms-playlist-sort)
           (require 'emms-browser)
           (require 'emms-mode-line-icon)
           (require 'emms-cue)
           (when (boundp 'mars/w3m-exists)
             (require 'emms-lastfm-client))
           (require 'emms-bookmarks)
           (require 'emms-last-played))
         (emms-mode-line 1)
         (emms-mode-line-blank)
         (emms-lyrics 1)
         (emms-playing-time 1)
         (add-to-list 'emms-info-functions 'emms-info-cueinfo)
         (add-hook 'emms-player-started-hook #'emms-last-played-update-current))

       ;; load
       (emms-devel)                            ; emms-smart-browse on <F2>
       (emms-default-players)

       ;; fix processes for `emms-stream-info'
       (eval-after-load "emms-stream-info"
         '(progn
            ;; - mplayer need cache or "Icy Info" field may not be reached
            (defun emms-stream-info-mplayer-backend (url)
              "Backend command for running mplayer on URL. NEED CACHE"
              (condition-case excep
                  (call-process "mplayer" nil t nil
                                "-endpos" "0" "-vo" "null" "-ao" "null"
                                url)
                (file-error
                 (error "Could not find the mplayer backend binary"))))
            ;; - vlc requires vlc://quit to properly end
            (defun emms-stream-info-vlc-backend (url)
              "Backend command for running VLC on URL."
              (condition-case excep
                  (call-process "vlc" nil t nil
                                "-vvv" "--intf" "dummy" "--stop-time" "1" "--noaudio"
                                url "vlc://quit")
                (file-error
                 (error "Could not find the VLC backend binary"))))))

       ;; use the script by Fang Lungang to create cover_small/cover_med

       ;; emms: customization
       ;; (add-to-list 'emms-info-functions 'emms-info-mpd)
       ;; (setq emms-player-list 'emms-player-mpd)

       ;; need compiled libtag-based binary using: make emms-print-metadata
       (setq exec-path (append (list emms-bin-path "~/.tools/bin" "/opt/local/bin" "/sw/bin") exec-path))
       (require 'emms-info-libtag)
       (setq emms-info-functions '(emms-info-libtag))
       ;; (setq emms-player-list 'emms-player-mpd)
       
       ;; Custom variables
       
       (setq emms-info-asynchronously t)
       (setq emms-mode-line-mode-line-function
             (lambda nil
               (let ((track (emms-playlist-current-selected-track)))
                 (let ((title (emms-track-get track 'info-title)))
                   (let ((name (emms-track-get track 'name)))
                     (if (not (null title))
                         (format emms-mode-line-format title)
                       (if (not (null (string-match "^url: " (emms-track-simple-description track))))
                           (format emms-mode-line-format "Internet Radio")
                         (setq name2 (replace-regexp-in-string ".*\/" "" name))
                         (format emms-mode-line-format name2))))))))
       (emms-mode-line-disable)
       (emms-mode-line-enable)
       (setq emms-track-description-function
             (lambda (track)
               (let ((artist (emms-track-get track 'info-artist))
                     (album  (emms-track-get track 'info-album))
                     (number (emms-track-get track 'info-tracknumber))
                     (title  (emms-track-get track 'info-title)))
                 (if (and artist album title)
                     (if number
                         (format "%s: %s - [%03d] %s" artist album (string-to-int number) title)
                       (format "%s: %s - %s" artist album title))
                   (emms-track-remote-simple-description track)))))
       ;; (set-face-attribute 'emms-playlist-track-face    nil :font "DejaVu Sans-10")
       ;; (set-face-attribute 'emms-playlist-selected-face nil :background "White" :foreground "Firebrick")
       ;; Initialization
       (define-emms-simple-player mplayer-mp3 '(file url)
         "\\.[mM][pP][23]$" "mplayer")

       (define-emms-simple-player mplayer-ogg '(file)
         (regexp-opt '(".ogg" ".OGG" ".FLAC" ".flac" )) "mplayer")

       (define-emms-simple-player mplayer-playlist '(streamlist)
         "http://" "mplayer" "-playlist")

       (define-emms-simple-player mplayer-list '(file url)
         (regexp-opt '(".m3u" ".pls")) "mplayer" "-playlist")

       (define-emms-simple-player mplayer-video '(file url)
         (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv"
                       ".wma" ".mov" ".avi" ".divx" ".ogm" ".asf"
                       ".mkv" "http://")) "mplayer")

       (setq emms-player-list '(emms-player-mplayer-mp3
                                emms-player-mplayer-ogg
                                emms-player-mplayer-playlist
                                emms-player-mplayer-video
                                emms-player-mplayer-list
                                ))

       ;; When asked for emms-play-directory,
       ;; always start from this one
       (setq emms-source-file-default-directory "~/Music/"
             emms-stream-default-list '(       ; Metal Rules
                                        ("Metal On: The Thrasher"
                                         "http://67.212.188.94:8100" 1 url)
                                        ("DEATH.FM"
                                         "http://hi1.death.fm:8211" 1 url)
                                        ("Metal On: The Brutal"
                                         "http://65.60.19.42:8130" 1 url)
                                        ("H4XED"
                                         "http://sc-01.h4xed.us:7080" 1 url)
                                        ("DarkSoul7"
                                         "http://www.darksoul7.com:8000" 1 url)
                                        ("DjukRadio"
                                         "http://pri.kts-af.net/redir/index.pls?esid=05e8df87734bc7bbd19e0ba0ce299491&url_no=1&client_id=7&uid=68efed4d03ec7e45fd3978262c107180&clicksrc=xml" 1 url)
                                        ("Metal On: The Heavy"
                                         "http://188.138.19.96:8260" 1 url)
                                        ("James Bond"
                                         "http://www.somafm.com/secretagent.pls" 1 streamlist)))))
  ;; Type M-x emms-add-all to add all music in your ~/Music directory.

  (defun mars/emms-any-streams ()
    (interactive)
    (if (boundp 'anything-c-source-emms-streams)
        (anything-emms)
      (emms-streams)))

  (defun mars/safe-emms-start-stop ()
    (interactive)
    (flet ((last-non-empty-buffer (bufs) (car (remove-if '(lambda (x) (= (buffer-size x) 0)) bufs))))
      (condition-case nil
          (if (null (last-non-empty-buffer (emms-playlist-buffer-list)))
              (error "no playlist")       ; throw if no playlist
            (emms-pause))               ; try pause/resume otherwise (normal way)
        (error
         (progn
           (mapcar
            (lambda (elt)
              (kill-buffer elt))
            (emms-playlist-buffer-list))  ; remove buffers if #'EMMS-PAUSE creates one
           (emms-history-load)            ; try to restore history
           (let ((last-loaded-emms-playlist (last-non-empty-buffer (emms-playlist-buffer-list)))) ; fetch the last living non-empty buffer if multiple playlists saved in history
             (if (null last-loaded-emms-playlist)
                 (emms)                   ; emms if nothing in history
               (if (null emms-player-playing-p) ; if not playing (stop or paused)
                   (save-current-buffer
                     (set-buffer last-loaded-emms-playlist)
                     (condition-case nil
                         (emms-playlist-mode-play-smart) ; try a smart start of the current or next track
                       (error
                        (flet ((info-emms-unable-to-play (which-track)
                                                         (message "Information: [mars] emms: unable to play the %s track in the last playlist saved in history" which-track)))
                          (info-emms-unable-to-play "next")
                          (emms-playlist-mode-first)
                          (condition-case nil
                              (emms-playlist-mode-play-smart) ; try a smart start of the very first track
                            (error
                             (info-emms-unable-to-play "first")
                             (emms))))))) ; else open a default playlist to load
                 (when emms-player-paused-p
                   (condition-case nil
                       (emms-pause)
                     (error (emms)))))))))))))

(provide 'media)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; media.el ends here
