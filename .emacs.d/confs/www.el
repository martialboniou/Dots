;;; www.el ---
;;
;; Filename: www.el
;; Description:
;; Author: Martial Boniou
;; Maintainer:
;; Created: Wed Feb 23 13:08:32 2011 (+0100)
;; Version:
;; Last-Updated: Fri Mar 11 23:59:39 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 22
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: W3M / newsticker
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

;;; W3M
(setq mars/w3m-exists t)

(eval-after-load "w3m"
  '(progn
     (setq w3m-command (or (executable-find "w3m")
                           "/opt/local/bin/w3m")
           w3m-home-page "http://www.google.fr"
           w3m-cookie-accept-bad-cookies t
           w3m-toggle-inline-image t
           w3m-cookie-file (concat (file-name-as-directory mars/local-root-dir)
                                   (file-name-as-directory mars/personal-data)
                                   "w3m-cookie"))))

;;; NEWSTICKER
(when (require 'newsticker nil t)
  (require 'w3m)
  (setq newsticker-html-rendererer 'w3m-region
        ;; browse-url-browser-function 'w3m-browse-url ; w3m as browser
        newsticker-automatically-mark-items-as-old t
        newsticker-automatically-mark-visited-items-as-old t
        newsticker-retrieval-method (quote extern)
        newsticker-wget-arguments (quote ("-q" "-O" "-" "--user-agent" "testing"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; www.el ends here
