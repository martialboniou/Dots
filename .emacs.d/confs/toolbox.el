;;; toolbox.el --- 
;; 
;; Filename: toolbox.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Mon Mar 14 20:02:00 2011 (+0100)
;; Version: 
;; Last-Updated: Wed Mar 23 12:47:45 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 7
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: keats /
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

;;; KEATS
(require 'keats)
(require 'keats-interactive)
(custom-set-variables '(keats-file (expand-file-name
                                    (concat (file-name-as-directory mars/local-root-dir)
                                            (file-name-as-directory mars/personal-data)
                                            "cheatsheets"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toolbox.el ends here
