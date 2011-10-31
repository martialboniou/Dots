;;; pure-object.el --- 
;; 
;; Filename: pure-object.el
;; Description: Every OO programming languages
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Mar  5 23:58:09 2011 (+0100)
;; Version: 
;; Last-Updated: Mon Oct 31 11:53:57 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 12
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: gst / factorcode 
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

(provide 'one-language-spoken)
(require 'preamble)
(unintern 'one-language-spoken)

;;; GNU SMALLTALK
(require 'smalltalk-mode-init)

;;; FACTOR
(when (boundp 'factorcode-source-rep)
  (when (file-exists-p factorcode-source-rep)
    (load (apply 'concat
                 (nconc
                  (mapcar '(lambda (x) (file-name-as-directory x))
                          (list factorcode-source-rep
                                "misc"
                                "fuel"))
                  '("fu"))) t)))        ; fu.el autoloads

(provide 'pure-object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pure-object.el ends here
