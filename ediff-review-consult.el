;;; ediff-review-consult.el --- Ediff-Review integration with Consult -*- lexical-binding: t -*-

;; Copyright (C) 2023 Niklas Eklund

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package integrates `ediff-review' with `consult'.

;;; Code:

;;;; Requirements

(require 'ediff-review)

(declare-function consult--multi "consult")



;;;; Variables

(defcustom ediff-review-file-consult-sources
  '(ediff-review-consult--source-files
    ediff-review-consult--source-ignored
    ediff-review-consult--source-comments
    ediff-review-consult--source-unreviewed)
  "Sources used by `ediff-review-consult-file'.

See `consult-multi' for a description of the source values."
  :type '(repeat symbol)
  :group 'ediff-review)

(defvar ediff-review-consult--source-files
  `(:category ediff-review-file
              :annotate ediff-review--annotation-function
              :action ediff-review-consult--decode-file-candidate
              :items
              ,(lambda ()
                 (seq-map #'car ediff-review--candidates)))
  "All `ediff-review' files as a source for `consult'.")

(defvar ediff-review-consult--source-ignored
  `(:narrow (?i . "Ignored")
            :hidden t
            :category ediff-review-file
            :annotate ediff-review--annotation-function
            :action ediff-review-consult--decode-file-candidate
            :items
            ,(lambda ()
               (thread-last ediff-review--candidates
                            (seq-filter
                             (lambda (x)
                               (let-alist (cdr x) .ignore)))
                            (seq-map #'car))))
  "All `ediff-review' files as a source for `consult'.")

(defvar ediff-review-consult--source-unreviewed
  `(:narrow (?u . "Unreviewed")
            :hidden t
            :category ediff-review-file
            :annotate ediff-review--annotation-function
            :action ediff-review-consult--decode-file-candidate
            :items
            ,(lambda ()
               (thread-last ediff-review--candidates
                            (seq-remove
                             (lambda (x)
                               (let-alist (cdr x) .reviewed)))
                            (seq-map #'car))))
  "All unreviewed `ediff-review' files as a source for `consult'.")

(defvar ediff-review-consult--source-comments
  `(:narrow (?c . "Comments")
            :hidden t
            :category ediff-review-file
            :annotate ediff-review--annotation-function
            :action ediff-review-consult--decode-file-candidate
            :items
            ,(lambda ()
               (thread-last ediff-review--candidates
                            (seq-filter
                             (lambda (x)
                               (let-alist (cdr x) .comments)))
                            (seq-map #'car))))
  "All `ediff-review' files with comments as a source for `consult'.")

;;;; Functions

;;;; Commands

;;;###autoload
(defun ediff-review-consult-file ()
  "Enhanced `ediff-review-select-file' command."
  (interactive)
  (unless (require 'consult nil 'noerror)
    (error "Install Consult to use `ediff-review-consult-files'"))
  (let* ((ediff-review--candidates (seq-map (lambda (file)
                                              `(,file . ,(ediff-review--file-info file)))
                                            (ediff-review--files)))
         (ediff-review--annotation-config ediff-review-file-annotation)
         (ediff-review--annotations (ediff-review--annotations ediff-review--candidates))
         (ediff-review--annotation-widths (ediff-review--annotation-widths)))
    (consult--multi ediff-review-file-consult-sources
                    :prompt "Select file: "
                    :require-match t
                    :sort nil)))

;;;; Support functions

(defun ediff-review-consult--decode-file-candidate (candidate)
  "Return change matching CANDIDATE."
  (ediff-review--switch-file candidate))

(provide 'ediff-review-consult)

;;; ediff-review-consult.el ends here
