;;; git-review-consult.el --- Git-Review integration with Consult -*- lexical-binding: t -*-

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

;; This package integrates `git-review' with `consult'.

;;; Code:

;;;; Requirements

(require 'git-review)

(declare-function consult--multi "consult")



;;;; Variables

(defcustom git-review-file-consult-sources
  '(git-review-consult--source-files
    git-review-consult--source-ignored
    git-review-consult--source-comments
    git-review-consult--source-unreviewed)
  "Sources used by `git-review-consult-file'.

See `consult-multi' for a description of the source values."
  :type '(repeat symbol)
  :group 'git-review)

(defvar git-review-consult--source-files
  `(:category git-review-file
              :annotate git-review--annotation-function
              :action git-review-consult--decode-file-candidate
              :items
              ,(lambda ()
                 (thread-last git-review--candidates
                              (seq-remove #'git-review-consult--ignore-file-p)
                              (seq-map #'car))))
  "All `git-review' files as a source for `consult'.")

(defvar git-review-consult--source-ignored
  `(:narrow (?i . "Ignored")
            :hidden t
            :category git-review-file
            :annotate git-review--annotation-function
            :action git-review-consult--decode-file-candidate
            :items
            ,(lambda ()
               (thread-last git-review--candidates
                            (seq-filter #'git-review-consult--ignore-file-p)
                            (seq-map #'car))))
  "All `git-review' files as a source for `consult'.")

(defvar git-review-consult--source-unreviewed
  `(:narrow (?u . "Unreviewed")
            :hidden t
            :category git-review-file
            :annotate git-review--annotation-function
            :action git-review-consult--decode-file-candidate
            :items
            ,(lambda ()
               (thread-last git-review--candidates
                            (seq-remove #'git-review-consult--ignore-file-p)
                            (seq-remove
                             (lambda (x)
                               (let-alist (cdr x) .reviewed)))
                            (seq-map #'car))))
  "All unreviewed `git-review' files as a source for `consult'.")

(defvar git-review-consult--source-comments
  `(:narrow (?c . "Comments")
            :hidden t
            :category git-review-file
            :annotate git-review--annotation-function
            :action git-review-consult--decode-file-candidate
            :items
            ,(lambda ()
               (thread-last git-review--candidates
                            (seq-remove #'git-review-consult--ignore-file-p)
                            (seq-filter
                             (lambda (x)
                               (let-alist (cdr x) .comments)))
                            (seq-map #'car))))
  "All `git-review' files with comments as a source for `consult'.")

;;;; Functions

;;;; Commands

;;;###autoload
(defun git-review-consult-file ()
  "Enhanced `git-review-select-file' command."
  (interactive)
  (unless (require 'consult nil 'noerror)
    (error "Install Consult to use `git-review-consult-files'"))
  (let* ((git-review--candidates (seq-map (lambda (file)
                                              `(,file . ,(git-review--file-info file)))
                                            (git-review--files)))
         (git-review--annotation-config git-review-file-annotation)
         (git-review--annotations (git-review--annotations git-review--candidates))
         (git-review--annotation-widths (git-review--annotation-widths)))
    (consult--multi git-review-file-consult-sources
                    :prompt "Select file: "
                    :require-match t
                    :sort nil)))

;;;; Support functions

(defun git-review-consult--ignore-file-p (file)
  "Return t if FILE should be ignored."
  (let-alist file .ignore))

(defun git-review-consult--decode-file-candidate (candidate)
  "Return change matching CANDIDATE."
  (git-review--switch-file candidate))

(provide 'git-review-consult)

;;; git-review-consult.el ends here
