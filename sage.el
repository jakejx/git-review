;;; sage.el --- Second Attempt at Gerrit for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Maintainer: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://sr.ht/~niklaseklund/sage.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (project "0.8.1"))
;; Keywords: convenience tools

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

;; Second Attempt at Gerrit for Emacs.  This package relies on
;; available SSH commands to communicate with Gerrit.  This interface
;; is more limited than the REST API.

;;; Code:

;;;; Requirements

(require 'ediff)
(require 'vc)
(require 'tab-bar)

;;;; Variables

(defvar sage-review-files nil)
(defvar sage-review-files-metadata nil)
(defvar sage-review-file nil)
(defvar sage-project-root nil)

;;;; Functions

(defun sage-start-review ()
  "Start review of `sage-review-files'."
  (let ((sage-review-tab "Sage Review"))
    (when (and sage-review-files
               sage-project-root
               (not (member sage-review-tab
                            (mapcar (lambda (tab)
                                      (alist-get 'name tab))
                                    (tab-bar-tabs)))))
      (tab-bar-new-tab)
      (tab-bar-rename-tab sage-review-tab)
      (sage-review-file (seq-elt sage-review-files 0)))))

(defun sage-review-file (file)
  "Review FILE."
  (cl-letf* (((symbol-function #'ediff-mode) (lambda () (sage-review-mode)))
             ((symbol-function #'ediff-set-keys) #'ignore)
             (default-directory sage-project-root))
    (setq sage-review-file file)
    (vc-version-ediff `(,sage-review-file) "HEAD~1" "HEAD")))

(defun sage-close-review-file ()
  "Close current review file."
  (cl-letf (((symbol-function #'y-or-n-p) (lambda (&rest _args) t))
            (buffers `(,ediff-buffer-A ,ediff-buffer-B)))
    (call-interactively #'ediff-quit)
    (seq-do #'kill-buffer buffers)
    (seq-do (lambda (it)
              (when (string-match (rx bol "*" (or "ediff" "Ediff" "Sage")) (buffer-name it))
                (kill-buffer it)))
            (buffer-list))))

(defun sage-review-files ()
  "Set the files to review."
  (let* ((files-in-latest-commit
          (split-string
           (string-trim
            (shell-command-to-string "git show --pretty=\"\" --name-status HEAD"))
           "\n")))
    (setq sage-review-files
          (seq-map (lambda (it)
                     (cadr (split-string it)))
                   files-in-latest-commit))
    (setq sage-review-files-metadata
          (seq-map (lambda (it)
                     (let ((elements (split-string it)))
                       `(,(cadr elements) . ,(car elements))))
                   files-in-latest-commit))))

;;;; Commands

(defun sage-review-quit ()
  "Quit `sage' review."
  (interactive)
  (sage-close-review-file)
  (tab-bar-close-tab))

(defun sage-review-next-file ()
  "Review next file."
  (interactive)
  (let* ((current-index (cl-position
                         sage-review-file sage-review-files :test #'equal))
         (next-index (1+ current-index)))
    (if (>= next-index (length sage-review-files))
        (message "No next file")
      (sage-close-review-file)
      (sage-review-file (seq-elt sage-review-files next-index))
      (message "Next file"))))

(defun sage-review-previous-file ()
  "Review previous file."
  (interactive)
  (let* ((current-index (cl-position
                         sage-review-file sage-review-files :test #'equal))
         (previous-index (1- current-index)))
    (if (< previous-index 0)
        (message "No previous file")
      (sage-close-review-file)
      (sage-review-file (seq-elt sage-review-files previous-index))
      (message "Previous file"))))

(defun sage-review-select-file ()
  "Select a file to review."
  (interactive)
  (when-let ((file (completing-read "Select file: " sage-review-files)))
    (sage-close-review-file)
    (sage-review-file file)))

(defun sage-review-project ()
  "Review current project."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (setq sage-project-root default-directory)
    (sage-review-files)
    (sage-start-review)))

;;;; Major modes

(defvar sage-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'sage-review-quit)
    (define-key map (kbd "o") #'sage-review-select-file)
    (define-key map (kbd "n") #'ediff-next-difference)
    (define-key map (kbd "p") #'ediff-previous-difference)
    (define-key map (kbd "]") #'sage-review-next-file)
    (define-key map (kbd "[") #'sage-review-previous-file)
    map))

(define-derived-mode sage-review-mode fundamental-mode "Sage Review"
  (read-only-mode)
  (rename-buffer
   (format "*Sage Review: [%s/%s]"
           (1+ (cl-position
                sage-review-file sage-review-files :test #'equal))
           (length sage-review-files))))

(provide 'sage)

;;; sage.el ends here
