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
(require 'tab-bar)



;;;; Variables

(defvar sage-review-files nil)
(defvar sage-review-files-metadata nil)
(defvar sage-review-file nil)
(defvar sage-project-root nil)
(defvar sage-review-temp-dir nil)
(defvar sage-review-file-a nil)
(defvar sage-review-file-b nil)
(defvar sage-review-setup-function nil)

;;;; Functions

(defun sage-start-review ()
  "Start review of variable `sage-review-files'."
  (let ((sage-review-tab "Sage Review"))
    (when (and sage-review-files
               sage-project-root
               (not (member sage-review-tab
                            (mapcar (lambda (tab)
                                      (alist-get 'name tab))
                                    (tab-bar-tabs)))))
      (tab-bar-new-tab)
      (tab-bar-rename-tab sage-review-tab)
      (funcall sage-review-setup-function (seq-elt sage-review-files 0))
      (sage-review-file))))

(defun sage-setup-project-file-review (file)
  "Setup `sage' for project FILE review."
  (let* ((default-directory sage-project-root)
         (file-metadata (cdr (assoc sage-review-file sage-review-files-metadata))))
    (setq sage-review-temp-dir (make-temp-file "sage-review-" t))
    (setq sage-review-file file)
    (cond ((string-equal "A" (plist-get file-metadata :type))
           (progn
             (setq sage-review-file-a (expand-file-name "null" sage-review-temp-dir))
             (with-temp-file sage-review-file-a)
             (setq sage-review-file-b sage-review-file)))
          ((string-equal "D" (plist-get file-metadata :type))
           (progn
             (setq sage-review-file-a (expand-file-name (file-name-nondirectory sage-review-file) sage-review-temp-dir))
             (with-temp-file sage-review-file-a
               (call-process-shell-command
                (format "git show HEAD~1:%s" sage-review-file) nil t))
             (setq sage-review-file-b (expand-file-name "null" sage-review-temp-dir))
             (with-temp-file sage-review-file-b)))
          ((string-prefix-p "R" (plist-get file-metadata :type))
           (progn
             (setq sage-review-file-a sage-review-file)
             (setq sage-review-file-b (expand-file-name (file-name-nondirectory sage-review-file) sage-review-temp-dir))
             (with-temp-file sage-review-file-b
               (call-process-shell-command
                (format "git show HEAD~1:%s" (plist-get file-metadata :base)) nil t))))
          (t
           (progn
             (setq sage-review-file-a (expand-file-name (file-name-nondirectory sage-review-file) sage-review-temp-dir))
             (with-temp-file sage-review-file-a
               (call-process-shell-command
                (format "git show HEAD~1:%s" sage-review-file) nil t))
             (setq sage-review-file-b sage-review-file))))))

(defun sage-review-file ()
  "Review file."
  (cl-letf* (((symbol-function #'ediff-mode) (lambda () (sage-review-mode)))
             ((symbol-function #'ediff-set-keys) #'ignore)
             (default-directory sage-project-root)
             (file-metadata (cdr (assoc sage-review-file sage-review-files-metadata)))
             (buffer-a (get-buffer-create (format "%s<HEAD~1>" (file-name-nondirectory (plist-get file-metadata :base)))))
             (buffer-b (get-buffer-create (format "%s<HEAD>" (file-name-nondirectory (plist-get file-metadata :name))))))
    (with-current-buffer buffer-a
        (insert-file-contents sage-review-file-a))
    (with-current-buffer buffer-b
        (insert-file-contents sage-review-file-b))
    (ediff-buffers buffer-a buffer-b)))

(defun sage-close-review-file ()
  "Close current review file."
  (cl-letf (((symbol-function #'y-or-n-p) (lambda (&rest _args) t))
            (buffers `(,ediff-buffer-A ,ediff-buffer-B)))
    (call-interactively #'ediff-quit)
    (delete-directory sage-review-temp-dir t)
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
                     (let ((elements (split-string it)))
                       (pcase elements
                         (`(,_type ,name) name)
                         (`(,_type ,_basename ,name) name))))
                   files-in-latest-commit))
    (setq sage-review-files-metadata
          (seq-map (lambda (it)
                     (let ((elements (split-string it)))
                       (pcase elements
                         (`(,type ,name) (cons name `(:type ,type :name ,name :base ,name)))
                         (`(,type ,basename ,name) (cons name `(:type ,type :name ,name :base ,basename))))))
                   files-in-latest-commit))))

;;;; Commands

(defun sage-review-toggle-highlight ()
  "Toggle syntax highlighting in review buffers."
  (interactive)
  (seq-do (lambda (buffer)
            (with-current-buffer buffer
              (if (eq major-mode 'fundamental-mode)
                  (when-let* ((extension (file-name-extension sage-review-file t))
                              (mode (thread-last auto-mode-alist
                                                 (seq-find (lambda (it)
                                                             (string-match-p (car it) extension)))
                                                 (cdr))))
                    (funcall mode))
                (fundamental-mode))))
          `(,ediff-buffer-A ,ediff-buffer-B)))

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
      (funcall sage-review-setup-function (seq-elt sage-review-files next-index))
      (sage-review-file)
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
      (funcall sage-review-setup-function (seq-elt sage-review-files previous-index))
      (sage-review-file)
      (message "Previous file"))))

(defun sage-review-select-file ()
  "Select a file to review."
  (interactive)
  (when-let* ((candidates (sage--review-file-candidates))
              (metadata `(metadata
                          (category . sage-file)
                          (cycle-sort-function . identity)
                          (display-sort-function . identity)))
              (collection (lambda (string predicate action)
                            (if (eq action 'metadata)
                                metadata
                              (complete-with-action action candidates string predicate))))
              (candidate (completing-read "Select file: " collection nil t))
              (file (cdr (assoc candidate candidates ))))
    (sage-close-review-file)
    (funcall sage-review-setup-function file)
    (sage-review-file)))

(defun sage-review-project ()
  "Review current project."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (setq sage-review-setup-function #'sage-setup-project-file-review)
    (setq sage-project-root default-directory)
    (sage-review-files)
    (sage-start-review)))

;;;; Support functions

(defun sage--review-file-candidates ()
  "Return an alist of review candidates."
  (thread-last sage-review-files
               (seq-map-indexed (lambda (it index)
                                  (let* ((status
                                          (cdr (assoc it sage-review-files-metadata)))
                                         (status-str (pcase (plist-get :type status)
                                                       ("A" "ADDED")
                                                       ("D" "DELETED")
                                                       ("M" "MODIFIED")
                                                       (_ "RENAMED"))))
                                    `(,(format "%s %s %s" (1+ index) it status-str) . ,it))))))

;;;; Major modes

(defvar sage-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'sage-review-quit)
    (define-key map (kbd "s") #'sage-review-select-file)
    (define-key map (kbd "t") #'sage-review-toggle-highlight)
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
