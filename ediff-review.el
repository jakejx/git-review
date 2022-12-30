;;; ediff-review.el --- Review patch sets with Ediff -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Maintainer: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://sr.ht/~niklaseklund/ediff-review.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (project "0.9.3"))
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

;; This package provides an `ediff' derived interface for reviewing patch-sets.

;;; Code:

;;;; Requirements

(require 'ediff)
(require 'project)
(require 'tab-bar)



;;;; Variables

;;;; Customizable

(defcustom ediff-review-open-in-browser nil
  "Function to open a review location in the browser."
  :type 'symbol
  :group 'ediff-review)

(defcustom ediff-review-user nil
  "The name of the user."
  :type 'string
  :group 'ediff-review)

(defcustom ediff-review-comment-renderer-function
  #'ediff-review--comment-renderer
  "Function to render a comment."
  :type 'symbol
  :group 'ediff-review)

(defcustom ediff-review-comment-major-mode nil
  "Defines the major mode to use in comment mode."
  :type 'symbol
  :group 'ediff-review)

;;;; Public

(defvar ediff-review nil)
(defvar ediff-review-base-revision-buffer nil)
(defvar ediff-review-current-revision-buffer nil)

;;;; Private

(defvar ediff-review--current-comment nil)

;;;; Faces

(defgroup ediff-review-faces nil
  "Faces used by `ediff-review'."
  :group 'ediff-review
  :group 'faces)

(defface ediff-review-current-rebase-diff
  '((t :inherit ediff-current-diff-C))
  "Face used to highlight rebase diff.")

(defface ediff-review-fine-rebase-diff
  '((t :inherit ediff-fine-diff-C))
  "Face used to highlight rebase diff.")

;;;; Functions

(defun ediff-review-start-review ()
  "Start review."
  (let ((ediff-review-tab "Ediff-Review Review"))
    (when (not (member ediff-review-tab
                       (mapcar (lambda (tab)
                                 (alist-get 'name tab))
                               (tab-bar-tabs))))
      (tab-bar-new-tab)
      (tab-bar-rename-tab ediff-review-tab)
      (ediff-review-setup-project-file-review (seq-elt (ediff-review--files) 0))
      (ediff-review-file))))

(defun ediff-review-setup-project-file-review (file)
  "Setup `ediff-review' for project FILE review."
  (setf (alist-get 'current-file ediff-review) file)
  (let* ((default-directory (ediff-review--project-root))
         (file-info (ediff-review--file-info)))
    (ediff-review--setup-buffers)
    (let-alist file-info
      (if (string= (ediff-review--current-file) "COMMIT_MSG")
          (progn
            (when (string-equal "M" .type)
              (ediff-review--commit-message (ediff-review--base-revision)
                                            ediff-review-base-revision-buffer))
            (ediff-review--commit-message (ediff-review--current-revision)
                                          ediff-review-current-revision-buffer))
        (unless (string-equal "A" .type)
          (ediff-review--file-content (ediff-review--base-revision)
                                      .base-filename
                                      ediff-review-base-revision-buffer))
        (unless (string-equal "D" .type)
          (ediff-review--file-content (ediff-review--current-revision)
                                      file
                                      ediff-review-current-revision-buffer t))))))

(defun ediff-review-file ()
  "Review current file."
  (cl-letf* (((symbol-function #'ediff-mode) (lambda () (ediff-review-mode)))
             ((symbol-function #'ediff-set-keys) #'ignore)
             (default-directory (ediff-review--project-root)))
    (ediff-buffers ediff-review-base-revision-buffer ediff-review-current-revision-buffer)
    (ediff-review--restore-comment-overlays)
    (ediff-review--restore-buffer-location)))

(defun ediff-review-close-review-file ()
  "Close current review file."
  (cl-letf (((symbol-function #'y-or-n-p) (lambda (&rest _args) t))
            (buffers `(,ediff-buffer-A ,ediff-buffer-B)))
    (ediff-review--update-file (ediff-review--current-file) 'reviewed t)
    (ediff-review--store-buffer-locations)
    (ediff-review--store-progress)
    (call-interactively #'ediff-quit)
    (seq-do (lambda (it)
              (with-current-buffer it
                (set-buffer-modified-p nil)
                (kill-buffer)))
            buffers)
    (seq-do (lambda (it)
              (when (string-match (rx bol "*" (or "ediff" "Ediff" "Ediff-Review")) (buffer-name it))
                (kill-buffer it)))
            (buffer-list))))

(defun ediff-review--patchset-files ()
  "Determine which files to review."
  (let* ((files-in-latest-commit
          (split-string
           (string-trim
            (shell-command-to-string
             (format "git diff --name-status %s..%s"
                     (ediff-review--base-revision)
                     (ediff-review--current-revision))))
           "\n")))
    (setq files-in-latest-commit `(,(format "%s COMMIT_MSG" (if (let-alist ediff-review .multiple-patchsets) "M" "A")) ,@files-in-latest-commit))
    (setf (alist-get 'files ediff-review)
          (seq-map (lambda (it)
                     (let ((elements (split-string it)))
                       (pcase elements
                         (`(,type ,filename)
                          (cons filename `((current-filename . ,filename)
                                           (base-filename . ,filename)
                                           (type . ,type)
                                           (reviewed . nil))))
                         (`(,type ,base-filename ,filename)
                          (cons filename `((current-filename . ,filename)
                                           (base-filename . ,base-filename)
                                           (type . ,type)
                                           (reviewed . nil)))))))
                   files-in-latest-commit))))

(defun ediff-review-branch-modified-files (branch)
  "Return a list of modified files in BRANCH."
  (let ((lines (split-string
                (with-temp-buffer
                  (call-process-shell-command
                   (format "git show --pretty=\"\" --name-status %s" branch)
                   nil t)
                  (buffer-string))
                "\n")))
    (thread-last lines
                 (seq-map (lambda (it)
                            (pcase (split-string it)
                              (`(,_type ,name) name)
                              (`(,_type ,old-name ,new-name) `(,old-name ,new-name)))))
                 (flatten-list))))

(defun ediff-review-branch-review-files (branch-a branch-b)
  "Set list of files based on BRANCH-A and BRANCH-B."
  (ediff-review--patchset-files)
  ;; Filter review files to only be modified in latest commits on
  ;; branch-a and branch-b
  (let* ((files-union
          `("COMMIT_MSG" ,@(thread-last `(,branch-a ,branch-b)
                                        (seq-map #'ediff-review-branch-modified-files)
                                        (flatten-list))))
         (review-files
          (thread-last (ediff-review--files)
                       (seq-filter (lambda (it)
                                     (member it files-union)))
                       (seq-remove #'ediff-review-file-rebased-p))))
    ;; Update `ediff-review' with files
    (let ((updated-files (alist-get 'files ediff-review)))
      (seq-do (lambda (it)
                (let ((file (car it)))
                  (unless (member file review-files)
                    (setf updated-files (assoc-delete-all file updated-files #'equal)))))
              updated-files)
      (setf (alist-get 'files ediff-review) updated-files))))

(defun ediff-review-hunk-regions (base-revision current-revision base-file current-file)
  "Hunk regions for BASE-REVISION:BASE-FILE and CURRENT-REVISION:CURRENT-FILE."
  (let* ((diff-command (format "git diff %s:%s %s:%s --unified=0"
                               base-revision base-file current-revision current-file))
         (re-hunk-header (rx bol "@@ -"
                             (group (one-or-more digit) (zero-or-one "," (one-or-more digit)))
                             " +"
                             (group (one-or-more digit) (zero-or-one "," (one-or-more digit)))
                             " @@"))
         (hunk-regions-a)
         (hunk-regions-b))
    (with-temp-buffer
      (call-process-shell-command diff-command nil t)
      (goto-char (point-min))
      (while (search-forward-regexp re-hunk-header nil t)
        (let ((a-hunk (match-string 1))
              (b-hunk (match-string 2)))
          (when-let ((region (ediff-review--parse-review-hunk a-hunk)))
            (push region  hunk-regions-a))
          (when-let ((region (ediff-review--parse-review-hunk b-hunk)))
            (push region  hunk-regions-b)))))
    `((a . ,hunk-regions-a)
      (b . ,hunk-regions-b))))

(defun ediff-review-file-rebased-p (file)
  "Return t if FILE is changed due to a rebase."
  (unless (string= file "COMMIT_MSG")
    (let* ((file-info (ediff-review--file-info file))
           (base-current-regions (ediff-review-hunk-regions (ediff-review--base-revision)
                                                            (ediff-review--current-revision)
                                                            (let-alist file-info .base-filename)
                                                            file))
           (current-regions (ediff-review-hunk-regions (concat (ediff-review--current-revision) "~1")
                                                       (ediff-review--current-revision)
                                                       file
                                                       file)))
      (ediff-review--update-file file 'review-diff-regions base-current-regions)
      (ediff-review--update-file file 'current-revision-diff-regions current-regions)
      (not
       (ediff-review--file-differences-intersect-p base-current-regions
                                                   current-regions)))))

;;;; Commands

(defun ediff-review-quit ()
  "Quit `ediff-review' review."
  (interactive)
  (ediff-review-close-review-file)
  (tab-bar-close-tab))

(defun ediff-review-next-hunk ()
  "Go to next hunk."
  (interactive)
  (ediff-review--restore-overlays)
  (ediff-next-difference)
  (ediff-review---maybe-modify-overlays))

(defun ediff-review-previous-hunk ()
  "Go to previous hunk."
  (interactive)
  (ediff-review--restore-overlays)
  (ediff-previous-difference)
  (ediff-review---maybe-modify-overlays))

(defun ediff-review-next-file ()
  "Review next file."
  (interactive)
  (let* ((current-index (cl-position
                         (ediff-review--current-file) (ediff-review--files) :test #'equal))
         (next-index (1+ current-index)))
    (if (>= next-index (length (ediff-review--files)))
        (message "No next file")
      (setf (alist-get 'recent-file ediff-review) (ediff-review--current-file))
      (ediff-review-close-review-file)
      (ediff-review-setup-project-file-review (seq-elt (ediff-review--files) next-index))
      (ediff-review-file))))

(defun ediff-review-previous-file ()
  "Review previous file."
  (interactive)
  (let* ((current-index (cl-position
                         (ediff-review--current-file) (ediff-review--files) :test #'equal))
         (previous-index (1- current-index)))
    (if (< previous-index 0)
        (message "No previous file")
      (setf (alist-get 'recent-file ediff-review) (ediff-review--current-file))
      (ediff-review-close-review-file)
      (ediff-review-setup-project-file-review (seq-elt (ediff-review--files) previous-index))
      (ediff-review-file))))

(defun ediff-review-select-file ()
  "Select a file to review."
  (interactive)
  (when-let* ((candidates (ediff-review---file-candidates))
              (metadata `(metadata
                          (category . ediff-review-file)
                          (cycle-sort-function . identity)
                          (display-sort-function . identity)))
              (collection (lambda (string predicate action)
                            (if (eq action 'metadata)
                                metadata
                              (complete-with-action action candidates string predicate))))
              (candidate (completing-read "Select file: " collection nil t))
              (file (cdr (assoc candidate candidates))))
    (setf (alist-get 'recent-file ediff-review) (ediff-review--current-file))
    (ediff-review-close-review-file)
    (ediff-review-setup-project-file-review file)
    (ediff-review-file)))

(defun ediff-review-switch-to-most-recent-file ()
  "Switch to most recently reviewed file."
  (interactive)
  (when-let* ((recent-file (ediff-review--most-recent-file)))
    (setf (alist-get 'recent-file ediff-review) (ediff-review--current-file))
    (ediff-review-close-review-file)
    (ediff-review-setup-project-file-review recent-file)
    (ediff-review-file)))

;;;###autoload
(defun ediff-review-patchset ()
  "Review current patch-set."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (ediff-review--initialize-review (ediff-review--current-git-branch))
    (ediff-review--patchset-files)
    (ediff-review-start-review)))

;;;###autoload
(defun ediff-review-patchsets ()
  "Review the difference between two patch-sets."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (when-let ((base-revision (completing-read "Select base revision: "
                                               (ediff-review--other-git-branches)))
               (current-revision (ediff-review--current-git-branch)))
      (ediff-review--initialize-review current-revision
                                       base-revision)
      (when (and (ediff-review--base-revision)
                 (ediff-review--current-revision))
        (ediff-review-branch-review-files (ediff-review--base-revision)
                                          (ediff-review--current-revision))
        (ediff-review-start-review)))))

(defun ediff-review-browse-a ()
  "Open side a in browser."
  (interactive)
  (funcall ediff-review-open-in-browser 'a))

(defun ediff-review-browse-b ()
  "Open side b in browser."
  (interactive)
  (funcall ediff-review-open-in-browser 'b))

;;;; Support functions

(defun ediff-review--project-root ()
  "Return the project root of the current review."
  (let-alist ediff-review .project))

(defun ediff-review--current-revision ()
  "Return the current revision."
  (let-alist ediff-review .current-revision))

(defun ediff-review--base-revision ()
  "Return the base revision."
  (let-alist ediff-review
    (or .base-revision
        (concat .current-revision "~1"))))

(defun ediff-review--has-comments-p (file)
  "Return t if FILE has comments."
  (let-alist (ediff-review--file-info file)
    (not (null .comments))))

(defun ediff-review--multiple-patchsets-p ()
  "Return t if multiple patch-sets are being reviewed."
  (let-alist ediff-review .multiple-patchsets))

(defun ediff-review--files ()
  "Return a list of review files."
  (seq-map #'car (let-alist ediff-review .files)))

(defun ediff-review--current-file ()
  "Return the name of the current file being reviewed."
  (let-alist ediff-review .current-file))

(defun ediff-review--most-recent-file ()
  "Return the name of the most recently reviewed file."
  (let-alist ediff-review .recent-file))

(defun ediff-review--current-revision-diff-regions ()
  "Return diff regions for file from current revision."
  (let-alist (ediff-review--file-info)
    .current-revision-diff-regions))

(defun ediff-review--file-info (&optional file)
  "Info about FILE."
  (let-alist ediff-review
    (let ((file (or file .current-file)))
      (alist-get file .files nil nil #'equal))))

(defun ediff-review--update-file (file key value)
  "Update FILE with KEY and VALUE."
  (let* ((files (let-alist ediff-review .files))
         (file-info (alist-get file files nil nil #'equal)))
    (setf (alist-get key file-info) value)
    (setf (alist-get file files nil nil #'equal) file-info)
    (setf (alist-get 'files ediff-review) files)))

(defun ediff-review--update-comments (id &optional comment)
  "Update list of file comments with ID to COMMENT.

Unless COMMENT is nil, then delete ID."
  (let* ((file (let-alist ediff-review .current-file))
         (files (let-alist ediff-review .files))
         (file-info (alist-get file files nil nil #'equal))
         (comments (alist-get 'comments file-info)))
    (if comment
        (setf (alist-get id comments) comment)
      (setf comments (assoc-delete-all id comments)))
    (setf (alist-get 'comments file-info) comments)
    (setf (alist-get file files nil nil #'equal) file-info)
    (setf (alist-get 'files ediff-review) files)))

(defun ediff-review--store-buffer-locations ()
  "Store locations in review buffers for current review file."
  (ediff-review--update-file (ediff-review--current-file) 'buffer-location
                             `((a . ,(with-current-buffer ediff-review-base-revision-buffer (point)))
                               (b . ,(with-current-buffer ediff-review-current-revision-buffer (point))))))

(defun ediff-review--store-progress ()
  "Store progress percentage."
  (let ((progress (let-alist ediff-review
                    (/
                     (thread-last .files
                                  (seq-map #'cdr)
                                  (seq-filter (lambda (it)
                                                (let-alist it .reviewed)))
                                  (length)
                                  (float))
                     (length .files)))))
    (setf (alist-get 'progress ediff-review) progress)))

(defun ediff-review--initialize-review (current-revision &optional base-revision)
  "Initialize review of CURRENT-REVISION.

If a BASE-REVISION is provided it indicates multiple patch-sets reivew."
  (setq ediff-review `((base-revision . ,base-revision)
                       (current-revision . ,current-revision)
                       (multiple-patchsets . ,(not (null base-revision)))
                       (project . ,default-directory))))

(defun ediff-review--restore-buffer-location ()
  "Restore buffer location to nearest diff in revision buffer.

This is done for files that has already been reviewed before and where
there is a previous location to return to."
  (let-alist (ediff-review--file-info)
    (when .buffer-location
      (with-selected-window (get-buffer-window ediff-review-current-revision-buffer)
        (goto-char .buffer-location.b))
      (with-selected-window (ediff-review--control-window)
        (let ((last-command-event ?b))
          (ediff-jump-to-difference-at-point nil))))))

(defun ediff-review--control-window ()
  "Return window for variable `ediff-control-buffer'."
  (seq-find (lambda (it)
              (with-selected-window it
                ediff-control-buffer))
            (window-list)))

(defun ediff-review--file-content (revision file buffer &optional set-filename)
  "Populate BUFFER with FILE content from REVISION.

Optionally instruct function to SET-FILENAME."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (call-process-shell-command
       (format "git show %s:%s" revision file) nil t)
      (setq-local default-directory
                  (file-name-directory (expand-file-name file (ediff-review--project-root))))
      (when set-filename
        (setq-local buffer-file-name (expand-file-name file (ediff-review--project-root))))
      (ediff-review---enable-mode))
    (read-only-mode)))

(defun ediff-review--commit-message (revision buffer)
  "Populate BUFFER with commit message from REVISION."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (call-process-shell-command
       (format "git show --pretty=full --stat %s" revision) nil t))
    (read-only-mode)))

(defun ediff-review--setup-buffers ()
  "Setup buffers for `ediff-review'."
  (let ((file-info (ediff-review--file-info)))
    (let-alist file-info
      (setq ediff-review-base-revision-buffer
            (get-buffer-create (format "%s<%s>" (ediff-review--base-revision)
                                       (file-name-nondirectory .base-filename))))
      (setq ediff-review-current-revision-buffer
            (get-buffer-create (format "%s<%s>" (ediff-review--current-revision)
                                       (file-name-nondirectory (ediff-review--current-file))))))
    (with-current-buffer ediff-review-base-revision-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (with-current-buffer ediff-review-current-revision-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun ediff-review---enable-mode ()
  "Enable filename appropriate mode."
  (when-let* ((extension (file-name-extension (ediff-review--current-file) t))
              (mode (thread-last auto-mode-alist
                                 (seq-find (lambda (it)
                                             (string-match-p (car it) extension)))
                                 (cdr))))
    (funcall mode)
    (ediff-review-minor-mode-mode)))

(defun ediff-review--restore-overlays ()
  "Restore altered overlays."
  (when-let ((overlay ediff-current-diff-overlay-A))
    (with-current-buffer ediff-buffer-A
      (let ((overlays (overlays-in (overlay-start overlay)
                                   (overlay-end overlay))))
        (thread-last overlays
                     (seq-filter (lambda (it) (overlay-get it 'face)))
                     (seq-do (lambda (it)
                               (pcase (overlay-get it 'face)
                                 ('ediff-review-current-rebase-diff
                                  (overlay-put it 'face 'ediff-current-diff-A))
                                 ('ediff-review-fine-rebase-diff
                                  (overlay-put it 'face 'ediff-fine-diff-A)))))))))
  (when-let ((overlay ediff-current-diff-overlay-B))
    (with-current-buffer ediff-buffer-B
      (let ((overlays (overlays-in (overlay-start overlay)
                                   (overlay-end overlay))))
        (thread-last overlays
                     (seq-filter (lambda (it) (overlay-get it 'face)))
                     (seq-do (lambda (it)
                               (pcase (overlay-get it 'face)
                                 ('ediff-review-current-rebase-diff
                                  (overlay-put it 'face 'ediff-current-diff-B))
                                 ('ediff-review-fine-rebase-diff
                                  (overlay-put it 'face 'ediff-fine-diff-B))))))))))

(defun ediff-review---rebase-region-p ()
  "Return t if current diff is based on a rebase."
  (unless (string= "COMMIT_MSG" (ediff-review--current-file))
    (let* ((current-region-fun (lambda (buffer face)
                                 (with-current-buffer buffer
                                   (when-let ((diff-overlay
                                               (seq-find (lambda (it) (eq face (overlay-get it 'face)))
                                                         (overlays-at (point))))
                                              (start-line (line-number-at-pos (overlay-start diff-overlay)))
                                              (end-line (line-number-at-pos (overlay-end diff-overlay))))
                                     (list `(:begin ,start-line :end ,end-line))))))
           (file-regions
            `((a . ,(funcall current-region-fun ediff-buffer-A 'ediff-current-diff-A))
              (b . ,(funcall current-region-fun ediff-buffer-B 'ediff-current-diff-B)))))
      (not
       (ediff-review--file-differences-intersect-p file-regions
                                                   (ediff-review--current-revision-diff-regions))))))

(defun ediff-review---maybe-modify-overlays ()
  "Maybe modify overlays if current diff is due to a rebase."
  (when-let* ((is-rebase-diff (and
                               (ediff-review--multiple-patchsets-p)
                               (ediff-review---rebase-region-p)))
              (update-overlay-fun
               (lambda (side)
                 (let ((buffer (if (eq side 'b) ediff-buffer-B ediff-buffer-A))
                       (diff-face (if (eq side 'b) 'ediff-current-diff-B 'ediff-current-diff-A))
                       (diff-fine-face (if (eq side 'b) 'ediff-fine-diff-B 'ediff-fine-diff-A)))
                   (with-current-buffer buffer
                     (when-let* ((diff-overlay
                                  (seq-find (lambda (it) (eq diff-face (overlay-get it 'face)))
                                            (overlays-at (point)))))
                       (let* ((all-overlays-in-region (overlays-in (overlay-start diff-overlay) (overlay-end diff-overlay)))
                              (diff-fine-overlays
                               (seq-filter (lambda (it) (eq diff-fine-face (overlay-get it 'face)))
                                           all-overlays-in-region)))
                         (overlay-put diff-overlay 'face 'ediff-review-current-rebase-diff)
                         (seq-do (lambda (it)
                                   (overlay-put it 'face 'ediff-review-fine-rebase-diff))
                                 diff-fine-overlays))))))))
    (funcall update-overlay-fun 'a)
    (funcall update-overlay-fun 'b)))

(defun ediff-review---file-candidates ()
  "Return an alist of file candidates."
  (thread-last (ediff-review--files)
               (seq-map-indexed (lambda (it index)
                                  (let* ((file-info
                                          (let-alist ediff-review
                                            (alist-get it .files nil nil #'equal)))
                                         (status-str (pcase (let-alist file-info .type)
                                                       ("A" "ADDED")
                                                       ("D" "DELETED")
                                                       ("M" "MODIFIED")
                                                       (_ "RENAMED"))))
                                    `(,(format "%s %s %s" (1+ index) it status-str) . ,it))))))

(defun ediff-review--parse-review-hunk (hunk)
  "Parse HUNK into a property list."
  (pcase-let ((`(,start ,length)
               (seq-map #'string-to-number (string-split hunk ","))))
    (if (or (not length))
        `(:begin ,start :end ,start)
      (unless (= length 0)
        `(:begin ,start :end ,(+ start (1- length)))))))

(defun ediff-review--diff-regions-intersect-p (regions1 regions2)
  "Return t if REGIONS1 intersect REGIONS2."
  (seq-find (lambda (it)
              (let ((region1
                     (number-sequence (plist-get it :begin)
                                      (plist-get it :end)
                                      1)))
                (seq-find (lambda (it)
                            (let ((region2 (number-sequence (plist-get it :begin)
                                                            (plist-get it :end)
                                                            1)))
                              (seq-intersection region1 region2)))
                          regions2)))
            regions1))

(defun ediff-review--file-differences-intersect-p (file-diffs1 file-diffs2)
  "Return t if FILE-DIFFS1 intersects with FILE-DIFFS2."
  (or (ediff-review--diff-regions-intersect-p (alist-get 'a file-diffs1)
                                              (alist-get 'a file-diffs2))
      (ediff-review--diff-regions-intersect-p (alist-get 'b file-diffs1)
                                              (alist-get 'b file-diffs2))))

(defun ediff-review--current-git-branch ()
  "Return current branch name."
  (string-trim
   (with-temp-buffer
     (call-process-shell-command "git rev-parse --abbrev-ref HEAD" nil t)
     (buffer-string))))

(defun ediff-review--other-git-branches ()
  "Return list of local branch names, excluding the current branch."
  (let ((branches (split-string
                   (with-temp-buffer
                     (call-process-shell-command "git branch" nil t)
                     (buffer-string))
                   "\n" t)))
    (thread-last branches
                 (seq-remove (lambda (it) (string-prefix-p "*" it)))
                 (seq-map #'string-trim))))

;;;; Major modes

(defvar ediff-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "ba") #'ediff-review-browse-a)
    (define-key map (kbd "bb") #'ediff-review-browse-b)
    (define-key map (kbd "ga") #'ediff-jump-to-difference-at-point)
    (define-key map (kbd "gb") #'ediff-jump-to-difference-at-point)
    (define-key map (kbd "q") #'ediff-review-quit)
    (define-key map (kbd "s") #'ediff-review-select-file)
    (define-key map (kbd "n") #'ediff-review-next-hunk)
    (define-key map (kbd "p") #'ediff-review-previous-hunk)
    (define-key map (kbd "]") #'ediff-review-next-file)
    (define-key map (kbd "[") #'ediff-review-previous-file)
    (define-key map (kbd "^") #'ediff-review-switch-to-most-recent-file)
    map))

(define-derived-mode ediff-review-mode fundamental-mode "Ediff Review"
  (read-only-mode)
  (rename-buffer
   (format "*Ediff Review: [%s/%s]"
           (1+ (cl-position
                (ediff-review--current-file) (ediff-review--files) :test #'equal))
           (length (ediff-review--files)))))


;;;; WIP Comments

(defun ediff-review--comment-renderer (comment)
  "Default renderer for COMMENT."
  (let-alist comment .message))

(defun ediff-review--restore-comment-overlays ()
  "Restore comment overlays in the current file."
  (when (ediff-review--has-comments-p (ediff-review--current-file))
      (seq-do (lambda (it)
                (let ((comment (cdr it)))
                  (setq ediff-review--current-comment comment)
                  (let-alist ediff-review--current-comment
                    (setf (alist-get 'header-overlay ediff-review--current-comment) nil)
                    (setf (alist-get 'comment-overlay ediff-review--current-comment) nil)
                    (ediff-review--add-comment-overlay)
                    (ediff-review--update-comments .id ediff-review--current-comment))
                  (setq ediff-review--current-comment nil)))
              (let-alist (ediff-review--file-info) .comments))))

(defun ediff-review--add-comment-header-overlay ()
  "Add a comment header overlay."
  (let-alist ediff-review--current-comment
    (with-current-buffer (if (eq .side 'a) ediff-review-base-revision-buffer ediff-review-current-revision-buffer)
      (save-excursion
        (goto-char .location.start-point)
        (beginning-of-line)
        (let* ((ov (make-overlay (point) (point)))
               (time (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time)))
               (comment-message (funcall ediff-review-comment-renderer-function ediff-review--current-comment))
               (summary (seq-elt (split-string comment-message "\n") 0))
               (summary-str (truncate-string-to-width summary 30))
               (comment-header (concat ediff-review-user ": " summary-str (when (> (length summary) 30) "...") " " time "\n")))
          (when .header-overlay
            (delete-overlay .header-overlay))
          (setf (alist-get 'header-overlay ediff-review--current-comment) ov)
          (overlay-put ov 'ediff-review-comment .id)
          (overlay-put ov 'before-string (propertize comment-header 'face 'diff-function)))))))

(defun ediff-review--add-comment-overlay ()
  "Add a comment overlay."
  (let-alist ediff-review--current-comment
    (with-current-buffer (if (eq .side 'a) ediff-review-base-revision-buffer ediff-review-current-revision-buffer)
      (unless .comment-overlay
        (let* ((ov (or .comment-overlay (make-overlay .location.start-point .location.end-point))))
          (setf (alist-get 'comment-overlay ediff-review--current-comment) ov)
          (overlay-put ov 'ediff-review-comment .id)
          (overlay-put ov 'face 'ansi-color-fast-blink)))
      (ediff-review--add-comment-header-overlay))))

(defun ediff-review--create-comment ()
  "Create a new comment and return it."
  (let* ((id (intern (secure-hash 'md5 (number-to-string (time-to-seconds)))))
         (side (if (eq (current-buffer) ediff-review-base-revision-buffer) 'a 'b))
         (start-position (min (mark) (point)))
         (end-position (max (mark) (point)))
         (location
          `((start-line . ,(save-excursion (goto-char start-position) (current-line)))
            (start-column . ,(save-excursion (goto-char start-position) (current-column)))
            (start-point . ,start-position)
            (end-line . ,(save-excursion (goto-char end-position) (current-line)))
            (end-column . ,(save-excursion (goto-char end-position) (current-column)))
            (end-point . ,end-position))))
    `((id . ,id)
      (side . ,side)
      (published . ,nil)
      (location . ,location))))

(defun ediff-review-comment ()
  "Add or edit a comment."
  (interactive)
  (setq ediff-review--current-comment
        (or
         (let-alist (ediff-review--file-info)
           (thread-last .comments
                        (seq-find (lambda (it)
                                    (let-alist it
                                      (<= .location.start-point (point) .location.end-point))))
                        (cdr)))
         (ediff-review--create-comment)))
  (let* ((buffer (get-buffer-create "*ediff-review-comment*")))
    (display-buffer buffer '(display-buffer-in-side-window
                             (side . bottom)
                             (dedicated . t)))
    (with-current-buffer buffer
      (erase-buffer)
      (let-alist ediff-review--current-comment
        (when .message (insert .message)))

      (when ediff-review-comment-major-mode
        (funcall ediff-review-comment-major-mode))
      (ediff-review-comment-mode)
      (select-window (get-buffer-window (current-buffer)))
      (goto-char (point-max)))))

(defun ediff-review-kill-comment ()
  "Kill comment at point."
  (interactive)
  (when-let ((comment
              (let-alist (ediff-review--file-info)
                (thread-last .comments
                             (seq-find (lambda (it)
                                         (let-alist it
                                           (<= .location.start-point (point) .location.end-point))))
                             (cdr)))))
    (let-alist comment
      (delete-overlay .header-overlay)
      (delete-overlay .comment-overlay)
      (ediff-review--update-comments .id))))

(defun ediff-review-complete-comment ()
  "Complete the review comment."
  (interactive)
  (setf (alist-get 'message ediff-review--current-comment)
        (buffer-substring-no-properties (point-min) (point-max)))
  (ediff-review--add-comment-overlay)
  (ediff-review--update-comments (let-alist ediff-review--current-comment .id)
                                 ediff-review--current-comment)
  (setq ediff-review--current-comment nil)
  (quit-restore-window))

(defun ediff-review-quit-comment ()
  "Quit review comment."
  (interactive)
  (setq ediff-review--current-comment nil)
  (quit-restore-window (get-buffer-window (current-buffer)) 'kill))

(define-minor-mode ediff-review-comment-mode
  "Mode for `ediff-review' comment."
  :global nil
  :lighter " Ediff Review Comment"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'ediff-review-complete-comment)
            (define-key map (kbd "C-c C-k") #'ediff-review-quit-comment)
            map))

(define-minor-mode ediff-review-minor-mode-mode
  "Minor mode for `ediff-review'."
  :global nil
  :lighter "Ediff Review"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-'") #'ediff-review-comment)
            (define-key map (kbd "C-c C-k") #'ediff-review-kill-comment)
            map))

;;;; Sage WIP

(defun sage-publish-review-comments (review)
  "Publish all unpublished comments in REVIEW."
  (let-alist review
    (let ((comments (thread-last .files
                                 (seq-filter (lambda (file)
                                               (let-alist file .comments)))
                                 (seq-map (lambda (file)
                                            (let-alist file
                                              `(,(if (string= "COMMIT_MSG" .current-filename) "/COMMIT_MSG" .current-filename)
                                                .
                                                ,(thread-last .comments
                                                              (seq-map (lambda (it)
                                                                         (let* ((comment (cdr it))
                                                                                (range
                                                                                 (let-alist comment
                                                                                   `((start_line . ,(1+ .location.start-line))
                                                                                     (start_character . ,.location.start-column)
                                                                                     (end_line . ,(1+ .location.end-line))
                                                                                     (end_character . ,.location.end-column)))))
                                                                           `((side . ,(if (eq (let-alist comment .side) 'a) "PARENT" "REVISION"))
                                                                             (range . ,range)
                                                                             (unresolved . ,t)
                                                                             (message . ,(let-alist comment .message))))))
                                                              (vconcat)))))))))
      (sage--send-json-review `((comments . ,comments))))))

(sage-publish-review-comments ediff-review)

(defun sage--send-json-review (comments)
  "Send COMMENTS as a json message."
  (setq test-json-message (json-encode comments))
  (let ((temp-file (make-temp-file "sage"))
        (buffer (get-buffer-create "*sage-gerrit*")))
    (with-current-buffer buffer
      (erase-buffer))
    (with-temp-file temp-file
      (insert test-json-message))
    (call-process-shell-command
     (format "cat %s | ssh -p 29418 gerrit.cicd.autoheim.net gerrit review --json 330958,1" temp-file)
     nil buffer)
    (message "Temp file: %s" temp-file)))



(provide 'ediff-review)

;;; ediff-review.el ends here
