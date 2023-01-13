;;; ediff-review.el --- Review patch sets with Ediff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Maintainer: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://sr.ht/~niklaseklund/ediff-review
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
(require 'files)
(require 'project)
(require 'tab-bar)
(require 'subr-x)



;;;; Variables

;;;; Customizable

(defcustom ediff-review-user nil
  "The name of the user."
  :type 'string
  :group 'ediff-review)

(defcustom ediff-review-comment-major-mode #'text-mode
  "Defines the major mode to use in comment mode."
  :type 'symbol
  :group 'ediff-review)

(defcustom ediff-review-publish-function nil
  "Function that can publish a review."
  :type 'symbol
  :group 'ediff-review)

(defcustom ediff-review-database-dir user-emacs-directory
  "The directory to store the review database in."
  :type 'string
  :group 'ediff-review)

(defcustom ediff-review-comment-buffer-action
  '(display-buffer-at-bottom
    (window-height . 0.33))
  "The action used to display a comment."
  :group 'ediff-review
  :type 'sexp)

(defun ediff-review--annotation-file-name (entry)
  "Return file-name of ENTRY."
  (car entry))

(defcustom ediff-review-file-annotation
  '((:name type :function ediff-review--annotation-file-type :face 'font-lock-comment-face)
    (:name reviewed :function ediff-review--annotation-file-reviewed :face 'font-lock-string-face)
    (:name ignored :function ediff-review--annotation-file-ignored :face 'font-lock-string-face)
    (:name comments :function ediff-review--annotation-file-comments :face 'font-lock-string-face))
  "A list of annotations to display for a review file.

Each entry in the list is a property list with the following properties:
- :name
- :function
- :align
- :face
- :width"
  :group 'ediff-review
  :type '(repeat (plist :options ((:name symbol)
                                  (:function symbol)
                                  (:align symbol)
                                  (:face symbol)))))

(defcustom ediff-review-ignore-file-predicates nil
  "A list of predicates for determining if a file should be ignored."
  :group 'ediff-review
  :type '(repeat symbol))

(defcustom ediff-review-metadata-functions nil
  "A list of metadata functions which adds a metadata property to a file."
  :group 'ediff-review
  :type '(repeat symbol))

;;;; Public

(defvar ediff-review nil
  "Variable which holds all data related to the current review.")
(defvar ediff-review-change nil
  "Identifier for the current review.")
(defvar ediff-review-base-revision-buffer nil
  "Points to the buffer of the base revision.")
(defvar ediff-review-current-revision-buffer nil
  "Points to the buffer of the current revision.")

;;;; Private

(defvar ediff-review--current-comment nil)
(defvar ediff-review--reviews nil)
(defvar ediff-review--annotations nil)
(defvar ediff-review--annotation-widths nil)
(defvar ediff-review--annotation-config nil)

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

(defface ediff-review-comment-header
  '((t :inherit diff-function))
  "Face used to highlight comment header.")

;;;; Functions

(defun ediff-review-init-db ()
  "Initialize the review database."
  (unless ediff-review--reviews
    (let ((db (expand-file-name "ediff-review.db" ediff-review-database-dir)))
      (if (file-exists-p db)
          (setq ediff-review--reviews
                (with-temp-buffer
                  (insert-file-contents db)
                  (goto-char (point-min))
                  (read (current-buffer))))
        (make-empty-file db t)
        (setq ediff-review--reviews nil)))))

(defun ediff-review-update-db ()
  "Update the review database."
  (let ((db (expand-file-name "ediff-review.db" ediff-review-database-dir)))
    (with-temp-file db
      (prin1 ediff-review--reviews (current-buffer)))))

(defun ediff-review--update-review ()
  "Update reviews with review.

If review can already be found in an existing group update it
otherwise create it."
  (if-let ((grouped-reviews
            (alist-get ediff-review-change ediff-review--reviews nil nil #'equal)))
      (if (seq-find (lambda (it)
                      (and (equal (let-alist it .current-revision)
                                  (let-alist ediff-review .current-revision))
                           (equal (let-alist it .base-revision)
                                  (let-alist ediff-review .base-revision))))
                    grouped-reviews)
          (setf (alist-get ediff-review-change ediff-review--reviews nil nil #'equal)
                (thread-last grouped-reviews
                             (seq-map (lambda (it)
                                        (if (and (equal (let-alist it .current-revision)
                                                        (let-alist ediff-review .current-revision))
                                                 (equal (let-alist it .base-revision)
                                                        (let-alist ediff-review .base-revision)))
                                            ediff-review
                                          it)))
                             (vconcat)))
        (setf (alist-get ediff-review-change ediff-review--reviews nil nil #'equal)
              (vconcat (vconcat `(,ediff-review)) grouped-reviews)))
    (setf (alist-get ediff-review-change ediff-review--reviews nil nil #'equal)
          (vconcat `(,ediff-review)))))

(defun ediff-review-start-review ()
  "Start review."
  (let ((ediff-review-tab "Ediff-Review Review"))
    (when (not (member ediff-review-tab
                       (mapcar (lambda (tab)
                                 (alist-get 'name tab))
                               (tab-bar-tabs))))
      (tab-bar-new-tab)
      (tab-bar-rename-tab ediff-review-tab)
      (ediff-review-setup-project-file-review
       (if-let ((current-file (ediff-review--current-file)))
           current-file
         (seq-elt (ediff-review--files) 0)))
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
    (ediff-review--store-buffer-locations)
    (ediff-review--remove-file-comment-overlays)
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
  (ediff-review--update-review)
  (ediff-review-update-db)
  (setq ediff-review-change nil)
  (setq ediff-review nil)
  (tab-bar-close-tab))

(defun ediff-review-next-hunk ()
  "Go to next hunk."
  (interactive)
  (ediff-review--restore-overlays)
  (ediff-next-difference)
  (ediff-review---maybe-modify-overlays)
  (ediff-review--maybe-set-reviewed))

(defun ediff-review-previous-hunk ()
  "Go to previous hunk."
  (interactive)
  (ediff-review--restore-overlays)
  (ediff-previous-difference)
  (ediff-review---maybe-modify-overlays))

(defun ediff-review-next-file ()
  "Review next file."
  (interactive)
  (if-let ((next-file (ediff-review--next-file)))
      (progn
        (setf (alist-get 'recent-file ediff-review) (ediff-review--current-file))
        (ediff-review-close-review-file)
        (ediff-review-setup-project-file-review next-file)
        (ediff-review-file))
    (message "No next file")))

(defun ediff-review-previous-file ()
  "Review previous file."
  (interactive)
  (if-let ((previous-file (ediff-review--previous-file)))
      (progn
        (setf (alist-get 'recent-file ediff-review) (ediff-review--current-file))
        (ediff-review-close-review-file)
        (ediff-review-setup-project-file-review previous-file)
        (ediff-review-file))
    (message "No previous file")))

(defun ediff-review-select-file ()
  "Select a file to review."
  (interactive)
  (when-let ((candidates (seq-map (lambda (file)
                                    `(,file . ,(ediff-review--file-info file)))
                                  (ediff-review--files)))
             (file-info (ediff-review-completing-read (ediff-review--harmonize-candidate-lengths candidates)
                                                      "Select file: "
                                                      'ediff-review-file
                                                      ediff-review-file-annotation))
             (file (let-alist file-info .current-filename)))
    (setf (alist-get 'recent-file ediff-review) (ediff-review--current-file))
    (ediff-review-close-review-file)
    (ediff-review-setup-project-file-review file)
    (ediff-review-file)))

(defun ediff-review--harmonize-candidate-lengths (candidates)
  "Return CANDIDATES with same length."
  (let ((max-width (thread-last candidates
                                (seq-map #'car)
                                (seq-map #'length)
                                (seq-max)
                                (+ 3))))
    (seq-map (lambda (it)
               (pcase-let* ((`(,str . ,data) it)
                            (new-str (concat str (make-string (- max-width (length str)) ?\s))))
                 `(,new-str . ,data)))
             candidates)))

(defun ediff-review-completing-read (candidates prompt category annotation-config)
  "Select CANDIDATES from CATEGORY with and PROMPT."
  (when-let* ((ediff-review--annotation-config annotation-config)
              (ediff-review--annotations (ediff-review--annotations candidates))
              (ediff-review--annotation-widths (ediff-review--annotation-widths))
              (metadata `(metadata
                          (category . ,category)
                          (cycle-sort-function . identity)
                          (annotation-function . ediff-review--annotation-function)
                          (display-sort-function . identity)))
              (collection (lambda (string predicate action)
                            (if (eq action 'metadata)
                                metadata
                              (complete-with-action action candidates string predicate))))
              (candidate (completing-read prompt collection nil t)))
    (cdr (assoc candidate candidates))))

(defun ediff-review-switch-to-most-recent-file ()
  "Switch to most recently reviewed file."
  (interactive)
  (when-let* ((recent-file (ediff-review--most-recent-file)))
    (setf (alist-get 'recent-file ediff-review) (ediff-review--current-file))
    (ediff-review-close-review-file)
    (ediff-review-setup-project-file-review recent-file)
    (ediff-review-file)))

;;;###autoload
(defun ediff-review-patchset (select-branch)
  "Review current patch-set or SELECT-BRANCH."
  (interactive "P")
  (unless ediff-review-change
    (setq ediff-review-change
          (format "%s@%s"
                  (if-let ((branch (and select-branch
                                        (completing-read "Select base revision: "
                                                         (ediff-review--other-git-branches)))))
                      branch
                    (ediff-review--current-git-branch))
                  (project-root (project-current)))))
  (let* ((default-directory (project-root (project-current))))
    (ediff-review--initialize-review (ediff-review--current-git-branch))
    (ediff-review-start-review)))

;;;###autoload
(defun ediff-review-patchsets ()
  "Review the difference between two patch-sets."
  (interactive)
  (unless ediff-review-change
    (setq ediff-review-change
          (format "%s@%s"
                  (ediff-review--current-git-branch)
                  (project-root (project-current)))))
  (let* ((default-directory (project-root (project-current))))
    (when-let ((base-revision (completing-read "Select base revision: "
                                               (ediff-review--other-git-branches)))
               (current-revision (ediff-review--current-git-branch)))
      (ediff-review--initialize-review current-revision
                                       base-revision)
      (ediff-review-start-review))))

(defun ediff-review-publish-review ()
  "Publish review."
  (interactive)
  (if (functionp ediff-review-publish-function)
      (progn
        ;; TODO: For each comment set published if not already set, and if
        ;; so add a timestamp
        (funcall ediff-review-publish-function ediff-review))
    (message "No publish function definied")))

(defun ediff-review-jump-to-a ()
  "Jump to base revision buffer."
  (interactive)
  (select-window
   (get-buffer-window ediff-review-base-revision-buffer)))

(defun ediff-review-jump-to-b ()
  "Jump to current revision buffer."
  (interactive)
  (select-window
   (get-buffer-window ediff-review-current-revision-buffer)))

(defun ediff-review-jump-to-control ()
  "Jump to control buffer."
  (interactive)
  (select-window (ediff-review--control-window)))

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
    (display-buffer buffer ediff-review-comment-buffer-action)
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

(defun ediff-review--is-reviewed-p (file)
  "Return t if FILE is reviewed."
  (let-alist (ediff-review--file-info file)
    (not (null .reviewed))))

(defun ediff-review--ignore-file-p (file)
  "Return t if FILE should be ignored."
  (seq-find (lambda (predicate)
              (funcall predicate file))
            ediff-review-ignore-file-predicates))

(defun ediff-review--file-metadata (file)
  "Return metadata for FILE."
  (thread-last ediff-review-metadata-functions
               (seq-map (lambda (it)
                          (funcall it file)))
               (seq-remove #'null)))

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

(defun ediff-review--progress ()
  "Return review progress."
  (or (let-alist ediff-review .progress) 0.0))

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
                                                (let-alist it
                                                  (and .reviewed
                                                       (not .ignore)))))
                                  (length)
                                  (float))
                     (thread-last .files
                                  (seq-remove (lambda (it)
                                                (let-alist it .ignore)))
                                  (length))))))
    (setf (alist-get 'progress ediff-review) progress)))

(defun ediff-review--initialize-review (current-revision &optional base-revision)
  "Initialize review of CURRENT-REVISION.

If a BASE-REVISION is provided it indicates multiple patch-sets review."
  (if-let ((review (ediff-review--stored-review current-revision base-revision)))
      (setq ediff-review review)
    (let ((multiple-patchsets (not (null base-revision))))
      (setq ediff-review `((base-revision . ,base-revision)
                           (current-revision . ,current-revision)
                           (multiple-patchsets . ,multiple-patchsets)
                           (project . ,default-directory)))
      (ediff-review--add-files-to-review)
      (when multiple-patchsets
        (ediff-review--remove-rebased-files-from-review))
      (ediff-review--add-metadata-to-files)
      (ediff-review--add-ignore-tag-to-files))))

(defun ediff-review--add-ignore-tag-to-files ()
  "Add ignore tag to files that should be ignored in variable `ediff-review'."
  (seq-do (lambda (file)
            (when (ediff-review--ignore-file-p file)
              (ediff-review--update-file file 'ignore t)))
          (ediff-review--files)))

(defun ediff-review--add-metadata-to-files ()
  "Add metadata to files in variable `ediff-review'."
  (seq-do (lambda (file)
            (when-let ((metadata (ediff-review--file-metadata file)))
              (ediff-review--update-file file 'metadata metadata)))
          (ediff-review--files)))

(defun ediff-review--add-files-to-review ()
  "Add files to variable `ediff-review'."
  (let* ((files-in-latest-commit
          `(,(format "%s COMMIT_MSG"
                     (if (ediff-review--multiple-patchsets-p)
                         "M" "A"))
            ,@(split-string
               (string-trim
                (shell-command-to-string
                 (format "git diff --name-status %s..%s"
                         (ediff-review--base-revision)
                         (ediff-review--current-revision))))
               "\n"))))
    (setf (alist-get 'files ediff-review)
          (seq-map (lambda (it)
                     (let ((elements (split-string it)))
                       (pcase elements
                         (`(,type ,filename)
                          (cons filename `((current-filename . ,filename)
                                           (base-filename . ,filename)
                                           (type . ,type))))
                         (`(,type ,base-filename ,filename)
                          (cons filename `((current-filename . ,filename)
                                           (base-filename . ,base-filename)
                                           (type . ,type)))))))
                   files-in-latest-commit))))

(defun ediff-review--remove-rebased-files-from-review ()
  "Remove rebased files in variable `ediff-review'."
  (let* ((files-union
          `("COMMIT_MSG"
            ,@(thread-last `(,(ediff-review--base-revision)
                             ,(ediff-review--current-revision))
                           (seq-map #'ediff-review-branch-modified-files)
                           (flatten-list))))
         (review-files
          (thread-last (ediff-review--files)
                       (seq-filter (lambda (it)
                                     (member it files-union)))
                       ;; TODO(Niklas Eklund, 20230111): Look into the
                       ;; logic of this, it doesn't seems to always
                       ;; work
                       (seq-remove #'ediff-review-file-rebased-p))))
    ;; Update `ediff-review' with files
    (let ((updated-files (alist-get 'files ediff-review)))
      (seq-do (lambda (it)
                (let ((file (car it)))
                  (unless (member file review-files)
                    (setf updated-files (assoc-delete-all file updated-files #'equal)))))
              updated-files)
      (setf (alist-get 'files ediff-review) updated-files))))

(defun ediff-review--stored-review (current-revision base-revision)
  "Return a stored review of CURRENT-REVISION and BASE-REVISION."
  (when-let ((grouped-reviews (alist-get ediff-review-change ediff-review--reviews nil nil #'equal)))
    (seq-find (lambda (it)
                (and (equal (let-alist it .current-revision)
                            current-revision)
                     (equal (let-alist it .base-revision)
                            base-revision)))
              grouped-reviews)))

(defun ediff-review--restore-buffer-location ()
  "Restore buffer location to nearest diff in revision buffer.

This is done for files that has already been reviewed before and where
there is a previous location to return to."
  (let-alist (ediff-review--file-info)
    (when (and .buffer-location
               ; `ediff' complains when location is at start of buffer
               (> .buffer-location.b 1))
      (with-selected-window (get-buffer-window ediff-review-current-revision-buffer)
        (goto-char .buffer-location.b))
      (with-selected-window (ediff-review--control-window)
        (let ((last-command-event ?b))
          (ediff-jump-to-difference-at-point nil))
        (ediff-review---maybe-modify-overlays)))))

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
    (text-mode)
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
  (when-let* ((mode (thread-last auto-mode-alist
                                 (seq-find (lambda (it)
                                             (string-match-p (car it)
                                                             (ediff-review--current-file))))
                                 (cdr))))
    (funcall mode))
  (ediff-review-minor-mode-mode))

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

(defun ediff-review--maybe-set-reviewed ()
  "Set file to reviewed if the last diff has been reached."
  (when (and (not (ediff-review--is-reviewed-p (ediff-review--current-file)))
         (= (1+ ediff-current-difference) ediff-number-of-differences))
    (ediff-review--update-file (ediff-review--current-file) 'reviewed t)
    (ediff-review--store-progress)
    (with-current-buffer (window-buffer (ediff-review--control-window))
      (rename-buffer (ediff-review--review-buffer-name)))))

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

(defun ediff-review--review-buffer-name ()
  "Return the name of the review buffer."
  (let* ((review-files (thread-last (ediff-review--files)
                                    (seq-remove #'ediff-review--ignore-file-p)))
         (file-index
          (if (ediff-review--ignore-file-p (ediff-review--current-file))
              "_"
            (seq-position review-files (ediff-review--current-file))))
         (number-of-files (1- (seq-length review-files)))
         (progress (* (ediff-review--progress) 100)))
    (format "*Ediff Review: [%s/%s] %s%%*"
            file-index
            number-of-files
            progress)))

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
      (location . ,location))))

(defun ediff-review--remove-file-comment-overlays ()
  "Remove all overlays in the comments.

This is required since we can't serialize the overlays and store them
in the database.  Plus storing them doesn't make sense."
  (let* ((comments (let-alist (ediff-review--file-info) .comments)))
    (seq-do (lambda (it)
              (pcase-let ((`(,id . ,comment) it))
                (setf comment (assoc-delete-all 'header-overlay comment))
                (setf comment (assoc-delete-all 'comment-overlay comment))
                (ediff-review--update-comments id comment)))
            comments)))

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

(defun ediff-review--add-comment-header-overlay ()
  "Add a comment header overlay."
  (let-alist ediff-review--current-comment
    (with-current-buffer (if (eq .side 'a) ediff-review-base-revision-buffer ediff-review-current-revision-buffer)
      (save-excursion
        (goto-char .location.start-point)
        (beginning-of-line)
        (let* ((ov (make-overlay (point) (point)))
               (time (let-alist ediff-review--current-comment
                       (when .timestamp
                         (format-time-string "%Y-%m-%d %a %H:%M:%S" .timestamp))))
               (comment-message (let-alist ediff-review--current-comment .message))
               (summary (seq-elt (split-string comment-message "\n") 0))
               (summary-str (truncate-string-to-width summary 30))
               (comment-header
                (concat ediff-review-user ": " summary-str (when (> (length summary) 30) "...") (when time " " time) "\n")))
          (when .header-overlay
            (delete-overlay .header-overlay))
          (setf (alist-get 'header-overlay ediff-review--current-comment) ov)
          (overlay-put ov 'ediff-review-comment .id)
          (overlay-put ov 'before-string (propertize comment-header 'face 'ediff-review-comment-header)))))))

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

(defun ediff-review--next-file ()
  "Return next file."
  (thread-last (let-alist ediff-review .files)
               (seq-drop-while (lambda (it)
                                 (not (string= (car it)
                                               (ediff-review--current-file)))))
               (cdr)
               (seq-find (lambda (it)
                           (let-alist (cdr it)
                             (not .ignore))))
               (car)))

(defun ediff-review--previous-file ()
  "Return previous file."
  (thread-last (let-alist ediff-review .files)
               (seq-take-while (lambda (it)
                                 (not (string= (car it)
                                               (ediff-review--current-file)))))
               (nreverse)
               (seq-find (lambda (it)
                           (let-alist (cdr it)
                             (not .ignore))))
               (car)))

(defun ediff-review--annotations (candidates)
  "Return annotations of CANDIDATES."
  (thread-last candidates
               (seq-map (lambda (candidate)
                          (cons (car candidate)
                                (thread-last ediff-review--annotation-config
                                             (seq-map (lambda (config)
                                                        `(,(plist-get config :name) .
                                                          ,(funcall (plist-get config :function) candidate))))))))))

(defun ediff-review--annotation-widths ()
  "Return widths of annotations."
  (seq-map (lambda (config)
             `(,(plist-get config :name) .
               ,(thread-last ediff-review--annotations
                             (seq-map #'cdr)
                             (seq-map (lambda (it) (length (alist-get (plist-get config :name) it))))
                             (funcall (lambda (it)
                                        (if-let ((max-width (plist-get config :width)))
                                            (min (seq-max it) max-width)
                                          (seq-max it)))))))
           ediff-review--annotation-config))

(defun ediff-review--annotation-function (candidate)
  "Return annotation for CANDIDATE."
  (string-join
   (thread-last ediff-review--annotation-config
                (seq-map (lambda (config)
                           (setq test-annotation (alist-get candidate ediff-review--annotations nil nil #'equal))
                           (setq test-config config)
                           (when-let* ((annotation (alist-get candidate ediff-review--annotations nil nil #'equal))
                                       (padding 3)
                                       (str (alist-get (plist-get config :name) annotation))
                                       (width (alist-get (plist-get config :name) ediff-review--annotation-widths))
                                       (new-str
                                        (if-let* ((align (plist-get config :align))
                                                  (align-right (eq 'right align)))
                                            (concat (make-string (- width (length str)) ?\s)
                                                    str (make-string padding ?\s))
                                          (concat
                                           (truncate-string-to-width str width 0 ?\s)
                                           (make-string padding ?\s)))))
                             (if-let ((face (plist-get config :face)))
                                 (propertize new-str 'face face)
                               new-str)))))
   ""))

(defun ediff-review--annotation-file-type (entry)
  "Return ENTRY's type."
  (let-alist (cdr entry)
    (pcase .type
      ("A" "ADDED")
      ("D" "DELETED")
      ("M" "MODIFIED")
      (_ "RENAMED"))))

(defun ediff-review--annotation-file-reviewed (entry)
  "Return ENTRY's review status."
  (let-alist (cdr entry)
    (if .reviewed
        "REVIEWED"
      "")))

(defun ediff-review--annotation-file-ignored (entry)
  "Return ENTRY's ignore status."
  (let-alist (cdr entry)
    (if .ignore
        "IGNORED"
      "")))

(defun ediff-review--annotation-file-comments (entry)
  "Return ENTRY's comments status."
  (let-alist (cdr entry)
    (if .comments
        (format "COMMENTS(%s)" (length .comments))
      "")))

;;;; Major modes

(defvar ediff-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'ediff-review-jump-to-a)
    (define-key map (kbd "b") #'ediff-review-jump-to-b)
    (define-key map (kbd "ga") #'ediff-jump-to-difference-at-point)
    (define-key map (kbd "gb") #'ediff-jump-to-difference-at-point)
    (define-key map (kbd "q") #'ediff-review-quit)
    (define-key map (kbd "s") #'ediff-review-select-file)
    (define-key map (kbd "S") #'ediff-review-publish-review)
    (define-key map (kbd "n") #'ediff-review-next-hunk)
    (define-key map (kbd "p") #'ediff-review-previous-hunk)
    (define-key map (kbd "]") #'ediff-review-next-file)
    (define-key map (kbd "[") #'ediff-review-previous-file)
    (define-key map (kbd "^") #'ediff-review-switch-to-most-recent-file)
    map))

(define-derived-mode ediff-review-mode fundamental-mode "Ediff Review"
  (read-only-mode)
  (rename-buffer
   (ediff-review--review-buffer-name)))

;;;; Minor modes

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
            (define-key map (kbd "C-c C-c") #'ediff-review-jump-to-control)
            (define-key map (kbd "C-c C-'") #'ediff-review-comment)
            (define-key map (kbd "C-c C-k") #'ediff-review-kill-comment)
            map))

(provide 'ediff-review)

;;; ediff-review.el ends here
