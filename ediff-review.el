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

;;;; Public

(defvar ediff-review nil)

(defvar ediff-review-files nil)
(defvar ediff-review-files-metadata nil)
(defvar ediff-review-setup-function nil)
(defvar ediff-review-base-revision-buffer nil)
(defvar ediff-review-current-revision-buffer nil)

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
  "Start review of variable `ediff-review-files'."
  (let ((ediff-review-tab "Ediff-Review Review"))
    (when (and ediff-review-files
               (not (member ediff-review-tab
                            (mapcar (lambda (tab)
                                      (alist-get 'name tab))
                                    (tab-bar-tabs)))))
      (tab-bar-new-tab)
      (tab-bar-rename-tab ediff-review-tab)
      (funcall ediff-review-setup-function (seq-elt ediff-review-files 0))
      (ediff-review-file))))

(defun ediff-review-setup-project-file-review (file)
  "Setup `ediff-review' for project FILE review."
  (setf (alist-get 'current-file ediff-review) file)
  (let* ((default-directory (ediff-review--project-root))
         (file-info (ediff-review--current-file-info)))
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
    (ediff-buffers ediff-review-base-revision-buffer ediff-review-current-revision-buffer)))

(defun ediff-review-close-review-file ()
  "Close current review file."
  (cl-letf (((symbol-function #'y-or-n-p) (lambda (&rest _args) t))
            (buffers `(,ediff-buffer-A ,ediff-buffer-B)))
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

(defun ediff-review-files ()
  "Set the files to review."
  (let* ((files-in-latest-commit
          (split-string
           (string-trim
            (shell-command-to-string
             (format "git diff --name-status %s..%s"
                     (ediff-review--base-revision)
                     (ediff-review--current-revision))))
           "\n")))
    (setq files-in-latest-commit `(,(format "%s COMMIT_MSG" (if (let-alist ediff-review .multiple-patchsets) "M" "A")) ,@files-in-latest-commit))
    (setq ediff-review-files
          (seq-map (lambda (it)
                     (let ((elements (split-string it)))
                       (pcase elements
                         (`(,_type ,name) name)
                         (`(,_type ,_basename ,name) name))))
                   files-in-latest-commit))
    (setq ediff-review-files-metadata
          (seq-map (lambda (it)
                     (let ((elements (split-string it)))
                       (pcase elements
                         (`(,type ,name) (cons name `(:type ,type :current-revision-name ,name :base-revision-name ,name)))
                         (`(,type ,basename ,name) (cons name `(:type ,type :current-revision-name ,name :base-revision-name ,basename))))))
                   files-in-latest-commit))
    ;; Update `ediff-review' with files
    (setf (alist-get 'files ediff-review)
          (thread-last ediff-review-files
                       (seq-map (lambda (file)
                                  (let ((metadata (cdr (assoc file ediff-review-files-metadata))))
                                    metadata
                                    (cons file `((current-filename . ,(plist-get metadata :current-revision-name))
                                                 (base-filename . ,(plist-get metadata :base-revision-name))
                                                 (type . ,(plist-get metadata :type))
                                                 (reviewed . ,nil))))))))))

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
  (ediff-review-files)
  ;; Filter review files to only be modified in latest commits on
  ;; branch-a and branch-b
  (let* ((files-union
          (thread-last `(,branch-a ,branch-b)
                       (seq-map #'ediff-review-branch-modified-files)
                       (flatten-list))))
    (setq files-union `("COMMIT_MSG" ,@files-union))
    (setq ediff-review-files
          (thread-last ediff-review-files
                       (seq-filter (lambda (it)
                                     (member it files-union)))
                       (seq-remove #'ediff-review-file-rebased-p)))
    ;; Update `ediff-review' with files
    (let ((updated-files (alist-get 'files ediff-review)))
      (seq-do (lambda (it)
                 (let ((file (car it)))
                   (unless (member file ediff-review-files)
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
    (let* ((file-metadata (cdr (assoc file ediff-review-files-metadata)))
           (base-current-regions (ediff-review-hunk-regions (ediff-review--base-revision)
                                                            (ediff-review--current-revision)
                                                            (plist-get file-metadata :base-revision-name)
                                                            file))
           (current-regions (ediff-review-hunk-regions (concat (ediff-review--current-revision) "~1")
                                                       (ediff-review--current-revision)
                                                       file
                                                       file))
           (files (let-alist ediff-review .files))
           (file-info (alist-get file files)))
      (setf (alist-get 'review-diff-regions file-info) base-current-regions)
      (setf (alist-get 'current-revision-diff-regions file-info) current-regions)
      (setf (alist-get file files nil nil #'equal) file-info)
      (setf (alist-get 'files ediff-review) files)
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
                         (ediff-review--current-file) ediff-review-files :test #'equal))
         (next-index (1+ current-index)))
    (if (>= next-index (length ediff-review-files))
        (message "No next file")
      (setf (alist-get 'recent-file ediff-review) (ediff-review--current-file))
      (ediff-review-close-review-file)
      (funcall ediff-review-setup-function (seq-elt ediff-review-files next-index))
      (ediff-review-file))))

(defun ediff-review-previous-file ()
  "Review previous file."
  (interactive)
  (let* ((current-index (cl-position
                         (ediff-review--current-file) ediff-review-files :test #'equal))
         (previous-index (1- current-index)))
    (if (< previous-index 0)
        (message "No previous file")
      (setf (alist-get 'recent-file ediff-review) (ediff-review--current-file))
      (ediff-review-close-review-file)
      (funcall ediff-review-setup-function (seq-elt ediff-review-files previous-index))
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
    (funcall ediff-review-setup-function file)
    (ediff-review-file)))

;;;###autoload
(defun ediff-review-patchset ()
  "Review current patch-set."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (setq ediff-review-setup-function #'ediff-review-setup-project-file-review)
    (ediff-review--initialize-review (ediff-review--current-git-branch))
    (ediff-review-files)
    (ediff-review-start-review)))

;;;###autoload
(defun ediff-review-patchsets ()
  "Review the difference between two patch-sets."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (setq ediff-review-setup-function #'ediff-review-setup-project-file-review)
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

(defun ediff-review--multiple-patchsets-p ()
  "Return t if multiple patch-sets are being reviewed."
  (let-alist ediff-review .multiple-patchsets))

(defun ediff-review--current-file ()
  "Return the name of the current file being reviewed."
  (let-alist ediff-review .current-file))

(defun ediff-review--current-revision-diff-regions ()
  "Return diff regions for file from current revision."
  (let-alist (ediff-review--current-file-info)
    .current-revision-diff-regions))

(defun ediff-review--current-file-info ()
  "Info about current file."
  (let-alist ediff-review
    (alist-get .current-file .files nil nil #'equal)))

(defun ediff-review--initialize-review (current-revision &optional base-revision)
  "Initialize review of CURRENT-REVISION.

If a BASE-REVISION is provided it indicates multiple patch-sets reivew."
  (setq ediff-review `((base-revision . ,base-revision)
                       (current-revision . ,current-revision)
                       (multiple-patchsets . ,(not (null base-revision)))
                       (project . ,default-directory))))

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
  (let ((file-info (ediff-review--current-file-info)))
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
    (funcall mode)))

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
  (thread-last ediff-review-files
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
    (define-key map (kbd "b a") #'ediff-review-browse-a)
    (define-key map (kbd "b b") #'ediff-review-browse-b)
    (define-key map (kbd "q") #'ediff-review-quit)
    (define-key map (kbd "s") #'ediff-review-select-file)
    (define-key map (kbd "n") #'ediff-review-next-hunk)
    (define-key map (kbd "p") #'ediff-review-previous-hunk)
    (define-key map (kbd "]") #'ediff-review-next-file)
    (define-key map (kbd "[") #'ediff-review-previous-file)
    map))

(define-derived-mode ediff-review-mode fundamental-mode "Ediff Review"
  (read-only-mode)
  (rename-buffer
   (format "*Ediff Review: [%s/%s]"
           (1+ (cl-position
                (ediff-review--current-file) ediff-review-files :test #'equal))
           (length ediff-review-files))))

(provide 'ediff-review)

;;; ediff-review.el ends here
