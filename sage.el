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

;; TODO:
;; - refer to base-revision and current-revision
;; - `ediff-review-file-base-revision' and `ediff-review-file-current-revision'
;; - skip generation of real files, use buffers instead
;; - rewrite package descript
;; - rename from `sage' to `ediff-review'



;;;; Variables

(defcustom sage-review-open-in-browser nil
  "Function to open a review location in the browser."
  :type 'symbol
  :group 'sage)

(defvar sage-review-files nil)
(defvar sage-review-files-metadata nil)
(defvar sage-review-file nil)
(defvar sage-project-root nil)
(defvar sage-review-temp-dir nil)
(defvar sage-review-file-a nil)
(defvar sage-review-file-b nil)
(defvar sage-review-setup-function nil)
(defvar sage-review-base nil)
(defvar sage-review-commit nil)
(defvar sage-review-base-revision-buffer nil)
(defvar sage-review-current-revision-buffer nil)

(defvar sage--review-regions nil)

;;;; Faces

(defgroup sage-faces nil
  "Faces used by `sage'."
  :group 'sage
  :group 'faces)

(defface sage-current-rebase-diff
  '((t :inherit ediff-current-diff-C))
  "Face used to highlight rebase diff.")

(defface sage-fine-rebase-diff
  '((t :inherit ediff-fine-diff-C))
  "Face used to highlight rebase diff.")

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
  (setq sage-review-file file)
  (let* ((default-directory sage-project-root)
         (file-metadata (cdr (assoc sage-review-file sage-review-files-metadata))))
    (setq sage--review-regions (sage-review-hunk-regions
                                (concat sage-review-commit "~1") sage-review-commit
                                (plist-get file-metadata :name) (plist-get file-metadata :name)))

    ;; Setup buffers
    (setq sage-review-base-revision-buffer
          (get-buffer-create (format "%s<%s>" sage-review-base
                                     (file-name-nondirectory (plist-get file-metadata :base)))))
    (setq sage-review-current-revision-buffer
          (get-buffer-create (format "%s<%s>" sage-review-commit
                                     (file-name-nondirectory (plist-get file-metadata :name)))))
    (with-current-buffer sage-review-base-revision-buffer (erase-buffer))
    (with-current-buffer sage-review-current-revision-buffer (erase-buffer))

    (if (string= sage-review-file "COMMIT_MSG")
        (progn
          (when (string-equal "M" (plist-get file-metadata :type))
            (with-current-buffer sage-review-base-revision-buffer
              (call-process-shell-command
               (format "git show --pretty=full --stat %s" sage-review-base) nil t)))
          (with-current-buffer sage-review-current-revision-buffer
            (call-process-shell-command
             (format "git show --pretty=full --stat %s" sage-review-commit) nil t)))
      (cond ((string-equal "A" (plist-get file-metadata :type))
             (with-current-buffer sage-review-current-revision-buffer
               (call-process-shell-command
                (format "git show %s:%s" sage-review-commit (plist-get file-metadata :name)) nil t)))
            ((string-equal "D" (plist-get file-metadata :type))
             (with-current-buffer sage-review-base-revision-buffer
               (call-process-shell-command
                (format "git show %s:%s" sage-review-base (plist-get file-metadata :base)) nil t)))
            ((string-prefix-p "R" (plist-get file-metadata :type))
             (progn
               (with-current-buffer sage-review-base-revision-buffer
                 (call-process-shell-command
                  (format "git show %s:%s" sage-review-base (plist-get file-metadata :base)) nil t))
               (with-current-buffer sage-review-current-revision-buffer
                 (call-process-shell-command
                  (format "git show %s:%s" sage-review-commit (plist-get file-metadata :name)) nil t))))
            (t
             (progn
               (with-current-buffer sage-review-base-revision-buffer
                 (call-process-shell-command
                  (format "git show %s:%s" sage-review-base (plist-get file-metadata :base)) nil t))
               (with-current-buffer sage-review-current-revision-buffer
                 (call-process-shell-command
                  (format "git show %s:%s" sage-review-commit (plist-get file-metadata :name)) nil t))))))))

(defun sage-review-file ()
  "Review file."
  (cl-letf* (((symbol-function #'ediff-mode) (lambda () (sage-review-mode)))
             ((symbol-function #'ediff-set-keys) #'ignore)
             (default-directory sage-project-root))
    (with-current-buffer sage-review-base-revision-buffer
      (sage--review-enable-mode))
    (with-current-buffer sage-review-current-revision-buffer
      (sage--review-enable-mode))
    (ediff-buffers sage-review-base-revision-buffer sage-review-current-revision-buffer)))

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

(defun sage-review-files (&optional modified-commit)
  "Set the files to review."
  (let* ((files-in-latest-commit
          (split-string
           (string-trim
            (shell-command-to-string
             (format "git diff --name-status %s..%s" sage-review-base sage-review-commit)))
           "\n")))
    (setq files-in-latest-commit `(,(format "%s COMMIT_MSG" (if modified-commit "M" "A")) ,@files-in-latest-commit))
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
                  (sage--review-enable-mode)
                (fundamental-mode))))
          `(,ediff-buffer-A ,ediff-buffer-B)))

(defun sage--review-enable-mode ()
  "Enable filename appropriate mode."
  (when-let* ((extension (file-name-extension sage-review-file t))
              (mode (thread-last auto-mode-alist
                                 (seq-find (lambda (it)
                                             (string-match-p (car it) extension)))
                                 (cdr))))
    (funcall mode)))

(defun sage-review-quit ()
  "Quit `sage' review."
  (interactive)
  (sage-close-review-file)
  (tab-bar-close-tab))

(defun sage-review-next-hunk ()
  "Go to next hunk."
  (interactive)
  (sage--restore-overlays)
  (ediff-next-difference)
  (when (and
         sage--review-regions
         (sage--review-rebase-region-p))
    (sage--review-update-overlay 'a)
    (sage--review-update-overlay 'b)))

(defun sage--restore-overlays ()
  "Restore altered overlays."
  (when-let ((overlay ediff-current-diff-overlay-A))
    (with-current-buffer ediff-buffer-A
      (let ((overlays (overlays-in (overlay-start overlay)
                                   (overlay-end overlay))))
        (thread-last overlays
                     (seq-filter (lambda (it) (overlay-get it 'face)))
                     (seq-do (lambda (it)
                               (pcase (overlay-get it 'face)
                                 ('sage-current-rebase-diff
                                  (overlay-put it 'face 'ediff-current-diff-A))
                                 ('sage-fine-rebase-diff
                                  (overlay-put it 'face 'ediff-fine-diff-A)))))))))
  (when-let ((overlay ediff-current-diff-overlay-B))
    (with-current-buffer ediff-buffer-B
      (let ((overlays (overlays-in (overlay-start overlay)
                                   (overlay-end overlay))))
        (thread-last overlays
                     (seq-filter (lambda (it) (overlay-get it 'face)))
                     (seq-do (lambda (it)
                               (pcase (overlay-get it 'face)
                                 ('sage-current-rebase-diff
                                  (overlay-put it 'face 'ediff-current-diff-B))
                                 ('sage-fine-rebase-diff
                                  (overlay-put it 'face 'ediff-fine-diff-B))))))))))

(defun sage--review-rebase-region-p ()
  "Return t if current diff is based on a rebase."
  (unless (string= "COMMIT_MSG" sage-review-file)
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
       (sage--file-differences-intersect-p file-regions sage--review-regions)))))

(defun sage--review-update-overlay (side)
  "Update overlay on SIDE with different faces."
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
          (progn
            (overlay-put diff-overlay 'face 'sage-current-rebase-diff)
            (seq-do (lambda (it)
                      (overlay-put it 'face 'sage-fine-rebase-diff))
                    diff-fine-overlays)))))))

(defun sage-review-previous-hunk ()
  "Go to previous hunk."
  (interactive)
  (sage--restore-overlays)
  (ediff-previous-difference)
  (when (and
         sage--review-regions
         (sage--review-rebase-region-p))
    (sage--review-update-overlay 'a)
    (sage--review-update-overlay 'b)))

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
    (setq sage-review-base "HEAD~1")
    (setq sage-review-commit "HEAD")
    (sage-review-files)
    (sage-start-review)))

(defun sage-review-project-branches ()
  "Review two branches in project."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (setq sage-review-setup-function #'sage-setup-project-file-review)
    (setq sage-project-root default-directory)
    (setq sage-review-base (completing-read "Select base revision: "
                                            (sage--other-git-branches)))
    (setq sage-review-commit (sage--current-git-branch))
    (when (and sage-review-base sage-review-commit)
      (sage-branch-review-files sage-review-base sage-review-commit)
      (sage-start-review))))

(defun sage--current-git-branch ()
  "Return current branch name."
  (string-trim
   (with-temp-buffer
     (call-process-shell-command "git rev-parse --abbrev-ref HEAD" nil t)
     (buffer-string))))

(defun sage--other-git-branches ()
  "Return list of other local git branches excluding current."
  (let ((branches (split-string
                   (with-temp-buffer
                     (call-process-shell-command "git branch" nil t)
                     (buffer-string))
                   "\n" t)))
    (thread-last branches
                 (seq-remove (lambda (it) (string-prefix-p "*" it)))
                 (seq-map #'string-trim))))

(defun sage-branch-modified-files (branch)
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

(defun sage-branch-review-files (branch-a branch-b)
  "Set list of files based on BRANCH-A and BRANCH-B."
  (sage-review-files t)
  ;; Filter review files to only be modified in latest commits on
  ;; branch-a and branch-b
  (let* ((files-union
          (thread-last `(,branch-a ,branch-b)
                       (seq-map #'sage-branch-modified-files)
                       (flatten-list))))
    (setq files-union `("COMMIT_MSG" ,@files-union))
    (setq sage-review-files
          (thread-last sage-review-files
                       (seq-filter(lambda (it)
                                    (member it files-union)))
                       (seq-remove #'sage-review-file-rebased-p)))))

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
    (define-key map (kbd "b a") #'sage-review-browse-a)
    (define-key map (kbd "b b") #'sage-review-browse-b)
    (define-key map (kbd "q") #'sage-review-quit)
    (define-key map (kbd "s") #'sage-review-select-file)
    (define-key map (kbd "t") #'sage-review-toggle-highlight)
    (define-key map (kbd "n") #'sage-review-next-hunk)
    (define-key map (kbd "p") #'sage-review-previous-hunk)
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

;;;; WIP

(defun sage-review-browse-b ()
  (interactive)
  (funcall sage-review-open-in-browser 'b))

(defun sage-review-browse-a ()
  (interactive)
  (funcall sage-review-open-in-browser 'a))

(defun sage-review-hunk-regions (base-revision current-revision base-file current-file)
  "TBD"
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
          (when-let ((region (sage--parse-review-hunk a-hunk)))
            (push region  hunk-regions-a))
          (when-let ((region (sage--parse-review-hunk b-hunk)))
            (push region  hunk-regions-b)))))
    `((a . ,hunk-regions-a)
      (b . ,hunk-regions-b))))

(defun sage-review-file-rebased-p (filename)
  "Return t if file is rebased."
  (unless (string= filename "COMMIT_MSG")
    (let* ((base-revision sage-review-base)
           (current-revision sage-review-commit)
           (file-metadata (cdr (assoc filename sage-review-files-metadata)))
           (base-revision-filename (plist-get file-metadata :base))
           (base-current-regions (sage-review-hunk-regions base-revision current-revision base-revision-filename filename))
           (current-regions (sage-review-hunk-regions (concat current-revision "~1") current-revision filename filename)))
      (not
       (sage--file-differences-intersect-p base-current-regions
                                           current-regions)))))

(defun sage--parse-review-hunk (hunk)
  "Parse HUNK."
  (pcase-let ((`(,start ,length)
               (seq-map #'string-to-number (string-split hunk ","))))
    (if (or (not length))
        `(:begin ,start :end ,start)
      (unless (= length 0)
        `(:begin ,start :end ,(+ start (1- length)))))))

(defun sage--location-intersect-with-hunk-regions-p (location regions)
  "Return t if LOCATION intersect with REGIONS."
  (when regions
    (seq-find (lambda (line)
                (seq-find (lambda (region)
                            (<= (plist-get region :begin) line (plist-get region :end)))
                          regions))
              location)))

(defun sage--differences-intersect-p (regions1 regions2)
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

(defun sage--file-differences-intersect-p (file-diffs1 file-diffs2)
  "Return t if FILE-DIFFS1 intersects with FILE-DIFFS2."
  (or (sage--differences-intersect-p (alist-get 'a file-diffs1)
                                     (alist-get 'a file-diffs2))
      (sage--differences-intersect-p (alist-get 'b file-diffs1)
                                     (alist-get 'b file-diffs2))))

(provide 'sage)

;;; sage.el ends here
