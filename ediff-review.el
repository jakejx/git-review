;;; ediff-review.el --- Review patch sets with Ediff -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Maintainer: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://sr.ht/~niklaseklund/ediff-review.el
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

;; This package provides an `ediff' derived interface for reviewing patch sets.

;;; Code:

;;;; Requirements

(require 'ediff)
(require 'tab-bar)

;; TODO:
;; - refer to base-revision and current-revision
;; - `ediff-review-file-base-revision' and `ediff-review-file-current-revision'


;;;; Variables

(defcustom ediff-review-open-in-browser nil
  "Function to open a review location in the browser."
  :type 'symbol
  :group 'ediff-review)

(defvar ediff-review-files nil)
(defvar ediff-review-files-metadata nil)
(defvar ediff-review-file nil)
(defvar ediff-review-project-root nil)
(defvar ediff-review-temp-dir nil)
(defvar ediff-review-file-a nil)
(defvar ediff-review-file-b nil)
(defvar ediff-review-setup-function nil)
(defvar ediff-review-base nil)
(defvar ediff-review-commit nil)
(defvar ediff-review-base-revision-buffer nil)
(defvar ediff-review-current-revision-buffer nil)

(defvar ediff-review---regions nil)

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
               ediff-review-project-root
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
  (setq ediff-review-file file)
  (let* ((default-directory ediff-review-project-root)
         (file-metadata (cdr (assoc ediff-review-file ediff-review-files-metadata))))
    (setq ediff-review---regions (ediff-review-hunk-regions
                                (concat ediff-review-commit "~1") ediff-review-commit
                                (plist-get file-metadata :name) (plist-get file-metadata :name)))

    ;; Setup buffers
    (setq ediff-review-base-revision-buffer
          (get-buffer-create (format "%s<%s>" ediff-review-base
                                     (file-name-nondirectory (plist-get file-metadata :base)))))
    (setq ediff-review-current-revision-buffer
          (get-buffer-create (format "%s<%s>" ediff-review-commit
                                     (file-name-nondirectory (plist-get file-metadata :name)))))
    (with-current-buffer ediff-review-base-revision-buffer (erase-buffer))
    (with-current-buffer ediff-review-current-revision-buffer (erase-buffer))

    (if (string= ediff-review-file "COMMIT_MSG")
        (progn
          (when (string-equal "M" (plist-get file-metadata :type))
            (with-current-buffer ediff-review-base-revision-buffer
              (call-process-shell-command
               (format "git show --pretty=full --stat %s" ediff-review-base) nil t)))
          (with-current-buffer ediff-review-current-revision-buffer
            (call-process-shell-command
             (format "git show --pretty=full --stat %s" ediff-review-commit) nil t)))
      (cond ((string-equal "A" (plist-get file-metadata :type))
             (with-current-buffer ediff-review-current-revision-buffer
               (call-process-shell-command
                (format "git show %s:%s" ediff-review-commit (plist-get file-metadata :name)) nil t)))
            ((string-equal "D" (plist-get file-metadata :type))
             (with-current-buffer ediff-review-base-revision-buffer
               (call-process-shell-command
                (format "git show %s:%s" ediff-review-base (plist-get file-metadata :base)) nil t)))
            ((string-prefix-p "R" (plist-get file-metadata :type))
             (progn
               (with-current-buffer ediff-review-base-revision-buffer
                 (call-process-shell-command
                  (format "git show %s:%s" ediff-review-base (plist-get file-metadata :base)) nil t))
               (with-current-buffer ediff-review-current-revision-buffer
                 (call-process-shell-command
                  (format "git show %s:%s" ediff-review-commit (plist-get file-metadata :name)) nil t))))
            (t
             (progn
               (with-current-buffer ediff-review-base-revision-buffer
                 (call-process-shell-command
                  (format "git show %s:%s" ediff-review-base (plist-get file-metadata :base)) nil t))
               (with-current-buffer ediff-review-current-revision-buffer
                 (call-process-shell-command
                  (format "git show %s:%s" ediff-review-commit (plist-get file-metadata :name)) nil t))))))))

(defun ediff-review-file ()
  "Review file."
  (cl-letf* (((symbol-function #'ediff-mode) (lambda () (ediff-review-mode)))
             ((symbol-function #'ediff-set-keys) #'ignore)
             (default-directory ediff-review-project-root))
    (with-current-buffer ediff-review-base-revision-buffer
      (ediff-review---enable-mode))
    (with-current-buffer ediff-review-current-revision-buffer
      (ediff-review---enable-mode))
    (ediff-buffers ediff-review-base-revision-buffer ediff-review-current-revision-buffer)))

(defun ediff-review-close-review-file ()
  "Close current review file."
  (cl-letf (((symbol-function #'y-or-n-p) (lambda (&rest _args) t))
            (buffers `(,ediff-buffer-A ,ediff-buffer-B)))
    (call-interactively #'ediff-quit)
    (seq-do #'kill-buffer buffers)
    (seq-do (lambda (it)
              (when (string-match (rx bol "*" (or "ediff" "Ediff" "Ediff-Review")) (buffer-name it))
                (kill-buffer it)))
            (buffer-list))))

(defun ediff-review-files (&optional modified-commit)
  "Set the files to review."
  (let* ((files-in-latest-commit
          (split-string
           (string-trim
            (shell-command-to-string
             (format "git diff --name-status %s..%s" ediff-review-base ediff-review-commit)))
           "\n")))
    (setq files-in-latest-commit `(,(format "%s COMMIT_MSG" (if modified-commit "M" "A")) ,@files-in-latest-commit))
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
                         (`(,type ,name) (cons name `(:type ,type :name ,name :base ,name)))
                         (`(,type ,basename ,name) (cons name `(:type ,type :name ,name :base ,basename))))))
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
  (ediff-review-files t)
  ;; Filter review files to only be modified in latest commits on
  ;; branch-a and branch-b
  (let* ((files-union
          (thread-last `(,branch-a ,branch-b)
                       (seq-map #'ediff-review-branch-modified-files)
                       (flatten-list))))
    (setq files-union `("COMMIT_MSG" ,@files-union))
    (setq ediff-review-files
          (thread-last ediff-review-files
                       (seq-filter(lambda (it)
                                    (member it files-union)))
                       (seq-remove #'ediff-review-file-rebased-p)))))

(defun ediff-review-hunk-regions (base-revision current-revision base-file current-file)
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
          (when-let ((region (ediff-review--parse-review-hunk a-hunk)))
            (push region  hunk-regions-a))
          (when-let ((region (ediff-review--parse-review-hunk b-hunk)))
            (push region  hunk-regions-b)))))
    `((a . ,hunk-regions-a)
      (b . ,hunk-regions-b))))

(defun ediff-review-file-rebased-p (file)
  "Return t if FILE is changed due to a rebase."
  (unless (string= file "COMMIT_MSG")
    (let* ((base-revision ediff-review-base)
           (current-revision ediff-review-commit)
           (file-metadata (cdr (assoc file ediff-review-files-metadata)))
           (base-revision-filename (plist-get file-metadata :base))
           (base-current-regions (ediff-review-hunk-regions base-revision current-revision base-revision-filename file))
           (current-regions (ediff-review-hunk-regions (concat current-revision "~1") current-revision file file)))
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
  (when (and
         ediff-review---regions
         (ediff-review---rebase-region-p))
    (ediff-review---update-overlay 'a)
    (ediff-review---update-overlay 'b)))

(defun ediff-review-previous-hunk ()
  "Go to previous hunk."
  (interactive)
  (ediff-review--restore-overlays)
  (ediff-previous-difference)
  (when (and
         ediff-review---regions
         (ediff-review---rebase-region-p))
    (ediff-review---update-overlay 'a)
    (ediff-review---update-overlay 'b)))

(defun ediff-review-next-file ()
  "Review next file."
  (interactive)
  (let* ((current-index (cl-position
                         ediff-review-file ediff-review-files :test #'equal))
         (next-index (1+ current-index)))
    (if (>= next-index (length ediff-review-files))
        (message "No next file")
      (ediff-review-close-review-file)
      (funcall ediff-review-setup-function (seq-elt ediff-review-files next-index))
      (ediff-review-file)
      (message "Next file"))))

(defun ediff-review-previous-file ()
  "Review previous file."
  (interactive)
  (let* ((current-index (cl-position
                         ediff-review-file ediff-review-files :test #'equal))
         (previous-index (1- current-index)))
    (if (< previous-index 0)
        (message "No previous file")
      (ediff-review-close-review-file)
      (funcall ediff-review-setup-function (seq-elt ediff-review-files previous-index))
      (ediff-review-file)
      (message "Previous file"))))

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
              (file (cdr (assoc candidate candidates ))))
    (ediff-review-close-review-file)
    (funcall ediff-review-setup-function file)
    (ediff-review-file)))

;;;###autoload
(defun ediff-review-change ()
  "Review current change."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (setq ediff-review-setup-function #'ediff-review-setup-project-file-review)
    (setq ediff-review-project-root default-directory)
    (setq ediff-review-base "HEAD~1")
    (setq ediff-review-commit "HEAD")
    (ediff-review-files)
    (ediff-review-start-review)))

;;;###autoload
(defun ediff-review-patchsets ()
  "Review the difference between two patch-sets."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (setq ediff-review-setup-function #'ediff-review-setup-project-file-review)
    (setq ediff-review-project-root default-directory)
    (setq ediff-review-base (completing-read "Select base revision: "
                                            (ediff-review--other-git-branches)))
    (setq ediff-review-commit (ediff-review--current-git-branch))
    (when (and ediff-review-base ediff-review-commit)
      (ediff-review-branch-review-files ediff-review-base ediff-review-commit)
      (ediff-review-start-review))))

(defun ediff-review-browse-b ()
  (interactive)
  (funcall ediff-review-open-in-browser 'b))

(defun ediff-review-browse-a ()
  (interactive)
  (funcall ediff-review-open-in-browser 'a))

;;;; Support functions

(defun ediff-review---enable-mode ()
  "Enable filename appropriate mode."
  (when-let* ((extension (file-name-extension ediff-review-file t))
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
  (unless (string= "COMMIT_MSG" ediff-review-file)
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
       (ediff-review--file-differences-intersect-p file-regions ediff-review---regions)))))

(defun ediff-review---update-overlay (side)
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
            (overlay-put diff-overlay 'face 'ediff-review-current-rebase-diff)
            (seq-do (lambda (it)
                      (overlay-put it 'face 'ediff-review-fine-rebase-diff))
                    diff-fine-overlays)))))))

(defun ediff-review---file-candidates ()
  "Return an alist of review candidates."
  (thread-last ediff-review-files
               (seq-map-indexed (lambda (it index)
                                  (let* ((status
                                          (cdr (assoc it ediff-review-files-metadata)))
                                         (status-str (pcase (plist-get :type status)
                                                       ("A" "ADDED")
                                                       ("D" "DELETED")
                                                       ("M" "MODIFIED")
                                                       (_ "RENAMED"))))
                                    `(,(format "%s %s %s" (1+ index) it status-str) . ,it))))))

(defun ediff-review--parse-review-hunk (hunk)
  "Parse HUNK."
  (pcase-let ((`(,start ,length)
               (seq-map #'string-to-number (string-split hunk ","))))
    (if (or (not length))
        `(:begin ,start :end ,start)
      (unless (= length 0)
        `(:begin ,start :end ,(+ start (1- length)))))))

(defun ediff-review--location-intersect-with-hunk-regions-p (location regions)
  "Return t if LOCATION intersect with REGIONS."
  (when regions
    (seq-find (lambda (line)
                (seq-find (lambda (region)
                            (<= (plist-get region :begin) line (plist-get region :end)))
                          regions))
              location)))

(defun ediff-review--differences-intersect-p (regions1 regions2)
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
  (or (ediff-review--differences-intersect-p (alist-get 'a file-diffs1)
                                     (alist-get 'a file-diffs2))
      (ediff-review--differences-intersect-p (alist-get 'b file-diffs1)
                                     (alist-get 'b file-diffs2))))

(defun ediff-review--current-git-branch ()
  "Return current branch name."
  (string-trim
   (with-temp-buffer
     (call-process-shell-command "git rev-parse --abbrev-ref HEAD" nil t)
     (buffer-string))))

(defun ediff-review--other-git-branches ()
  "Return list of other local git branches excluding current."
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
                ediff-review-file ediff-review-files :test #'equal))
           (length ediff-review-files))))

(provide 'ediff-review)

;;; ediff-review.el ends here
