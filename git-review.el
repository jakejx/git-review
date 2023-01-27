;;; git-review.el --- Review patch sets with Ediff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Niklas Eklund

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Maintainer: Niklas Eklund <niklas.eklund@posteo.net>
;; URL: https://sr.ht/~niklaseklund/git-review
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

(defcustom git-review-user nil
  "The name of the user."
  :type 'string
  :group 'git-review)

(defcustom git-review-comment-major-mode #'text-mode
  "Defines the major mode to use in comment mode."
  :type 'symbol
  :group 'git-review)

(defcustom git-review-determine-change-function nil
  "A function that returns a change value."
  :type 'symbol
  :group 'git-review)

(defcustom git-review-determine-patchset-function nil
  "A function that returns a patchset value."
  :type 'symbol
  :group 'git-review)

(defcustom git-review-publish-function nil
  "Function that can publish a review."
  :type 'symbol
  :group 'git-review)

(defcustom git-review-database-dir user-emacs-directory
  "The directory to store the review database in."
  :type 'string
  :group 'git-review)

(defcustom git-review-comment-buffer-action
  '(display-buffer-in-side-window
    (side . bottom)
    (slot . -1))
  "The action used to display a comment."
  :group 'git-review
  :type 'sexp)

(defun git-review--annotation-file-name (entry)
  "Return file-name of ENTRY."
  (car entry))

(defcustom git-review-file-annotation
  '((:name type :function git-review--annotation-file-type :face 'font-lock-comment-face)
    (:name reviewed :function git-review--annotation-file-reviewed :face 'font-lock-string-face)
    (:name ignored :function git-review--annotation-file-ignored :face 'font-lock-string-face)
    (:name comments :function git-review--annotation-file-comments :face 'font-lock-string-face))
  "A list of annotations to display for a review file.

Each entry in the list is a property list with the following properties:
- :name
- :function
- :align
- :face
- :width"
  :group 'git-review
  :type '(repeat (plist :options ((:name symbol)
                                  (:function symbol)
                                  (:align symbol)
                                  (:face symbol)))))

(defcustom git-review-ignore-file-predicates nil
  "A list of predicates for determining if a file should be ignored."
  :group 'git-review
  :type '(repeat symbol))

(defcustom git-review-metadata-functions nil
  "A list of metadata functions which adds a metadata property to a file."
  :group 'git-review
  :type '(repeat symbol))

;;;; Public

(defvar git-review nil
  "Variable which holds all data related to the current review.")
(defvar git-review-base-revision-buffer nil
  "Points to the buffer of the base revision.")
(defvar git-review-current-revision-buffer nil
  "Points to the buffer of the current revision.")
(defvar git-review-project nil "The name of the current project.")

;;;; Private

(defvar git-review--patchset nil "The current patchset.")

(defvar git-review--current-comment nil)
(defvar git-review--reviews nil)

(defvar git-review--candidates nil)
(defvar git-review--annotations nil)
(defvar git-review--annotation-widths nil)
(defvar git-review--annotation-config nil)

(defvar git-review--conversation-frame nil)

;;;; Faces

(defgroup git-review-faces nil
  "Faces used by `git-review'."
  :group 'git-review
  :group 'faces)

(defface git-review-current-rebase-diff
  '((t :inherit ediff-current-diff-C))
  "Face used to highlight rebase diff.")

(defface git-review-fine-rebase-diff
  '((t :inherit ediff-fine-diff-C))
  "Face used to highlight rebase diff.")

(defface git-review-comment-header
  '((t :inherit diff-function))
  "Face used to highlight comment header.")

;;;; Functions

(defun git-review-init-db ()
  "Initialize the review database."
  (unless git-review--reviews
    (let ((db (expand-file-name "git-review.db" git-review-database-dir)))
      (if (file-exists-p db)
          (setq git-review--reviews
                (with-temp-buffer
                  (insert-file-contents db)
                  (goto-char (point-min))
                  (read (current-buffer))))
        (make-empty-file db t)
        (setq git-review--reviews nil)))))

(defun git-review-update-db ()
  "Update the review database."
  (let ((db (expand-file-name "git-review.db" git-review-database-dir)))
    (with-temp-file db
      (prin1 git-review--reviews (current-buffer)))))

(defun git-review--update-review ()
  "Update reviews with review.

If review can already be found in an existing group update it
otherwise create it."
  (if-let ((grouped-reviews
            (alist-get git-review-change git-review--reviews nil nil #'equal)))
      (if (seq-find (lambda (it)
                      (and (equal (let-alist it .current-revision)
                                  (let-alist git-review .current-revision))
                           (equal (let-alist it .base-revision)
                                  (let-alist git-review .base-revision))))
                    grouped-reviews)
          (setf (alist-get git-review-change git-review--reviews nil nil #'equal)
                (thread-last grouped-reviews
                             (seq-map (lambda (it)
                                        (if (and (equal (let-alist it .current-revision)
                                                        (let-alist git-review .current-revision))
                                                 (equal (let-alist it .base-revision)
                                                        (let-alist git-review .base-revision)))
                                            git-review
                                          it)))
                             (vconcat)))
        (setf (alist-get git-review-change git-review--reviews nil nil #'equal)
              (vconcat (vconcat `(,git-review)) grouped-reviews)))
    (setf (alist-get git-review-change git-review--reviews nil nil #'equal)
          (vconcat `(,git-review)))))

(defun git-review-start-review ()
  "Start review."
  (let ((git-review-tab "Git Review"))
    (when (not (member git-review-tab
                       (mapcar (lambda (tab)
                                 (alist-get 'name tab))
                               (tab-bar-tabs))))
      (tab-bar-new-tab)
      (tab-bar-rename-tab git-review-tab)
      (git-review-setup-project-file-review
       (if-let ((current-file (git-review--current-file)))
           current-file
         (seq-elt (git-review--files) 0)))
      (git-review-file))))

(defun git-review-setup-project-file-review (file)
  "Setup `git-review' for project FILE review."
  (setf (alist-get 'current-file git-review--patchset) file)
  (let* ((default-directory (project-root (project-current)))
         (file-info (git-review--file-info)))
    (git-review--setup-buffers)
    (let-alist file-info
      (if (string= (git-review--current-file) "COMMIT_MSG")
          (progn
            (when (string-equal "M" .type)
              (git-review--commit-message (git-review--base-revision)
                                            git-review-base-revision-buffer))
            (git-review--commit-message (git-review--current-revision)
                                          git-review-current-revision-buffer))
        (unless (string-equal "A" .type)
          (git-review--file-content (git-review--base-revision)
                                      (or .original-filename .filename)
                                      git-review-base-revision-buffer))
        (unless (string-equal "D" .type)
          (git-review--file-content (git-review--current-revision)
                                      file
                                      git-review-current-revision-buffer t))))))

(defun git-review-file ()
  "Review current file."
  (cl-letf* (((symbol-function #'ediff-mode) (lambda () (git-review-mode)))
             ((symbol-function #'ediff-set-keys) #'ignore)
             (default-directory (git-review--project-root)))
    (ediff-buffers git-review-base-revision-buffer git-review-current-revision-buffer)
    (git-review--restore-comment-overlays)
    (git-review--restore-buffer-location)))

(defun git-review-close-review-file ()
  "Close current review file."
  (cl-letf (((symbol-function #'y-or-n-p) (lambda (&rest _args) t))
            (buffers `(,ediff-buffer-A ,ediff-buffer-B)))
    (git-review--store-buffer-locations)
    ;; (git-review--remove-file-comment-overlays)
    (call-interactively #'ediff-quit)
    (seq-do (lambda (it)
              (with-current-buffer it
                (set-buffer-modified-p nil)
                (kill-buffer)))
            buffers)
    (seq-do (lambda (it)
              (when (string-match (rx bol "*" (or "ediff" "Ediff" "Git-Review")) (buffer-name it))
                (kill-buffer it)))
            (buffer-list))))

(defun git-review-branch-modified-files (branch)
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

(defun git-review-hunk-regions (base-revision current-revision base-file current-file)
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
          (when-let ((region (git-review--parse-review-hunk a-hunk)))
            (push region  hunk-regions-a))
          (when-let ((region (git-review--parse-review-hunk b-hunk)))
            (push region  hunk-regions-b)))))
    `((a . ,hunk-regions-a)
      (b . ,hunk-regions-b))))

(defun git-review-file-rebased-p (file)
  "Return t if FILE is changed due to a rebase."
  (unless (string= file "COMMIT_MSG")
    (let* ((file-info (git-review--file-info file))
           (base-current-regions (git-review-hunk-regions (git-review--base-revision)
                                                            (git-review--current-revision)
                                                            (let-alist file-info .base-filename)
                                                            file))
           (current-regions (git-review-hunk-regions (concat (git-review--current-revision) "~1")
                                                       (git-review--current-revision)
                                                       file
                                                       file)))
      (git-review--update-file file 'review-diff-regions base-current-regions)
      (git-review--update-file file 'current-revision-diff-regions current-regions)
      (not
       (git-review--file-differences-intersect-p base-current-regions
                                                   current-regions)))))

;;;; Commands

(defun git-review-open-patchset-diff ()
  "Open diff buffer with patch-set."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (vc-diff-internal t
                      (list (vc-responsible-backend default-directory) (list default-directory))
                      (git-review--base-revision)
                      (git-review--current-revision))))

(defun git-review-toggle-conversation ()
  "Toggle conversation."
  (interactive)
  (if (frame-live-p git-review--conversation-frame)
      (progn
        (make-frame-invisible git-review--conversation-frame t)
        (kill-buffer "*git-review-conversation*"))
    (when-let ((comment (git-review--comment-at-point)))
      (let* ((parent-frame (window-frame))
             (child-frame (make-frame
                           `((parent-frame . ,parent-frame)
                             (minibuffer . ,(minibuffer-window parent-frame))
                             (child-frame-border-width . 1)
                             (visibility . nil)
                             (desktop-dont-save . t)
                             (tab-bar-lines . 0)
                             ;; (vertical-scroll-bars . t)
                             (tool-bar-lines . 0)
                             (menu-bar-lines . 0))))
             (buffer (get-buffer-create "*git-review-conversation*"))
             (window (frame-root-window child-frame))
             (line-count))
        (setq git-review--conversation-frame child-frame)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (git-review-conversation-mode)
            (let-alist comment
              (insert (propertize (format "%s:\n" git-review-user) 'face 'org-block-begin-line))
              (insert (with-temp-buffer
                        (insert .message)
                        (gfm-view-mode)
                        (font-lock-ensure)
                        (buffer-substring (point-min) (point-max)))))
            (goto-char (point-min)))
          (setq line-count (count-lines (point-min) (point-max) 'ignore-invisible-lines)))
        (set-window-buffer window buffer)
        (set-window-dedicated-p window t)
        (git-review--conversation-frame-configure-size child-frame comment line-count)
        (make-frame-visible child-frame)))))

(defun git-review--conversation-frame-configure-size (frame comment line-count)
  "Configure size of FRAME using info from COMMENT and LINE-COUNT."
  (save-excursion
    (let-alist comment (goto-char (overlay-start .header-overlay)))
    (pcase-let* ((`(,frame-x-start ,_ ,_ ,_) (frame-edges (selected-frame)))
                 (`(,_ ,_ ,_ ,window-y-end) (window-edges (selected-window) t t t))
                 (frame-width (* (window-font-width) 120))
                 (frame-height (min (* (line-pixel-height) line-count)
                                        (- window-y-end (cdr (window-absolute-pixel-position (point))))))
                 (x-position (- (car (window-absolute-pixel-position (point))) frame-x-start))
                 (y-position (- (cdr (window-absolute-pixel-position (point))) (window-font-height))))
      (set-frame-size frame frame-width frame-height t)
      (set-frame-position frame  x-position y-position))))

(defun git-review-quit ()
  "Quit `git-review' review."
  (interactive)
  (git-review-close-review-file)
  (git-review--update-review)
  (when git-review-change
    (git-review-update-db))
  (setq git-review-change nil)
  (setq git-review nil)
  (tab-bar-close-tab))

(defun git-review-next-comment ()
  "Go to next comment."
  (interactive)
  (if-let ((comment (with-selected-window (get-buffer-window git-review-current-revision-buffer)
                      (save-excursion
                        (git-review--next-comment)))))
      (progn
        (git-review--restore-overlays)
        ;; TODO(Niklas Eklund, 20230120): Handle comments in both sides
        (with-selected-window (get-buffer-window git-review-current-revision-buffer)
          (goto-char (let-alist comment  .location.start-point)))
        (save-excursion
          (with-selected-window (git-review--control-window)
            (let ((last-command-event ?b))
              (ediff-jump-to-difference-at-point nil))
            (git-review---maybe-modify-overlays)))
        (git-review---maybe-modify-overlays)
        (git-review--maybe-set-reviewed))
    (message "No next comment found")))

(defun git-review-previous-comment ()
  "Go to previous comment."
  (interactive)
  (if-let ((comment (with-selected-window (get-buffer-window git-review-current-revision-buffer)
                      (save-excursion
                        (git-review--previous-comment)))))
      (progn
        (git-review--restore-overlays)
        ;; TODO(Niklas Eklund, 20230120): Handle comments in both sides
        (with-selected-window (get-buffer-window git-review-current-revision-buffer)
          (goto-char (let-alist comment  .location.start-point)))
        (save-excursion
          (with-selected-window (git-review--control-window)
            (let ((last-command-event ?b))
              (ediff-jump-to-difference-at-point nil))
            (git-review---maybe-modify-overlays)))
        (git-review---maybe-modify-overlays)
        (git-review--maybe-set-reviewed))
    (message "No previous comment found")))

(defun git-review-next-hunk ()
  "Go to next hunk."
  (interactive)
  (git-review--restore-overlays)
  (ediff-next-difference)
  (git-review---maybe-modify-overlays)
  (git-review--maybe-set-reviewed))

(defun git-review-previous-hunk ()
  "Go to previous hunk."
  (interactive)
  (git-review--restore-overlays)
  (ediff-previous-difference)
  (git-review---maybe-modify-overlays))

(defun git-review-next-file ()
  "Review next file."
  (interactive)
  (if-let ((next-file (git-review--next-file)))
      (git-review--switch-file next-file)
    (message "No next file")))

(defun git-review-previous-file ()
  "Review previous file."
  (interactive)
  (if-let ((previous-file (git-review--previous-file)))
      (git-review--switch-file previous-file)
    (message "No previous file")))

(defun git-review-select-file ()
  "Select a file to review."
  (interactive)
  (when-let ((candidates (seq-map (lambda (file)
                                    `(,file . ,(git-review--file-info file)))
                                  (git-review--files)))
             (file-info (git-review-completing-read candidates
                                                      "Select file: "
                                                      'git-review-file
                                                      git-review-file-annotation))
             (file (let-alist file-info .current-filename)))
    (git-review--switch-file file)))

(defun git-review--harmonize-candidate-lengths (candidates)
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

(defun git-review-completing-read (candidates prompt category annotation-config)
  "Select CANDIDATES from CATEGORY with and PROMPT."
  (when-let* ((git-review--candidates (git-review--harmonize-candidate-lengths candidates))
              (git-review--annotation-config annotation-config)
              (git-review--annotations (git-review--annotations git-review--candidates))
              (git-review--annotation-widths (git-review--annotation-widths))
              (metadata `(metadata
                          (category . ,category)
                          (cycle-sort-function . identity)
                          (annotation-function . git-review--annotation-function)
                          (display-sort-function . identity)))
              (collection (lambda (string predicate action)
                            (if (eq action 'metadata)
                                metadata
                              (complete-with-action action git-review--candidates string predicate))))
              (candidate (completing-read prompt collection nil t)))
    (cdr (assoc candidate git-review--candidates))))

(defun git-review-switch-to-most-recent-file ()
  "Switch to most recently reviewed file."
  (interactive)
  (when-let* ((recent-file (git-review--most-recent-file)))
    (setf (alist-get 'recent-file git-review) (git-review--current-file))
    (git-review-close-review-file)
    (git-review-setup-project-file-review recent-file)
    (git-review-file)))

;;;###autoload
(defun git-review-patchset ()
  "Review current patchset."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (git-review--initialize-review)
    (git-review-start-review)))

;; ;;;###autoload
;; (defun git-review-patchsets ()
;;   "Review the difference between two patch-sets."
;;   (interactive)
;;   (unless git-review-change
;;     (setq git-review-change
;;           (format "%s@%s"
;;                   (git-review--current-git-branch)
;;                   (project-root (project-current)))))
;;   (let* ((default-directory (project-root (project-current))))
;;     (when-let ((base-revision (completing-read "Select base revision: "
;;                                                (git-review--other-git-branches)))
;;                (current-revision (git-review--current-git-branch)))
;;       (git-review--initialize-review current-revision
;;                                        base-revision)
;;       (git-review-start-review))))

(defun git-review-publish-review ()
  "Publish review."
  (interactive)
  (if (functionp git-review-publish-function)
      (progn
        ;; TODO: For each comment set published if not already set, and if
        ;; so add a timestamp
        (funcall git-review-publish-function git-review))
    (message "No publish function definied")))

(defun git-review-jump-to-a ()
  "Jump to base revision buffer."
  (interactive)
  (select-window
   (get-buffer-window git-review-base-revision-buffer)))

(defun git-review-jump-to-b ()
  "Jump to current revision buffer."
  (interactive)
  (select-window
   (get-buffer-window git-review-current-revision-buffer)))

(defun git-review-jump-to-control ()
  "Jump to control buffer."
  (interactive)
  (select-window (git-review--control-window)))

(defun git-review--comment-at-point ()
  "Return comment at point."
  (let-alist (git-review--file-info)
    (thread-last .comments
                 (seq-find (lambda (it)
                             (let-alist it
                               (<= .location.start-point (point) .location.end-point))))
                 (cdr))))

(defun git-review-comment ()
  "Add or edit a comment."
  (interactive)
  (setq git-review--current-comment
        (or
         (git-review--comment-at-point)
         (git-review--create-comment)))
  (let* ((buffer (get-buffer-create "*git-review-comment*")))
    (display-buffer buffer git-review-comment-buffer-action)
    (with-current-buffer buffer
      (erase-buffer)
      (let-alist git-review--current-comment
        (when .message (insert .message)))

      (when git-review-comment-major-mode
        (funcall git-review-comment-major-mode))
      (git-review-comment-mode)
      (select-window (get-buffer-window (current-buffer)))
      (goto-char (point-max)))))

(defun git-review-kill-comment ()
  "Kill comment at point."
  (interactive)
  (when-let ((comment
              (let-alist (git-review--file-info)
                (thread-last .comments
                             (seq-find (lambda (it)
                                         (let-alist it
                                           (<= .location.start-point (point) .location.end-point))))
                             (cdr)))))
    (let-alist comment
      (delete-overlay .header-overlay)
      (delete-overlay .comment-overlay)
      (git-review--update-comments .id))))

(defun git-review-complete-comment ()
  "Complete the review comment."
  (interactive)
  (setf (alist-get 'message git-review--current-comment)
        (buffer-substring-no-properties (point-min) (point-max)))
  (git-review--add-comment-overlay)
  (git-review--update-comments (let-alist git-review--current-comment .id)
                                 git-review--current-comment)
  (setq git-review--current-comment nil)
  (quit-restore-window))

(defun git-review-quit-comment ()
  "Quit review comment."
  (interactive)
  (setq git-review--current-comment nil)
  (quit-restore-window (get-buffer-window (current-buffer)) 'kill))

;;;; Support functions

(defun git-review--switch-file (file)
  "Switch to FILE."
  (setf (alist-get 'recent-file git-review--patchset) (git-review--current-file))
  (git-review-close-review-file)
  (git-review-setup-project-file-review file)
  (git-review-file))

(defun git-review--project-root ()
  "Return the project root of the current review."
  (let-alist git-review .project))

(defun git-review--current-revision ()
  "Return the current revision."
  (let-alist git-review--patchset .commit-hash))

(defun git-review--base-revision ()
  "Return the base revision."
  (let-alist git-review--patchset
    (or .base-patchset
        .parent-hash)))

(defun git-review--has-comments-p (file)
  "Return t if FILE has comments."
  (let-alist (git-review--file-info file)
    (not (null .comments))))

(defun git-review--is-reviewed-p (file)
  "Return t if FILE is reviewed."
  (let-alist (git-review--file-info file)
    (not (null .reviewed))))

(defun git-review--ignore-file-p (file)
  "Return t if FILE should be ignored."
  (seq-find (lambda (predicate)
              (funcall predicate file))
            git-review-ignore-file-predicates))

(defun git-review--file-metadata (file)
  "Return metadata for FILE."
  (thread-last git-review-metadata-functions
               (seq-map (lambda (it)
                          (funcall it file)))
               (seq-remove #'null)))

(defun git-review--multiple-patchsets-p ()
  "Return t if multiple patch-sets are being reviewed."
  (let-alist git-review .multiple-patchsets))

(defun git-review--files ()
  "Return a list of review files."
  (seq-map (lambda (it) (let-alist it .filename))
           (let-alist git-review--patchset .files)))

(defun git-review--current-file ()
  "Return the name of the current file being reviewed."
  (let-alist git-review--patchset .current-file))

(defun git-review--most-recent-file ()
  "Return the name of the most recently reviewed file."
  (let-alist git-review--patchset .recent-file))

(defun git-review--progress ()
  "Return review progress."
  (or (let-alist git-review .progress) 0.0))

(defun git-review--current-revision-diff-regions ()
  "Return diff regions for file from current revision."
  (let-alist (git-review--file-info)
    .current-revision-diff-regions))

(defun git-review--file-info (&optional file)
  "Info about FILE."
  (let-alist git-review--patchset
    (let ((file (or file .current-file)))
      (seq-find (lambda (it) (let-alist it (equal .filename file))) .files))))

(defun git-review--update-file (file key value)
  "Update FILE with KEY and VALUE."
  (let* ((files (let-alist git-review .files))
         (file-info (alist-get file files nil nil #'equal)))
    (setf (alist-get key file-info) value)
    (setf (alist-get file files nil nil #'equal) file-info)
    (setf (alist-get 'files git-review) files)))

(defun git-review--update-comments (id &optional comment)
  "Update list of file comments with ID to COMMENT.

Unless COMMENT is nil, then delete ID."
  (let* ((file (let-alist git-review .current-file))
         (files (let-alist git-review .files))
         (file-info (alist-get file files nil nil #'equal))
         (comments (alist-get 'comments file-info)))
    (if comment
        (setf (alist-get id comments) comment)
      (setf comments (assoc-delete-all id comments)))
    (setf (alist-get 'comments file-info) comments)
    (setf (alist-get file files nil nil #'equal) file-info)
    (setf (alist-get 'files git-review) files)))

(defun git-review--store-buffer-locations ()
  "Store locations in review buffers for current review file."
  (git-review--update-file (git-review--current-file) 'buffer-location
                             `((a . ,(with-current-buffer git-review-base-revision-buffer (point)))
                               (b . ,(with-current-buffer git-review-current-revision-buffer (point))))))

(defun git-review--store-progress ()
  "Store progress percentage."
  (let ((progress (let-alist git-review
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
    (setf (alist-get 'progress git-review) progress)))

(defun git-review--initialize-review ()
  "Initialize review of `gerrit-review-patchset'."
  ;; TODO(Niklas Eklund, 20230127): Find and return existing patchset review
  ;; (git-review--stored-review)
  (let* ((git-review-change (funcall git-review-determine-change-function))
         (git-review-patchset (funcall git-review-determine-patchset-function))
        (commit-hash (with-temp-buffer
                       (call-process-shell-command "git show --no-patch --pretty=format:%H" nil t)
                       (buffer-string)))
        (parent-hash (with-temp-buffer
                       (call-process-shell-command (concat "git show --no-patch --pretty=format:%P " commit-hash) nil t)
                       (buffer-string))))
    (setq git-review--patchset `((commit-hash . ,commit-hash)
                                 (parent-hash . ,parent-hash)
                                 (number . ,git-review-patchset)
                                 (change . ,git-review-change)
                                 (review-progress . [])
                                 (files . ,(git-review--generate-patchset-files)))))

  ;; TODO(Niklas Eklund, 20230127): Move to other location when a
  ;; different patchset is selected. Or maybe it needs to be here in
  ;; case a stored reviewed has a non-nil base-patchset

  ;; (when multiple-patchsets
  ;;   (git-review--remove-rebased-files-from-review))

  ;; TODO(Niklas Eklund, 20230127): Add back metadata tags
  ;; (git-review--add-metadata-to-files)
  ;; (git-review--add-ignore-tag-to-files)
  )

(defun git-review--add-ignore-tag-to-files ()
  "Add ignore tag to files that should be ignored in variable `git-review'."
  (seq-do (lambda (file)
            (when (git-review--ignore-file-p file)
              (git-review--update-file file 'ignore t)))
          (git-review--files)))

(defun git-review--add-metadata-to-files ()
  "Add metadata to files in variable `git-review'."
  (seq-do (lambda (file)
            (when-let ((metadata (git-review--file-metadata file)))
              (git-review--update-file file 'metadata metadata)))
          (git-review--files)))

(defun git-review--generate-patchset-files ()
  "Return a vector of patchset files."
  (let* ((files-in-patchset
          (split-string
           (string-trim
            (shell-command-to-string
             (format "git diff --name-status %s..%s"
                     (git-review--base-revision)
                     (git-review--current-revision))))
           "\n"))
         (files-info ))
    (vconcat
     '[((filename . "COMMIT_MSG"))]
     `[,@(seq-map (lambda (it)
                    (let ((elements (split-string it)))
                      (pcase elements
                        (`(,type ,filename)
                         `((filename . ,filename)
                           (type . ,type)))
                        (`(,type ,base-filename ,filename)
                         `((filename . ,filename)
                           (original-filename . ,base-filename)
                           (type . ,type))))))
                  files-in-patchset)])))

(defun git-review--remove-rebased-files-from-review ()
  "Remove rebased files in variable `git-review'."
  (let* ((files-union
          `("COMMIT_MSG"
            ,@(thread-last `(,(git-review--base-revision)
                             ,(git-review--current-revision))
                           (seq-map #'git-review-branch-modified-files)
                           (flatten-list))))
         (review-files
          (thread-last (git-review--files)
                       (seq-filter (lambda (it)
                                     (member it files-union)))
                       ;; TODO(Niklas Eklund, 20230111): Look into the
                       ;; logic of this, it doesn't seems to always
                       ;; work
                       (seq-remove #'git-review-file-rebased-p))))
    ;; Update `git-review' with files
    (let ((updated-files (alist-get 'files git-review)))
      (seq-do (lambda (it)
                (let ((file (car it)))
                  (unless (member file review-files)
                    (setf updated-files (assoc-delete-all file updated-files #'equal)))))
              updated-files)
      (setf (alist-get 'files git-review) updated-files))))

(defun git-review--stored-review (current-revision base-revision)
  "Return a stored review of CURRENT-REVISION and BASE-REVISION."
  (when git-review-change
    (when-let ((grouped-reviews (alist-get git-review-change git-review--reviews nil nil #'equal)))
      (seq-find (lambda (it)
                  (and (equal (let-alist it .current-revision)
                              current-revision)
                       (equal (let-alist it .base-revision)
                              base-revision)))
                grouped-reviews))))

(defun git-review--restore-buffer-location ()
  "Restore buffer location to nearest diff in revision buffer.

This is done for files that has already been reviewed before and where
there is a previous location to return to."
  (let-alist (git-review--file-info)
    (if (and .buffer-location
               ; `ediff' complains when location is at start of buffer
             (> .buffer-location.b 1))
        (progn
          (with-selected-window (get-buffer-window git-review-current-revision-buffer)
            (goto-char .buffer-location.b))
          (with-selected-window (git-review--control-window)
            (let ((last-command-event ?b))
              (ediff-jump-to-difference-at-point nil))
            (git-review---maybe-modify-overlays)))
      (with-selected-window (git-review--control-window)
        (ediff-next-difference)
        (git-review--maybe-set-reviewed)))))

(defun git-review--control-window ()
  "Return window for variable `ediff-control-buffer'."
  (seq-find (lambda (it)
              (with-selected-window it
                ediff-control-buffer))
            (window-list)))

(defun git-review--file-content (revision file buffer &optional set-filename)
  "Populate BUFFER with FILE content from REVISION.

Optionally instruct function to SET-FILENAME."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (call-process-shell-command
       (format "git show %s:%s" revision file) nil t)
      (setq-local default-directory
                  (file-name-directory (expand-file-name file (git-review--project-root))))
      (when set-filename
        (setq-local buffer-file-name (expand-file-name file (git-review--project-root))))
      (git-review---enable-mode))
    (read-only-mode)))

(defun git-review--commit-message (revision buffer)
  "Populate BUFFER with commit message from REVISION."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (insert (concat (git-review--commit-message-header revision) "\n\n"))
      (insert (with-temp-buffer
                (call-process-shell-command
                 (format "git show --pretty=format:\"%s\" --no-patch %s" "%s" revision) nil t)
                (concat (propertize (buffer-string) 'face 'font-lock-keyword-face) "\n\n")))
      (insert (with-temp-buffer
                (call-process-shell-command
                 (format "git show --pretty=format:\"%s\" --no-patch %s" "%b" revision) nil t)
                (propertize (buffer-string) 'face 'italic))))
    (git-review-minor-mode)
    (read-only-mode)))

(defun git-review--commit-message-header (revision)
  "Return propertized commit header for REVISION."
  (let* ((re (rx bol (group (regexp ".*?:")) (group (regexp ".*"))))
         (components)
         (pretty-format "Parent:     %p%nAuthor:     %aN <%ae>%nAuthorDate: %ai%nCommit:     %cN <%ce>%nCommitDate: %ci")
         (commit-header (with-temp-buffer
                          (call-process-shell-command
                           (format "git show --pretty=format:\"%s\" --no-patch %s" pretty-format revision) nil t)
                          (goto-char (point-min))
                          (while (search-forward-regexp re nil t)
                            (push (concat (propertize (match-string 1) 'face 'font-lock-comment-face)
                                          (propertize (match-string 2) 'face 'font-lock-comment-face))
                                  components))
                          (string-join components "\n"))))
    commit-header))

(defun git-review--setup-buffers ()
  "Setup buffers for `git-review'."
  (let ((file-info (git-review--file-info)))
    (let-alist file-info
      (setq git-review-base-revision-buffer
            (get-buffer-create (format "%s<%s>" (git-review--base-revision)
                                       (file-name-nondirectory (or .original-filename .filename)))))
      (setq git-review-current-revision-buffer
            (get-buffer-create (format "%s<%s>" (git-review--current-revision)
                                       (file-name-nondirectory .filename)))))
    (with-current-buffer git-review-base-revision-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (with-current-buffer git-review-current-revision-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun git-review---enable-mode ()
  "Enable filename appropriate mode."
  (when-let* ((mode (thread-last auto-mode-alist
                                 (seq-find (lambda (it)
                                             (string-match-p (car it)
                                                             (git-review--current-file))))
                                 (cdr))))
    (funcall mode))
  (git-review-minor-mode))

(defun git-review--restore-overlays ()
  "Restore altered overlays."
  (when-let ((overlay ediff-current-diff-overlay-A))
    (with-current-buffer ediff-buffer-A
      (let ((overlays (overlays-in (overlay-start overlay)
                                   (overlay-end overlay))))
        (thread-last overlays
                     (seq-filter (lambda (it) (overlay-get it 'face)))
                     (seq-do (lambda (it)
                               (pcase (overlay-get it 'face)
                                 ('git-review-current-rebase-diff
                                  (overlay-put it 'face 'ediff-current-diff-A))
                                 ('git-review-fine-rebase-diff
                                  (overlay-put it 'face 'ediff-fine-diff-A)))))))))
  (when-let ((overlay ediff-current-diff-overlay-B))
    (with-current-buffer ediff-buffer-B
      (let ((overlays (overlays-in (overlay-start overlay)
                                   (overlay-end overlay))))
        (thread-last overlays
                     (seq-filter (lambda (it) (overlay-get it 'face)))
                     (seq-do (lambda (it)
                               (pcase (overlay-get it 'face)
                                 ('git-review-current-rebase-diff
                                  (overlay-put it 'face 'ediff-current-diff-B))
                                 ('git-review-fine-rebase-diff
                                  (overlay-put it 'face 'ediff-fine-diff-B))))))))))

(defun git-review---rebase-region-p ()
  "Return t if current diff is based on a rebase."
  (unless (string= "COMMIT_MSG" (git-review--current-file))
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
       (git-review--file-differences-intersect-p file-regions
                                                   (git-review--current-revision-diff-regions))))))

(defun git-review---maybe-modify-overlays ()
  "Maybe modify overlays if current diff is due to a rebase."
  (when-let* ((is-rebase-diff (and
                               (git-review--multiple-patchsets-p)
                               (git-review---rebase-region-p)))
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
                         (overlay-put diff-overlay 'face 'git-review-current-rebase-diff)
                         (seq-do (lambda (it)
                                   (overlay-put it 'face 'git-review-fine-rebase-diff))
                                 diff-fine-overlays))))))))
    (funcall update-overlay-fun 'a)
    (funcall update-overlay-fun 'b)))

(defun git-review--maybe-set-reviewed ()
  "Set file to reviewed if the last diff has been reached."
  (when (and (not (git-review--is-reviewed-p (git-review--current-file)))
         (= (1+ ediff-current-difference) ediff-number-of-differences))
    (git-review--update-file (git-review--current-file) 'reviewed t)
    (git-review--store-progress)
    (with-current-buffer (window-buffer (git-review--control-window))
      (rename-buffer (git-review--review-buffer-name)))))

(defun git-review--parse-review-hunk (hunk)
  "Parse HUNK into a property list."
  (pcase-let ((`(,start ,length)
               (seq-map #'string-to-number (string-split hunk ","))))
    (if (or (not length))
        `(:begin ,start :end ,start)
      (unless (= length 0)
        `(:begin ,start :end ,(+ start (1- length)))))))

(defun git-review--diff-regions-intersect-p (regions1 regions2)
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

(defun git-review--file-differences-intersect-p (file-diffs1 file-diffs2)
  "Return t if FILE-DIFFS1 intersects with FILE-DIFFS2."
  (or (git-review--diff-regions-intersect-p (alist-get 'a file-diffs1)
                                              (alist-get 'a file-diffs2))
      (git-review--diff-regions-intersect-p (alist-get 'b file-diffs1)
                                              (alist-get 'b file-diffs2))))

(defun git-review--current-git-branch ()
  "Return current branch name."
  (string-trim
   (with-temp-buffer
     (call-process-shell-command "git rev-parse --abbrev-ref HEAD" nil t)
     (buffer-string))))

(defun git-review--other-git-branches ()
  "Return list of local branch names, excluding the current branch."
  (let ((branches (split-string
                   (with-temp-buffer
                     (call-process-shell-command "git branch" nil t)
                     (buffer-string))
                   "\n" t)))
    (thread-last branches
                 (seq-remove (lambda (it) (string-prefix-p "*" it)))
                 (seq-map #'string-trim))))

(defun git-review--review-buffer-name ()
  "Return the name of the review buffer."
  (let* ((review-files (thread-last (git-review--files)
                                    (seq-remove #'git-review--ignore-file-p)))
         (file-index
          (if (git-review--ignore-file-p (git-review--current-file))
              "_"
            (seq-position review-files (git-review--current-file))))
         (number-of-files (1- (seq-length review-files)))
         (progress (* (git-review--progress) 100)))
    (format "*Ediff Review: [%s/%s] %s%%*"
            file-index
            number-of-files
            progress)))

(defun git-review--create-comment ()
  "Create a new comment and return it."
  (let* ((id (intern (secure-hash 'md5 (number-to-string (time-to-seconds)))))
         (side (if (eq (current-buffer) git-review-base-revision-buffer) 'a 'b))
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

(defun git-review--remove-file-comment-overlays ()
  "Remove all overlays in the comments.

This is required since we can't serialize the overlays and store them
in the database.  Plus storing them doesn't make sense."
  (let* ((comments (let-alist (git-review--file-info) .comments)))
    (seq-do (lambda (it)
              (pcase-let ((`(,id . ,comment) it))
                (setf comment (assoc-delete-all 'header-overlay comment))
                (setf comment (assoc-delete-all 'comment-overlay comment))
                (git-review--update-comments id comment)))
            comments)))

(defun git-review--add-comment-overlay ()
  "Add a comment overlay."
  (let-alist git-review--current-comment
    (with-current-buffer (if (eq .side 'a) git-review-base-revision-buffer git-review-current-revision-buffer)
      (unless .comment-overlay
        (let* ((ov (or .comment-overlay (make-overlay .location.start-point .location.end-point))))
          (setf (alist-get 'comment-overlay git-review--current-comment) ov)
          (overlay-put ov 'git-review-comment .id)
          (overlay-put ov 'face 'ansi-color-fast-blink)))
      (git-review--add-comment-header-overlay))))

(defun git-review--add-comment-header-overlay ()
  "Add a comment header overlay."
  (let-alist git-review--current-comment
    (with-current-buffer (if (eq .side 'a) git-review-base-revision-buffer git-review-current-revision-buffer)
      (save-excursion
        (goto-char .location.start-point)
        (beginning-of-line)
        (let* ((ov (make-overlay (point) (point)))
               (time (let-alist git-review--current-comment
                       (when .timestamp
                         (format-time-string "%Y-%m-%d %a %H:%M:%S" .timestamp))))
               (comment-message (let-alist git-review--current-comment .message))
               (summary (seq-elt (split-string comment-message "\n") 0))
               (summary-str (truncate-string-to-width summary 30))
               (comment-header
                (concat git-review-user ": " summary-str (when (> (length summary) 30) "...") (when time " " time) "\n")))
          (when .header-overlay
            (delete-overlay .header-overlay))
          (setf (alist-get 'header-overlay git-review--current-comment) ov)
          (overlay-put ov 'git-review-comment .id)
          (overlay-put ov 'before-string (propertize comment-header 'face 'git-review-comment-header)))))))

(defun git-review--restore-comment-overlays ()
  "Restore comment overlays in the current file."
  (when (git-review--has-comments-p (git-review--current-file))
      (seq-do (lambda (it)
                (let ((comment (cdr it)))
                  (setq git-review--current-comment comment)
                  (let-alist git-review--current-comment
                    (setf (alist-get 'header-overlay git-review--current-comment) nil)
                    (setf (alist-get 'comment-overlay git-review--current-comment) nil)
                    (git-review--add-comment-overlay)
                    (git-review--update-comments .id git-review--current-comment))
                  (setq git-review--current-comment nil)))
              (let-alist (git-review--file-info) .comments))))

(defun git-review--next-file ()
  "Return next file."
  (thread-last (let-alist git-review--patchset .files)
               (seq-drop-while (lambda (it)
                                 (let-alist it
                                   (not (string= .filename
                                                 (git-review--current-file))))))
               (seq-rest)
               (seq-find (lambda (it)
                           (let-alist it .filename
                             (not .ignore))))
               (funcall (lambda (it)
                          (let-alist it .filename)))))

(defun git-review--previous-file ()
  "Return previous file."
  (thread-last (let-alist git-review--patchset .files)
               (seq-take-while (lambda (it)
                                 (let-alist it
                                   (not (string= .filename
                                                 (git-review--current-file))))))
               (nreverse)
               (seq-find (lambda (it)
                           (let-alist it .filename
                                      (not .ignore))))
               (funcall (lambda (it)
                          (let-alist it .filename)))))

(defun git-review--next-comment ()
  "Return next comment."
  (when-let* ((comments (let-alist (git-review--file-info) .comments))
              (next-comment-id (thread-last comments
                                            (seq-map (lambda (it) (let-alist (cdr it) (cons (- .location.start-point (point)) .id))))
                                            (seq-sort-by (lambda (it) (car it)) #'<)
                                            (seq-find (lambda (it) (> (car it) 0)))
                                            (cdr))))
    (alist-get next-comment-id comments)))

(defun git-review--previous-comment ()
  "Return previous comment."
  (when-let* ((comments (let-alist (git-review--file-info) .comments))
              (previous-comment-id (thread-last comments
                                            (seq-map (lambda (it) (let-alist (cdr it) (cons (- .location.start-point (point)) .id))))
                                            (seq-sort-by (lambda (it) (car it)) #'<)
                                            (seq-find (lambda (it) (< (car it) 0)))
                                            (cdr))))
    (alist-get previous-comment-id comments)))

(defun git-review--annotations (candidates)
  "Return annotations of CANDIDATES."
  (thread-last candidates
               (seq-map (lambda (candidate)
                          (cons (car candidate)
                                (thread-last git-review--annotation-config
                                             (seq-map (lambda (config)
                                                        `(,(plist-get config :name) .
                                                          ,(funcall (plist-get config :function) candidate))))))))))

(defun git-review--annotation-widths ()
  "Return widths of annotations."
  (seq-map (lambda (config)
             `(,(plist-get config :name) .
               ,(thread-last git-review--annotations
                             (seq-map #'cdr)
                             (seq-map (lambda (it) (length (alist-get (plist-get config :name) it))))
                             (funcall (lambda (it)
                                        (if-let ((max-width (plist-get config :width)))
                                            (min (seq-max it) max-width)
                                          (seq-max it)))))))
           git-review--annotation-config))

(defun git-review--annotation-function (candidate)
  "Return annotation for CANDIDATE."
  (string-join
   (thread-last git-review--annotation-config
                (seq-map (lambda (config)
                           (setq test-annotation (alist-get candidate git-review--annotations nil nil #'equal))
                           (setq test-config config)
                           (when-let* ((annotation (alist-get candidate git-review--annotations nil nil #'equal))
                                       (padding 3)
                                       (str (alist-get (plist-get config :name) annotation))
                                       (width (alist-get (plist-get config :name) git-review--annotation-widths))
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

(defun git-review--annotation-file-type (entry)
  "Return ENTRY's type."
  (let-alist (cdr entry)
    (pcase .type
      ("A" "ADDED")
      ("D" "DELETED")
      ("M" "MODIFIED")
      (_ "RENAMED"))))

(defun git-review--annotation-file-reviewed (entry)
  "Return ENTRY's review status."
  (let-alist (cdr entry)
    (if .reviewed
        "REVIEWED"
      "")))

(defun git-review--annotation-file-ignored (entry)
  "Return ENTRY's ignore status."
  (let-alist (cdr entry)
    (if .ignore
        "IGNORED"
      "")))

(defun git-review--annotation-file-comments (entry)
  "Return ENTRY's comments status."
  (let-alist (cdr entry)
    (if .comments
        (format "COMMENTS(%s)" (length .comments))
      "")))

;;;; Major modes

(defvar git-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'git-review-jump-to-a)
    (define-key map (kbd "b") #'git-review-jump-to-b)
    (define-key map (kbd "d") #'git-review-open-patchset-diff)
    (define-key map (kbd "f") #'git-review-select-file)
    (define-key map (kbd "ga") #'ediff-jump-to-difference-at-point)
    (define-key map (kbd "gb") #'ediff-jump-to-difference-at-point)
    (define-key map (kbd "q") #'git-review-quit)
    (define-key map (kbd "S") #'git-review-publish-review)
    (define-key map (kbd "n") #'git-review-next-hunk)
    (define-key map (kbd "N") #'git-review-next-comment)
    (define-key map (kbd "p") #'git-review-previous-hunk)
    (define-key map (kbd "P") #'git-review-previous-comment)
    (define-key map (kbd "]") #'git-review-next-file)
    (define-key map (kbd "[") #'git-review-previous-file)
    (define-key map (kbd "^") #'git-review-switch-to-most-recent-file)
    map))

(define-derived-mode git-review-mode fundamental-mode "Ediff Review"
  (read-only-mode)
  (rename-buffer
   (git-review--review-buffer-name)))

(defvar git-review-conversation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'git-review-toggle-conversation)
    map))

(define-derived-mode git-review-conversation-mode fundamental-mode "Ediff Review Conversation"
  (setq-local mode-line-format nil)
  (read-only-mode))

;;;; Minor modes

(define-minor-mode git-review-comment-mode
  "Mode for `git-review' comment."
  :global nil
  :lighter " Ediff Review Comment"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'git-review-complete-comment)
            (define-key map (kbd "C-c C-k") #'git-review-quit-comment)
            map))

(define-minor-mode git-review-minor-mode
  "Minor mode for `git-review'."
  :global nil
  :lighter "Ediff Review"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'git-review-jump-to-control)
            (define-key map (kbd "C-c C-'") #'git-review-comment)
            (define-key map (kbd "C-c C-k") #'git-review-kill-comment)
            (define-key map (kbd "<tab>") #'git-review-toggle-conversation)
            map))

(provide 'git-review)

;;; git-review.el ends here