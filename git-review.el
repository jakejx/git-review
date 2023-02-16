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

(defcustom git-review-config nil
  "A function which returns a review configuration."
  :group 'git-review
  :type 'symbol)

(defcustom git-review-change-annotation
  '((:name patchsets :function (lambda (elt) (format "PATCHSETS(%s)" (seq-length (plist-get (cdr elt) :patchsets)))) :face 'font-lock-comment-face)
    (:name author :function (lambda (elt) (plist-get (seq-first (plist-get (cdr elt) :patchsets)) :author)) :face 'font-lock-comment-face))
  "A list of annotations to display for a change."
  :group 'git-review
  :type 'symbol)

(defcustom git-review-patchset-annotation
  '((:name conversations :function git-review--annotation-patchset-conversations :face 'font-lock-comment-face)
    (:name author :function git-review--annotation-patchset-author :face 'font-lock-comment-face)
    (:name progress :function git-review--annotation-patchset-progress :face 'font-lock-comment-face))
  "A list of annotations to display for a patchset."
  :group 'git-review
  :type 'symbol)

(defcustom git-review-file-annotation
  '((:name type :function git-review--annotation-file-type :face 'font-lock-comment-face)
    (:name reviewed :function git-review--annotation-file-reviewed :face 'font-lock-string-face)
    (:name ignored :function git-review--annotation-file-ignored :face 'font-lock-string-face)
    (:name comments :function git-review--annotation-file-conversations :face 'font-lock-string-face))
  "A list of annotations to display for a review file."
  :group 'git-review
  :type 'symbol)

(defcustom git-review-conversation-annotation
  '((:name file :function git-review--annotation-conversation-file :face 'font-lock-string-face)
    (:name user :function git-review--annotation-conversation-starter :face 'font-lock-string-face)
    (:name location :function git-review--annotation-conversation-location :face 'font-lock-string-face)
    (:name comments :function git-review--annotation-conversation-comments :face 'font-lock-string-face)
    (:name draft :function git-review--annotation-conversation-draft :face 'font-lock-string-face)
    (:name resolved :function git-review--annotation-conversation-resolved :face 'font-lock-string-face))
  "A list of annotations to display for a review conversation."
  :group 'git-review
  :type 'symbol)

(defcustom git-review-ignore-file-predicates '(git-review-rebased-file-p)
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

(defvar git-review--changes nil "List of changes.")
(defvar git-review--change nil "The current change.")
(defvar git-review--patchset nil "The current patchset.")
(defvar git-review--conversations nil "List of conversations.")
(defvar git-review--files nil "List of files.")
(defvar git-review--config nil "Configuration of current review.")
(defvar git-review--hide-other-conversations nil)
(defvar git-review--conversation-frame nil)

(defvar-local git-review--current-comment nil)
(defvar-local git-review--current-conversation nil)

;; Used for completion
(defvar git-review--candidates nil)
(defvar git-review--annotations nil)
(defvar git-review--annotation-widths nil)
(defvar git-review--annotation-config nil)

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
  (unless git-review--changes
    (let ((db (expand-file-name "git-review.db" git-review-database-dir)))
      (if (file-exists-p db)
          (setq git-review--changes
                (with-temp-buffer
                  (insert-file-contents db)
                  (goto-char (point-min))
                  (read (current-buffer))))
        (make-empty-file db t)
        (setq git-review--changes nil)))))

(defun git-review-update-db ()
  "Update the review database."
  (let ((db (expand-file-name "git-review.db" git-review-database-dir)))
    (with-temp-file db
      (prin1 git-review--changes (current-buffer)))))

(defun git-review--update-review ()
  "Update change(s) with change."
  (unless (plist-get git-review--config :wip)
    (git-review--store-buffer-locations)
    (git-review--update-patchset-files git-review--files)
    (git-review--update-patchsets git-review--patchset)

    ;; Remove remote conversations from local storage
    (setq git-review--change
          (plist-put git-review--change :conversations
                     (seq-remove (lambda (it)
                                   (plist-get it :remote))
                                 git-review--conversations)))

    (git-review--update-changes git-review--change)
    (git-review-update-db))

  ;; Reset variables
  (setq git-review--change nil)
  (setq git-review--patchset nil)
  (setq git-review--conversations nil)
  (setq git-review--files nil)
  (setq git-review--config nil)
  (setq git-review--hide-other-conversations nil))

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
         (plist-get
          (seq-elt (git-review--get-files) 0)
          :filename)))
      (git-review-file))))

(defun git-review-setup-project-file-review (file)
  "Setup `git-review' for project FILE review."
  (setq git-review--files (plist-put git-review--files :current-file file))
  (let* ((default-directory (git-review--project-root))
         (file-info (git-review--file-info))
         (type (plist-get file-info :type)))
    (git-review--setup-buffers)
    (if (string= (git-review--current-file) "COMMIT_MSG")
        (progn
          (when (string-equal "M" type)
            (git-review--commit-message (git-review--base-revision git-review--patchset)
                                        git-review-base-revision-buffer))
          (git-review--commit-message (git-review--current-revision git-review--patchset)
                                      git-review-current-revision-buffer))
      (unless (string-equal "A" type)
        (git-review--file-content (git-review--base-revision git-review--patchset)
                                  (or (plist-get file-info :original-filename)
                                      (plist-get file-info :filename))
                                  git-review-base-revision-buffer))
      (unless (string-equal "D" type)
        (git-review--file-content (git-review--current-revision git-review--patchset)
                                  file
                                  git-review-current-revision-buffer t)))))

(defun git-review-file ()
  "Review current file."
  (cl-letf* (((symbol-function #'ediff-mode) (lambda () (git-review-mode)))
             ((symbol-function #'ediff-set-keys) #'ignore)
             (default-directory (git-review--project-root)))
    (ediff-buffers git-review-base-revision-buffer git-review-current-revision-buffer)
    (git-review--init-conversation-overlays)
    (git-review--restore-buffer-location (git-review--get-file (git-review--current-file)))))

(defun git-review-close-review-file ()
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
              (when (string-match (rx bol "*" (or "ediff" "Git" "Git-Review")) (buffer-name it))
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

(defun git-review--get-patchset-file (patchset filename)
  "Get file with FILENAME from PATCHSET."
  (seq-find (lambda (file)
              (equal (plist-get file :filename)
                     filename))
            (git-review--get-patchset-files
             (git-review--patchset-id patchset))))

(defun git-review--get-patchset-files (patchset-id)
  "Get files from PATCHSET-ID."
  (plist-get
   (seq-find (lambda (it)
               (equal (plist-get it :id) patchset-id))
             (plist-get git-review--change :files))
   :files))

(defun git-review--remove-unchanged-files (files)
  "Remove FILES that are not changed in patchset or base-patchset."
  (let* ((patchset git-review--patchset)
         (patchset-files (seq-map (lambda (file)
                                    (plist-get file :filename))
                                  (git-review--get-patchset-files
                                   (number-to-string (plist-get patchset :number)))))
         (base-patchset (git-review--base-patchset patchset)))
    (if base-patchset
        (progn
          (setq patchset-files `(,@patchset-files
                                 ,@(seq-map (lambda (file)
                                              (plist-get file :filename))
                                            (git-review--get-patchset-files
                                             (number-to-string (plist-get base-patchset :number))))))
          (seq-filter (lambda (file)
                        (member (plist-get file :filename) patchset-files))
                      files))
      files)))

(defun git-review--detect-rebased-files (files)
  "Add rebase property to FILES."
  (if (plist-get git-review--patchset :base-patchset)
      (seq-map (lambda (file)
                 (plist-put file :rebased
                            (git-review-file-rebased-p file)))
               files)
    files))

(defun git-review-rebased-file-p (file)
  "Return t if FILE is rebased."
  (plist-get file :rebased))

(defun git-review-file-rebased-p (file)
  "Return t if FILE is changed due to a rebase."
  (unless (string= (plist-get file :filename) "COMMIT_MSG")
    (let* ((base-patchset (git-review--base-patchset git-review--patchset))
           (base-current-regions (git-review-hunk-regions (plist-get base-patchset :commit-hash)
                                                          (plist-get git-review--patchset :commit-hash)
                                                          (or (plist-get file :original-filename)
                                                              (plist-get file :filename))
                                                          (plist-get file :filename)))
           (current-regions (git-review--current-revision-diff-regions file)))
      (if-let* ((base-regions (git-review--base-revision-diff-regions file)))
          (and (not (git-review--file-differences-intersect-p base-current-regions
                                                              current-regions))
               (not (git-review--file-differences-intersect-p base-current-regions
                                                              base-regions)))
        (not (git-review--file-differences-intersect-p base-current-regions
                                                       current-regions))))))

;;;; Commands

(defun git-review-control-edit-comment ()
  "Edit comment from currently focused conversation."
  (interactive)
  (when-let ((conversation (git-review--control-conversation-at-point))
             (comment (seq-first (seq-reverse (plist-get conversation :comments)))))
    (git-review--edit-comment conversation comment)))

(defun git-review-control-kill-comment ()
  "Kill currently focused comment."
  (interactive)
  (when-let ((conversation (git-review--control-conversation-at-point)))
    (with-selected-window (get-buffer-window
                           (git-review--conversation-buffer
                            conversation))
      (git-review-kill-comment))))

(defun git-review-control-toggle-conversation ()
  "Toggle currently focused conversation."
  (interactive)
  (if (frame-live-p git-review--conversation-frame)
      (progn
        (make-frame-invisible git-review--conversation-frame t)
        (kill-buffer "*git-review-conversation*"))
    (when-let ((conversation (git-review--control-conversation-at-point)))
      (with-selected-window (get-buffer-window
                             (git-review--conversation-buffer
                              conversation))
        (git-review-toggle-conversation)))))

(defun git-review--control-conversation-at-point ()
  "Return conversation at point from control buffer."
  (when-let* ((conversations (seq-remove #'null
                                         `(,(with-selected-window (get-buffer-window git-review-base-revision-buffer)
                                              (git-review--conversation-at-point))
                                           ,(with-selected-window (get-buffer-window git-review-current-revision-buffer)
                                              (git-review--conversation-at-point))))))
    (if (= 1 (length conversations))
        (car conversations)
      ;; TODO(Niklas Eklund, 20230210): Should probably add side to
      ;; annotation to easier distinguish a comment, maybe line as
      ;; well?
      (git-review-completing-read (git-review--conversation-candidates conversations)
                                  "Select conversation: "
                                  'git-review-conversation
                                  git-review-conversation-annotation))))

(defun git-review-toggle-hide-conversations ()
  "Toggle to hide all conversations with others."
  (interactive)
  (setq git-review--hide-other-conversations
        (not git-review--hide-other-conversations))
  (when git-review--hide-other-conversations
    (git-review-file)))

(defun git-review--project-root ()
  "Return project root of the current review."
  (plist-get git-review--config :project-root))

(defun git-review-open-patchset-diff ()
  "Open diff buffer with patch-set."
  (interactive)
  (let* ((default-directory (git-review--project-root)))
    (vc-diff-internal t
                      (list (vc-responsible-backend default-directory) (list default-directory))
                      (git-review--base-revision git-review--patchset)
                      (git-review--current-revision git-review--patchset))))

(defun git-review-toggle-conversation ()
  "Toggle conversation."
  (interactive)
  (if (frame-live-p git-review--conversation-frame)
      (progn
        (make-frame-invisible git-review--conversation-frame t)
        (kill-buffer "*git-review-conversation*"))
    (when-let* ((conversation (git-review--conversation-at-point)))
      (let* ((parent-frame (window-frame))
             (child-frame (make-frame
                           `((parent-frame . ,parent-frame)
                             (minibuffer . ,(minibuffer-window parent-frame))
                             (child-frame-border-width . 1)
                             (visibility . nil)
                             (desktop-dont-save . t)
                             (tab-bar-lines . 0)
                             ;; TODO: Conditionally use scroll-bar if conversation is long
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
            (seq-do (lambda (comment)
                      (insert (propertize (format "%s:\n" (plist-get comment :user)) 'face 'org-block-begin-line))
                      (insert (with-temp-buffer
                                (insert (plist-get comment :message))
                                (gfm-view-mode)
                                (font-lock-ensure)
                                (buffer-substring (point-min) (point-max))))
                      (insert "\n"))
                    (plist-get conversation :comments))
            (goto-char (point-min)))
          (setq line-count (count-lines (point-min) (point-max) 'ignore-invisible-lines)))
        (set-window-buffer window buffer)
        (set-window-dedicated-p window t)
        (git-review--conversation-frame-configure-size child-frame conversation line-count)
        (make-frame-visible child-frame)))))

(defun git-review--conversation-frame-configure-size (frame conversation line-count)
  "Configure size of FRAME using info from CONVERSATION and LINE-COUNT."
  (save-excursion
    ;; TODO: Improve by determining position on screen to know if window should be up or down
    (goto-char (git-review--conversation-start-point conversation))
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
  (git-review--update-review)
  (git-review-close-review-file)
  (tab-bar-close-tab))

(defun git-review-next-conversation ()
  "Go to next conversation."
  (interactive)
  (if-let ((conversation (git-review--file-next-conversation
                          (git-review--current-file))))
      (git-review--move-to-conversation conversation)
    (if-let ((conversation (git-review--next-conversation-file
                            (git-review--current-file))))
        (progn (git-review--switch-file (plist-get conversation :filename))
               (git-review--move-to-conversation conversation))
      (message "No next conversation found"))))

(defun git-review-previous-conversation ()
  "Go to previous conversation."
  (interactive)
  (if-let ((conversation (git-review--file-previous-conversation
                          (git-review--current-file))))
      (git-review--move-to-conversation conversation)
    (if-let ((conversation (git-review--previous-conversation-file
                            (git-review--current-file))))
        (progn (git-review--switch-file (plist-get conversation :filename))
               (git-review--move-to-conversation conversation))
      (message "No previous conversation found"))))

(defun git-review--move-to-conversation (conversation)
  "Move to CONVERSATION."
  (git-review--restore-overlays)
  (with-selected-window (get-buffer-window git-review-current-revision-buffer)
    (goto-char (git-review--conversation-start-point conversation)))
  (with-selected-window (git-review--control-window)
    (let ((last-command-event ?b))
      (ediff-jump-to-difference-at-point nil))
    (git-review---maybe-modify-overlays))
  (with-selected-window (get-buffer-window git-review-current-revision-buffer)
    (goto-char (git-review--conversation-start-point conversation))
    (recenter))
  (with-selected-window (get-buffer-window git-review-base-revision-buffer)
    (goto-char (git-review--conversation-start-point conversation))
    (recenter)))

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
  (if-let ((next-file (git-review--next-file (git-review--current-file))))
      (git-review--switch-file next-file)
    (message "No next file")
    nil))

(defun git-review-previous-file ()
  "Review previous file."
  (interactive)
  (if-let ((previous-file (git-review--previous-file (git-review--current-file))))
      (git-review--switch-file previous-file)
    (message "No previous file")
    nil))

(defun git-review-select-file ()
  "Select a file to review."
  (interactive)
  (when-let* ((candidates (seq-map (lambda (file)
                                     `(,(plist-get file :filename) . ,file))
                                   (git-review--get-files)))
              (file (git-review-completing-read candidates
                                                "Select file: "
                                                'git-review-file
                                                git-review-file-annotation)))
    (git-review--switch-file (plist-get file :filename))))

;;;###autoload
(defun git-review-commit ()
  "Review the current commit."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (setq git-review--config `(:project-root ,default-directory
                                             :change-id ,(lambda () (git-review--commit-hash))
                                             :patchset ,(lambda () 1)
                                             :wip t))
    (git-review--initialize-review))
  (git-review-start-review))

(defun git-review-select-change ()
  "Select a change."
  (when-let* ((project-name (git-review--commit-project))
              (candidates (thread-last git-review--changes
                                       (seq-filter (lambda (change)
                                                     (equal (plist-get change :project)
                                                            project-name)))
                                       (seq-map (lambda (change)
                                                  (let ((patchset (car (last (plist-get change :patchsets)))))
                                                    `(,(plist-get patchset :subject) .
                                                      ,change))))))
              (change (git-review-completing-read candidates
                                                  "Select change: "
                                                  'git-review-change
                                                  git-review-change-annotation)))
    change))

(defun git-review-select-patchset ()
  "Select and switch to patchset."
  (interactive)
  (when-let* ((candidates (thread-last (plist-get git-review--change :patchsets)
                                       (seq-reverse)
                                       (seq-map (lambda (patchset)
                                                  `(,(number-to-string (plist-get patchset :number)) .
                                                    ,patchset)))))
              (patchset (git-review-completing-read candidates
                                                    "Select patchset: "
                                                    'git-review-patchset
                                                    git-review-patchset-annotation)))
    (git-review--store-buffer-locations)
    (git-review--update-patchset-files git-review--files)
    (git-review--update-patchsets git-review--patchset)
    (git-review-close-review-file)
    (git-review--initialize-patchset (plist-get patchset :number))
    (tab-bar-close-tab)
    (git-review-start-review)))

(defun git-review-select-base-patchset ()
  "Select the base patchset."
  (interactive)
  (when-let* ((patchset git-review--patchset)
              (candidates (thread-last (plist-get git-review--change :patchsets)
                                       (seq-remove (lambda (it)
                                                     (equal (plist-get it :number)
                                                            (plist-get git-review--patchset :number))))
                                       (seq-reverse)
                                       (seq-map (lambda (patchset)
                                                  `(,(number-to-string (plist-get patchset :number)) .
                                                    ,patchset)))))
              (base-patchset (git-review-completing-read candidates
                                                         "Select patchset: "
                                                         'git-review-patchset
                                                         git-review-patchset-annotation)))
    (git-review--store-buffer-locations)
    (git-review--update-patchset-files git-review--files)
    (git-review--update-patchsets git-review--patchset)
    (git-review-close-review-file)
    (git-review--initialize-patchset (plist-get patchset :number)
                                     (plist-get base-patchset :number))
    (tab-bar-close-tab)
    (git-review-start-review)))

(defun git-review-select-conversation ()
  "Select a conversation."
  (interactive)
  (when-let* ((candidates (git-review--conversation-candidates
                           (git-review--get-conversations)))
              (conversation (git-review-completing-read candidates
                                                        "Select conversation: "
                                                        'git-review-conversation
                                                        git-review-conversation-annotation)))
    (git-review--switch-file (plist-get conversation :filename))
    (git-review--move-to-conversation conversation)))

(defun git-review--conversation-candidates (conversations)
  "Return a list of candidates based on CONVERSATIONS."
  (thread-last conversations
               (seq-filter (lambda (conversation)
                             (seq-find (lambda (file)
                                         (equal (plist-get file :filename)
                                                (plist-get conversation :filename)))
                                       (git-review--get-files))))
               (seq-filter #'git-review--conversation-viewable-p)
               (seq-map (lambda (it)
                          `(,(git-review--conversation-summary it) . ,it)))))

(defun git-review--deduplicate-candidates (candidates)
  "De-duplicate CANDIDATES."
  (let* ((ht (make-hash-table :test #'equal :size (seq-length candidates))))
    (thread-last candidates
                 (seq-do (lambda (candidate)
                           (if-let* ((str (car candidate))
                                     (count (gethash str ht))
                                     (identifier (propertize
                                                  (format " (%s)" (puthash str (1+ count) ht)) 'face 'font-lock-comment-face)))
                               (setcar candidate (concat str identifier))
                             (puthash (car candidate) 0 ht)))))))

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

(defun git-review-completing-read (candidates prompt category annotation-config &optional multiple)
  "Select CANDIDATES from CATEGORY with and PROMPT.

Optionally if MULTIPLE is t use `completing-read-multiple'."
  (when-let* ((deduplicated-candidates (git-review--deduplicate-candidates candidates))
              (git-review--candidates (git-review--harmonize-candidate-lengths deduplicated-candidates))
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
              (candidate (if multiple
                             (completing-read-multiple prompt collection nil t)
                           (completing-read prompt collection nil t))))
    (if (listp candidate)
        (seq-map (lambda (it)
                   ;; Completing-read multiple strips padding on all but last candidate
                   (let ((it-without-spaces (string-trim it)))
                     (thread-last git-review--candidates
                                  (seq-find (lambda (it2)
                                              (string-equal it-without-spaces
                                                            (string-trim (car it2)))))
                                  (cdr))))
                 candidate)
      (cdr (assoc candidate git-review--candidates)))))

(defun git-review-switch-to-most-recent-file ()
  "Switch to most recently reviewed file."
  (interactive)
  (when-let* ((recent-file (git-review--most-recent-file)))
    (setq git-review--patchset
          (plist-put git-review--patchset :recent-file (git-review--current-file)))
    (git-review--store-buffer-locations)
    (git-review-close-review-file)
    (git-review-setup-project-file-review recent-file)
    (git-review-file)))

;;;###autoload
(defun git-review-change ()
  "Review a new change."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (setq git-review--config (when (functionp git-review-config)
                               (plist-put (funcall git-review-config)
                                          :project-root (project-root
                                                         (project-current)))))
    (git-review--initialize-review)
    (git-review-start-review)))

;;;###autoload
(defun git-review-patchset ()
  "Review current patchset."
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (setq git-review--config (when (functionp git-review-config)
                               (plist-put (funcall git-review-config)
                                          :project-root (project-root
                                                         (project-current)))))
    (git-review--initialize-review)
    (git-review-start-review)))

(defun git-review-submit-review ()
  "Submit review."
  (interactive)
  (if-let ((submit-function (plist-get git-review--config :submit)))
      (progn
        (funcall submit-function)
        (git-review-quit))
    (message "No submit function defined")))

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

(defun git-review--conversation-at-point ()
  "Return conversation at point."
  (when-let* ((overlays (overlays-at (point)))
              (overlay (seq-find (lambda (ov)
                                   (overlay-get ov 'git-review-conversation-id))
                                 overlays))
              (conversation-id (overlay-get overlay 'git-review-conversation-id)))
    (seq-find (lambda (conversation)
                (equal conversation-id (plist-get conversation :id)))
              git-review--conversations)))

(defun git-review-conversation-dwim ()
  "Continue on, or start a new, conversation."
  (interactive)
  (let* ((conversation
          (or (git-review--conversation-at-point)
              (git-review--create-conversation)))
         (comment (or (seq-first (seq-reverse (plist-get conversation :comments)))
                      `(:user ,git-review-user
                              :draft t
                              :id ,(intern (secure-hash 'md5 (number-to-string (time-to-seconds))))))))
    (git-review--edit-comment conversation comment)))

(defun git-review-complete-comment ()
  "Complete the review comment."
  (interactive)
  (let* ((comment (plist-put git-review--current-comment :message
                             (buffer-substring-no-properties (point-min) (point-max))))
         (conversation (git-review--update-conversation-comment git-review--current-conversation
                                                                comment)))
    (git-review--update-conversations conversation)
    (git-review--add-conversation-overlays conversation))
  (quit-restore-window))

(defun git-review-kill-comment ()
  "Kill comment at point."
  (interactive)
  (when-let* ((conversation (git-review--conversation-at-point)))
    (seq-do (lambda (overlay)
              (delete-overlay overlay))
            (git-review--conversation-overlays conversation))
    (setq git-review--conversations
          (seq-remove (lambda (it)
                        (equal (plist-get it :id)
                               (plist-get conversation :id)))
                      git-review--conversations))))

(defun git-review-quit-comment ()
  "Quit review comment."
  (interactive)
  (quit-restore-window
   (get-buffer-window (current-buffer))
   'kill))

;;;; Support functions

(defun git-review--edit-comment (conversation comment)
  "Edit COMMENT from CONVERSATION."
  (let ((buffer (get-buffer-create "*git-review-comment*")))
    (display-buffer buffer git-review-comment-buffer-action)
    (with-current-buffer buffer
      (erase-buffer)
      (when-let ((is-draft (plist-get comment :draft))
                 (message (plist-get comment :message)))
        (insert message))
      (when git-review-comment-major-mode
        (funcall git-review-comment-major-mode))
      (git-review-comment-mode)
      (setq git-review--current-conversation conversation)
      (setq git-review--current-comment comment)
      (select-window (get-buffer-window (current-buffer)))
      (goto-char (point-max)))))

(defun git-review--commit-author ()
  "Return the name of the commit author."
  (with-temp-buffer
    (call-process-shell-command "git show --pretty=format:%cn --no-patch" nil t)
    (buffer-string)))

(defun git-review--commit-project ()
  "Return name of current project."
  (with-temp-buffer
    (call-process-shell-command "git config --local remote.origin.url" nil t)
    (string-trim
     (car
      (last
       (split-string (buffer-string) "/" t))))))

(defun git-review--commit-parent-hash ()
  "Return parent commit hash."
  (with-temp-buffer
    (call-process-shell-command "git show --pretty=format:%P --no-patch" nil t)
    (buffer-string)))

(defun git-review--commit-hash ()
  "Return current commit hash."
  (with-temp-buffer
    (call-process-shell-command "git show --pretty=format:%H --no-patch" nil t)
    (buffer-string)))

(defun git-review--commit-subject ()
  "Return subject of current commit."
  (with-temp-buffer
    (call-process-shell-command "git show --pretty=format:%s --no-patch" nil t)
    (buffer-string)))

(defun git-review--update-conversation-comment (conversation comment)
  "Update CONVERSATION with COMMENT."
  (let* ((found-comment)
         (comments (seq-map (lambda (it)
                              (if (equal (plist-get it :id) (plist-get comment :id))
                                  (progn
                                    (setq found-comment t)
                                    comment)
                                it))
                            (plist-get conversation :comments))))
    (plist-put conversation :comments
               (if found-comment
                   comments
                 (append comments
                         `(,comment))))))

(defun git-review--update-changes (change)
  "Update CHANGE."
  (setq git-review--changes
        (seq-map (lambda (it)
                   (if (equal (plist-get it :id) (plist-get change :id))
                       change
                     it))
                 git-review--changes)))

(defun git-review--update-conversations (conversation)
  "Update conversations with CONVERSATION."
  (let* ((found-conversation)
         (conversations
          (seq-map (lambda (it)
                     (if (equal (plist-get it :id) (plist-get conversation :id))
                         (progn
                           (setq found-conversation t)
                           conversation)
                       it))
                   git-review--conversations)))
    (setq git-review--conversations
          (if found-conversation
              conversations
            (append
             conversations
             `(,conversation))))))

(defun git-review--update-patchsets (patchset)
  "Update patch-sets with PATCHSET."
  (let* ((updated-patchsets
          (seq-map (lambda (it)
                     (if (equal (plist-get it :number) (plist-get patchset :number))
                         patchset
                       it))
                   (plist-get git-review--change :patchsets))))
    (setq git-review--change
          (plist-put git-review--change :patchsets updated-patchsets))))

(defun git-review--add-patchset (patchset)
  "Add PATCHSET to current change."
  (let* ((patchsets (append
                     (plist-get git-review--change :patchsets)
                     `(,patchset)))
         (sorted-patchsets (seq-sort (lambda (a b)
                                       (< (plist-get a :number)
                                          (plist-get b :number)))
                                     patchsets)))
    (setq git-review--change (plist-put git-review--change :patchsets sorted-patchsets))))

(defun git-review--add-patchset-files (files)
  "Add patchset FILES."
  (let* ((change-files (plist-get git-review--change :files))
         (updated-files (append
                         change-files
                         `(,files))))
    (setq git-review--change (plist-put git-review--change :files updated-files))))

(defun git-review--conversation-overlays (conversation)
  "Return overlays associated with CONVERSATION."
  (when-let* ((conversation-id (plist-get conversation :id))
              (buffer-overlays (overlays-in (point-min) (point-max))))
    (seq-filter (lambda (overlay)
                  (equal (overlay-get overlay 'git-review-conversation-id)
                         conversation-id))
                buffer-overlays)))

(defun git-review--switch-file (file)
  "Switch to FILE."
  (setq git-review--patchset
        (plist-put git-review--patchset :recent-file (git-review--current-file)))
  (git-review--store-buffer-locations)
  (git-review-close-review-file)
  (git-review-setup-project-file-review file)
  (git-review-file))

(defun git-review--current-revision (patchset)
  "Return the current hash of PATCHSET."
  (plist-get patchset :commit-hash))

(defun git-review--base-revision (patchset)
  "Return the base revision for PATCHSET."
  (if-let* ((base-patchset (git-review--base-patchset patchset)))
      (plist-get base-patchset :commit-hash)
    (plist-get git-review--patchset :parent-hash)))

(defun git-review--base-patchset (patchset)
  "Return the base patchset of PATCHSET."
  (when-let ((base-patchset-number (plist-get patchset :base-patchset)))
    (seq-find (lambda (it)
                (equal base-patchset-number
                       (plist-get it :number)))
              (plist-get git-review--change :patchsets))))

(defun git-review--has-comments-p (file)
  "Return t if FILE has comments."
  (let-alist (git-review--file-info file)
    (not (null .comments))))

(defun git-review--is-reviewed-p (file)
  "Return t if FILE is reviewed."
  (plist-get (seq-find (lambda (it)
                         (equal file (plist-get it :filename)))
                       (git-review--get-files))
             :reviewed))

(defun git-review--update-file (file)
  "Update files with FILE."
  (git-review--update-files
   (seq-map (lambda (it)
              (if (equal (plist-get it :filename)
                         (plist-get file :filename))
                  file
                it))
            (git-review--get-files))))

(defun git-review--get-file (filename)
  "Return file with FILENAME."
  (seq-find (lambda (it)
              (equal (plist-get it :filename)
                     filename))
            (git-review--get-files)))

(defun git-review--get-conversations ()
  "Return conversations."
  (if git-review--hide-other-conversations
      (seq-filter (lambda (it)
                    (let ((comments (plist-get it :comments)))
                      (and (= (length comments) 1)
                           (string= (plist-get (car comments) :user)
                                    git-review-user))))
                  git-review--conversations)
    git-review--conversations))

(defun git-review--get-files ()
  "Return files."
  (plist-get git-review--files :files))

(defun git-review--update-files (files)
  "Update review with FILES."
  (setq git-review--files
        (plist-put git-review--files :files files)))

(defun git-review--update-patchset-files (files)
  "Update patchset FILES."
  (let ((updated-files (seq-map (lambda (it)
                                  (if (equal (plist-get it :id) (plist-get files :id))
                                      files
                                    it))
                                (plist-get git-review--change :files))))
    (setq git-review--change
          (plist-put git-review--change :files updated-files))))

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

(defun git-review--current-file ()
  "Return the name of the current file being reviewed."
  (plist-get git-review--files :current-file))

(defun git-review--most-recent-file ()
  "Return the name of the most recently reviewed file."
  (plist-get git-review--patchset :recent-file))

(defun git-review--progress (files)
  "Return review progress of FILES."
  (if-let* ((interesting-files (seq-remove (lambda (it)
                                             (plist-get it :ignore))
                                           files))
            (reviewed-files (seq-filter (lambda (it)
                                          (plist-get it :reviewed))
                                        interesting-files)))
      (* (/ (float (length reviewed-files)) (length interesting-files)) 100)
    0.0))

(defun git-review--current-revision-diff-regions (file)
  "Return diff regions for FILE from current revision."
  (git-review-hunk-regions (plist-get git-review--patchset :commit-hash)
                           (plist-get git-review--patchset :parent-hash)
                           (or (plist-get file :original-filename)
                               (plist-get file :filename))
                           (plist-get file :filename)))

(defun git-review--base-revision-diff-regions (file)
  "Return diff regions for FILE from current revision."
  (when-let* ((base-patchset (git-review--base-patchset git-review--patchset))
              (base-file (git-review--get-patchset-file base-patchset
                                                        (or (plist-get file :original-filename)
                                                            (plist-get file :filename)))))
    (git-review-hunk-regions (plist-get base-patchset :parent-hash)
                             (plist-get base-patchset :commit-hash)
                             (or (plist-get base-file :original-filename)
                                 (plist-get base-file :filename))
                             (plist-get base-file :filename))))

(defun git-review--file-info (&optional file)
  "Info about FILE."
  (let ((file (or file (plist-get git-review--files :current-file)))
        (files (git-review--get-files)))
    (seq-find (lambda (it) (equal (plist-get it :filename) file)) files)))

(defun git-review--store-buffer-locations ()
  "Store locations in review buffers for current review file."
  (git-review--update-file
   (plist-put (git-review--get-file (git-review--current-file)) :buffer-location
              `((a . ,(with-current-buffer git-review-base-revision-buffer (point)))
                (b . ,(with-current-buffer git-review-current-revision-buffer (point)))))))

(defun git-review--initialize-review (&optional change-id patchset-number)
  "Initialize review of CHANGE-ID's PATCHSET-NUMBER."
  (let* ((change-id (or change-id
                        (funcall (plist-get git-review--config :change-id))))
         (patchset-number (or patchset-number
                              (funcall (plist-get git-review--config :patchset)))))

    ;; Change
    (if-let ((change (seq-find (lambda (it)
                                 (equal (plist-get it :id) change-id))
                               git-review--changes)))
        (setq git-review--change change)
      (setq git-review--change (git-review--create-change
                                change-id)))

    ;; Conversations
    (setq git-review--conversations
          (append (plist-get git-review--change :conversations)
                  (when-let ((remote-conversations-fun (plist-get git-review--config :remote-conversations)))
                    (funcall remote-conversations-fun))))
    ;; Patchset
    (git-review--initialize-patchset patchset-number)))

(defun git-review--initialize-patchset (patchset-number &optional base-patchset-number)
  "Initialize patchset with PATCHSET-NUMBER.

Optionally provide a BASE-PATCHSET-NUMBER."
  ;; Patchset
  (if-let ((patchset (seq-find (lambda (it)
                                 (equal (plist-get it :number) patchset-number))
                               (plist-get git-review--change :patchsets))))
      (setq git-review--patchset patchset)
    (setq git-review--patchset (git-review--create-patchset patchset-number)))
  (setq git-review--change
        (plist-put git-review--change :current-patchset
                   (plist-get git-review--patchset :number)))
  (setq git-review--patchset (plist-put git-review--patchset :base-patchset base-patchset-number))

  ;; Files
  (if-let ((files (git-review--get-patchset-files git-review--patchset)))
      (setq git-review--files files)
    (setq git-review--files (git-review--create-files git-review--patchset))
    (git-review--add-metadata-to-files)
    (git-review--add-ignore-tag-to-files)))

(defun git-review--patchset-id (patchset)
  "Return the ID of PATCHSET review."
  (concat (number-to-string (plist-get patchset :number))
          (when-let ((base (plist-get patchset :base-patchset)))
            (concat "-" (number-to-string base)))))

(defun git-review--create-change (change-id)
  "Create change with CHANGE-ID."
  (let ((change `(:id ,change-id
                      :current-patchset ,nil
                      :project ,(git-review--commit-project)
                      :conversations ,nil
                      :patchsets ,nil
                      :files ,nil)))
    (unless (plist-get git-review--config :wip)
      (git-review--add-change change))
    change))

(defun git-review--add-change (change)
  "Add CHANGE to list of change(s)."
  (setq git-review--changes
        (append git-review--changes
                `(,change))))

(defun git-review--create-files (patchset)
  "Create PATCHSET files."
  (let ((files `(:id ,(git-review--patchset-id patchset)
                     :current-file ,nil
                     :files ,(git-review--detect-rebased-files
                              (git-review--remove-unchanged-files
                               (git-review--generate-patchset-files
                                patchset))))))
    (git-review--add-patchset-files files)
    files))

(defun git-review--create-patchset (number)
  "Create patch-set with NUMBER."
  (let ((patchset `(:commit-hash ,(git-review--commit-hash)
                                 :parent-hash ,(git-review--commit-parent-hash)
                                 :subject ,(git-review--commit-subject)
                                 :author ,(git-review--commit-author)
                                 :number ,number)))
    (unless (plist-get git-review--config :wip)
      (git-review--add-patchset patchset))
    patchset))

(defun git-review--generate-patchset-files (patchset)
  "Return a list of files in patchset with PATCHSET."
  (let* ((files-in-patchset
          (split-string
           (string-trim
            (shell-command-to-string
             (format "git diff --name-status %s..%s"
                     (git-review--base-revision patchset)
                     (git-review--current-revision patchset))))
           "\n")))
    (append
     `((:filename "COMMIT_MSG" :type ,(if (plist-get patchset :base-patchset) "M" "A")))
     (seq-map (lambda (it)
                (let ((elements (split-string it)))
                  (pcase elements
                    (`(,type ,filename)
                     `(:filename ,filename
                                 :type ,type))
                    (`(,type ,base-filename ,filename)
                     `(:filename ,filename
                                 :original-filename ,base-filename
                                 :type ,type)))))
              files-in-patchset))))

(defun git-review--add-ignore-tag-to-files ()
  "Add ignore tag to files that should be ignored."
  (git-review--update-files
   (seq-map (lambda (it)
              (if (git-review--ignore-file-p it)
                  (plist-put it :ignore t)
                it))
            (git-review--get-files))))

(defun git-review--add-metadata-to-files ()
  "Add metadata to files in variable `git-review--files'."
  (git-review--update-files
   (seq-map (lambda (it)
              (if-let ((metadata (git-review--file-metadata it)))
                  (plist-put it :metadata metadata)
                it))
            (git-review--get-files))))

(defun git-review--restore-buffer-location (file)
  "Restore buffer location in FILE.

This is done for files that has already been reviewed before and where
there is a previous location to return to."
  (if-let* ((location (plist-get file :buffer-location)))
      (progn
        (with-selected-window (get-buffer-window git-review-current-revision-buffer)
          (goto-char (alist-get 'b location)))
        (with-selected-window (git-review--control-window)
          (let ((last-command-event ?b))
            (ediff-jump-to-difference-at-point nil))
          (git-review---maybe-modify-overlays))
        (with-selected-window (get-buffer-window git-review-current-revision-buffer)
          (goto-char (alist-get 'b location)))
        (with-selected-window (get-buffer-window git-review-base-revision-buffer)
          (goto-char (alist-get 'a location))))
    (with-selected-window (git-review--control-window)
      (git-review-next-hunk))))

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
                  (file-name-directory
                   (expand-file-name file (git-review--project-root))))
      (when (and set-filename
                 (equal (plist-get git-review--patchset :commit-hash)
                        (git-review--commit-hash))
                 (not (git-review--file-modified-p file)))
        (setq-local buffer-file-name
                    (expand-file-name file (git-review--project-root))))
      (git-review---enable-mode))
    (read-only-mode)))

(defun git-review--file-modified-p (file)
  "Return t if FILE is currently modified."
  (string-equal file
                (string-trim
                 (with-temp-buffer
                  (call-process-shell-command
                   (format "git diff --name-only --diff-filter=M %s" file)
                   nil t)
                  (buffer-string)))))

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
         (command (format "git show --pretty=format:\"%s\" --no-patch %s" pretty-format revision))
         (commit-header (with-temp-buffer
                          (call-process-shell-command command nil t)
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
    (setq git-review-base-revision-buffer
          (get-buffer-create (format "%s<%s>" (git-review--base-revision git-review--patchset)
                                     (file-name-nondirectory (or (plist-get file-info :original-filename)
                                                                 (plist-get file-info :filename))))))
    (setq git-review-current-revision-buffer
          (get-buffer-create (format "%s<%s>" (git-review--current-revision git-review--patchset)
                                     (file-name-nondirectory (plist-get file-info :filename)))))
    (with-current-buffer git-review-base-revision-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local default-directory (git-review--project-root))))
    (with-current-buffer git-review-current-revision-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local default-directory (git-review--project-root))))))

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
      (and
       (not
        (git-review--file-differences-intersect-p file-regions
                                                  (git-review--current-revision-diff-regions
                                                   (git-review--file-info (git-review--current-file)))))
       (not
        (git-review--file-differences-intersect-p file-regions
                                                  (git-review--base-revision-diff-regions
                                                   (git-review--file-info (git-review--current-file)))))))))

(defun git-review---maybe-modify-overlays ()
  "Maybe modify overlays if current diff is due to a rebase."
  (when-let* ((is-rebase-diff (and
                               (git-review--base-patchset git-review--patchset)
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
    (git-review--update-file
     (plist-put (git-review--get-file (git-review--current-file)) :reviewed t))
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
  (let* ((review-files (thread-last (git-review--get-files)
                                    (seq-remove (lambda (it)
                                                  (plist-get it :ignore)))))
         (file (git-review--get-file (git-review--current-file)))
         (file-index
          (if (plist-get file :ignore)
              "_"
            (seq-position review-files file (lambda (a b)
                                              (equal (plist-get a :filename)
                                                     (plist-get b :filename))))))
         (number-of-files (1- (seq-length review-files)))
         (progress (git-review--progress (git-review--get-files))))
    (format "*Git Review: %s [%s/%s] %s%%*"
            (concat (when-let ((number (plist-get git-review--patchset :base-patchset)))
                      (format "PS%s-" number))
                    (format "PS%s" (plist-get git-review--patchset :number)))
            file-index
            number-of-files
            (format "%.1f" progress))))

(defun git-review--create-conversation ()
  "Create a conversation."
  (let* ((start-position (min (mark) (point)))
         (end-position (max (mark) (point)))
         (location
          `((start-line . ,(save-excursion (goto-char start-position) (current-line)))
            (start-column . ,(save-excursion (goto-char start-position) (current-column)))
            (end-line . ,(save-excursion (goto-char end-position) (current-line)))
            (end-column . ,(save-excursion (goto-char end-position) (current-column)))))
         (patchset-side (git-review--buffer-representation (current-buffer))))
    `(:id ,(intern (secure-hash 'md5 (number-to-string (time-to-seconds))))
          :patchset ,(plist-get patchset-side :patchset)
          :filename ,(git-review--current-file)
          :resolved nil
          :location ,location
          :side ,(plist-get patchset-side :side))))

(defun git-review--buffer-representation (buffer)
  "Return the patchset and side representation of BUFFER."
  (if-let ((base-patchset (plist-get git-review--patchset :base-patchset)))
      `(:side b
              :patchset ,(if (eq buffer git-review-base-revision-buffer)
                             base-patchset
                           (plist-get git-review--patchset :number)))
    `(:side ,(if (eq buffer git-review-base-revision-buffer) 'a 'b)
            :patchset ,(plist-get git-review--patchset :number))))

(defun git-review--add-conversation-overlays (conversation)
  "Add overlays for CONVERSATION."
  ;; Region overlay
  (let* ((ov (git-review--get-conversation-region-overlay conversation)))
    (overlay-put ov 'git-review-conversation-id (plist-get conversation :id))
    (overlay-put ov 'git-review-overlay-type 'region)
    (overlay-put ov 'face 'ansi-color-fast-blink))

  ;; Header overlay
  (let* ((ov (git-review--get-conversation-header-overlay conversation))
         (first-comment (seq-first (plist-get conversation :comments)))
         (last-comment (car (last (plist-get conversation :comments))))
         (time (when (plist-get first-comment :timestamp)
                 (format-time-string "%Y-%m-%d %a %H:%M:%S" (plist-get first-comment :timestamp))))
         (draft (plist-get last-comment :draft))
         (summary (git-review--conversation-summary conversation))
         (comment-header
          (concat (plist-get first-comment :user) ": " summary (when time " " time) (when draft " DRAFT") "\n")))
    (overlay-put ov 'git-review-conversation-id (plist-get conversation :id))
    (overlay-put ov 'git-review-overlay-type 'header)
    (overlay-put ov 'before-string (propertize comment-header 'face 'git-review-comment-header))))

(defun git-review--get-conversation-region-overlay (conversation)
  "Return region overlay for CONVERSATION."
  (with-current-buffer (git-review--get-conversation-buffer conversation)
    (or (seq-find (lambda (it)
                    (eq 'region (overlay-get it 'git-review-overlay-type)))
                  (git-review--conversation-overlays conversation))
        (make-overlay (git-review--conversation-start-point conversation)
                      (git-review--conversation-end-point conversation)))))

(defun git-review--get-conversation-header-overlay (conversation)
  "Return header overlay for CONVERSATION."
  (with-current-buffer (git-review--get-conversation-buffer conversation)
    (save-excursion
      (goto-char (point-min))
      (forward-line (let-alist (plist-get conversation :location) .start-line))
      (beginning-of-line)
      (or (seq-find (lambda (it)
                      (eq 'header (overlay-get it 'git-review-overlay-type)))
                    (git-review--conversation-overlays conversation))
          (make-overlay (point) (point))))))

(defun git-review--get-conversation-buffer (conversation)
  "Get the buffer in which to display CONVERSATION."
  (if-let ((base-patchset (plist-get git-review--patchset :base-patchset)))
      (if (equal base-patchset (plist-get conversation :patchset))
          git-review-base-revision-buffer
        git-review-current-revision-buffer)
    (if (eq (plist-get conversation :side) 'a)
        git-review-base-revision-buffer
      git-review-current-revision-buffer)))

(defun git-review--init-conversation-overlays ()
  "Initialize overlays for conversations."
  (thread-last (git-review--file-conversations (git-review--current-file))
               (seq-filter #'git-review--conversation-viewable-p)
               (seq-do (lambda (conversation)
                         (git-review--add-conversation-overlays conversation)))))

(defun git-review--conversation-viewable-p (conversation)
  "Return t if CONVERSATION is viewable given current review setup."
  (let ((conversation-patchset (plist-get conversation :patchset)))
    (if-let ((base-patchset (plist-get git-review--patchset :base-patchset)))
        ;; Correct side and patchset
        (and (equal (plist-get conversation :side) 'b)
             (or (equal conversation-patchset
                        (plist-get git-review--patchset :number))
                 (equal conversation-patchset base-patchset)))
      ;; Correct patchset
      (equal
       conversation-patchset
       (plist-get git-review--patchset :number)))))

(defun git-review--conversation-summary (conversation)
  "Return summary string for CONVERSATION."
  (let* ((max-length 30)
         (first-comment (seq-first (plist-get conversation :comments)))
         (summary (seq-elt (split-string (plist-get first-comment :message) "\n") 0))
         (summary-str (truncate-string-to-width summary max-length)))
    (if (> (length summary) max-length)
        (concat summary-str "...")
      summary-str)))

(defun git-review--conversation-buffer (conversation)
  "Return buffer associated with CONVERSATION."
  (if (eq 'a (plist-get conversation :side))
      git-review-base-revision-buffer
    git-review-current-revision-buffer))

(defun git-review--next-conversation-file (file)
  "Return the next file after FILE with conversation(s)."
  (let ((conversation))
    (while (setq file (git-review--next-file file))
      (when-let ((conversations (git-review--review-conversations file)))
        (setq conversation (seq-first conversations))))
    conversation))

(defun git-review--previous-conversation-file (file)
  "Return the previous file before FILE with conversation(s)."
  (let ((conversation))
    (while (setq file (git-review--previous-file file))
      (when-let ((conversations (git-review--review-conversations file)))
        (setq conversation (seq-first conversations))))
    conversation))

(defun git-review--file-next-conversation (file)
  "Return next conversation in FILE."
  (let ((conversations (git-review--review-conversations file))
        (position (with-current-buffer git-review-current-revision-buffer
                    (point))))
    (thread-last conversations
                 (seq-map (lambda (it) `(,(- (git-review--conversation-start-point it) position) . ,it)))
                 (seq-sort-by (lambda (it) (car it)) #'<)
                 (seq-find (lambda (it) (> (car it) 0)))
                 (cdr))))

(defun git-review--file-previous-conversation (file)
  "Return next conversation in FILE."
  (let ((conversations (git-review--review-conversations file))
        (position (with-current-buffer git-review-current-revision-buffer
                    (point))))
    (thread-last conversations
                 (seq-map (lambda (it) `(,(- (git-review--conversation-start-point it) position) . ,it)))
                 (seq-sort-by (lambda (it) (car it)) #'>)
                 (seq-find (lambda (it) (< (car it) 0)))
                 (cdr))))

(defun git-review--conversation-start-point (conversation)
  "Return start point of CONVERSATION."
  (with-current-buffer (git-review--get-conversation-buffer conversation)
    (let-alist (plist-get conversation :location)
      (save-excursion (goto-char (point-min))
                      (forward-line .end-line)
                      (move-to-column .start-column)
                      (point)))))

(defun git-review--conversation-end-point (conversation)
  "Return end point of CONVERSATION."
  (with-current-buffer (git-review--get-conversation-buffer conversation)
    (let-alist (plist-get conversation :location)
      (save-excursion (goto-char (point-min))
                      (forward-line .end-line)
                      (move-to-column .end-column)
                      (point)))))

(defun git-review--file-conversations (file)
  "Return conversations on FILE."
  (seq-filter (lambda (it)
                (equal (plist-get it :filename) file))
              (git-review--get-conversations)))

(defun git-review--review-conversations (file)
  "Return conversations on current review FILE."
  (seq-filter #'git-review--conversation-viewable-p
              (git-review--file-conversations file)))

(defun git-review--next-file (file)
  "Return next file after FILE."
  (thread-last (git-review--get-files)
               (seq-drop-while (lambda (it)
                                 (not (string= (plist-get it :filename)
                                               file))))
               (seq-rest)
               (seq-find (lambda (it)
                           (not (plist-get it :ignore))))
               (funcall (lambda (it)
                          (plist-get it :filename)))))

(defun git-review--previous-file (file)
  "Return previous file before FILE."
  (thread-last (git-review--get-files)
               (seq-take-while (lambda (it)
                                 (let-alist it
                                   (not (string= (plist-get it :filename)
                                                 file)))))
               (nreverse)
               (seq-find (lambda (it)
                           (not (plist-get it :ignore))))
               (funcall (lambda (it)
                          (plist-get it :filename)))))

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

(defun git-review--annotation-conversation-starter (entry)
  "Return the name of the user that started the conversation in ENTRY."
  (plist-get (seq-first (plist-get (cdr entry) :comments)) :user))

(defun git-review--annotation-conversation-file (entry)
  "Return the name of the file the conversation in ENTRY."
  (plist-get (cdr entry) :filename))

(defun git-review--annotation-conversation-location (entry)
  "Return the location of the conversation in ENTRY."
  (format "%s:%s"
          (plist-get (cdr entry) :side)
          (let-alist (plist-get (cdr entry) :location) .start-line)))

(defun git-review--annotation-conversation-draft (entry)
  "Return draft status on conversation in ENTRY."
  (let ((last-comment (car (last (plist-get (cdr entry) :comments)))))
    (if (plist-get last-comment :draft)
        "DRAFT"
      "")))

(defun git-review--annotation-conversation-comments (entry)
  "Return number of replies to conversation in ENTRY."
  (let ((num-comments (seq-length (plist-get (cdr entry) :comments))))
    (if (> num-comments 1)
        (format "REPLIES(%s)" (- num-comments 1))
      "")))

(defun git-review--annotation-conversation-resolved (entry)
  "Return DONE if conversation in ENTRY is resolved."
  (if (plist-get (cdr entry) :resolved)
      "DONE"
    ""))

(defun git-review--annotation-file-type (entry)
  "Return ENTRY's type."
  (pcase (plist-get (cdr entry) :type)
    ("A" "ADDED")
    ("D" "DELETED")
    ("M" "MODIFIED")
    (_ "RENAMED")))

(defun git-review--annotation-file-reviewed (entry)
  "Return ENTRY's review status."
  (if (plist-get (cdr entry) :reviewed)
      "REVIEWED"
    ""))

(defun git-review--annotation-file-ignored (entry)
  "Return ENTRY's ignore status."
  (if (plist-get (cdr entry) :ignore)
      "IGNORED"
    ""))

(defun git-review--annotation-file-conversations (entry)
  "Return ENTRY's conversation status."
  (if-let* ((file (plist-get (cdr entry) :filename))
            (file-patchset-conversations
             (seq-filter (lambda (it)
                           (equal (plist-get it :patchset)
                                  (plist-get git-review--patchset :number)))
                         (git-review--file-conversations file))))
      (format "CONVERSATIONS(%s)" (length file-patchset-conversations))
    ""))

(defun git-review--annotation-patchset-conversations (entry)
  "Return ENTRY's conversation status."
  (if-let* ((patchset-number (plist-get (cdr entry) :number))
            (patchset-conversations (seq-filter (lambda (it)
                                                  (equal (plist-get it :patchset)
                                                         patchset-number))
                                                (git-review--get-conversations))))
      (format "CONVERSATIONS(%s)" (length patchset-conversations))
    ""))

(defun git-review--annotation-patchset-author (entry)
  "Return ENTRY's author."
  (plist-get (cdr entry) :author))

(defun git-review--annotation-patchset-progress (entry)
  "Return review progress of ENTRY."
  (if-let* ((patchset (cdr entry))
            (files
             (if (equal (plist-get git-review--files :id)
                        (git-review--patchset-id patchset))
                 git-review--files
               (seq-find (lambda (it)
                           (equal (plist-get it :id)
                                  (git-review--patchset-id patchset)))
                         (plist-get git-review--change :files)))))
      (format "%.1f%%" (git-review--progress (plist-get files :files)))
    "?"))

;;;; Major modes

(defvar git-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'git-review-jump-to-a)
    (define-key map (kbd "b") #'git-review-jump-to-b)
    (define-key map (kbd "B") #'git-review-select-base-patchset)
    (define-key map (kbd "ce") #'git-review-control-edit-comment)
    (define-key map (kbd "ch") #'git-review-toggle-hide-conversations)
    (define-key map (kbd "ck") #'git-review-control-kill-comment)
    (define-key map (kbd "cs") #'git-review-select-conversation)
    (define-key map (kbd "d") #'git-review-open-patchset-diff)
    (define-key map (kbd "f") #'git-review-select-file)
    (define-key map (kbd "ga") #'ediff-jump-to-difference-at-point)
    (define-key map (kbd "gb") #'ediff-jump-to-difference-at-point)
    (define-key map (kbd "q") #'git-review-quit)
    (define-key map (kbd "S") #'git-review-submit-review)
    (define-key map (kbd "n") #'git-review-next-hunk)
    (define-key map (kbd "N") #'git-review-next-conversation)
    (define-key map (kbd "p") #'git-review-previous-hunk)
    (define-key map (kbd "P") #'git-review-previous-conversation)
    (define-key map (kbd "]") #'git-review-next-file)
    (define-key map (kbd "[") #'git-review-previous-file)
    (define-key map (kbd "-") #'git-review-select-patchset)
    (define-key map (kbd "^") #'git-review-switch-to-most-recent-file)
    (define-key map (kbd "TAB") #'git-review-control-toggle-conversation)
    map))

(define-derived-mode git-review-mode fundamental-mode "Git Review"
  (read-only-mode)
  (rename-buffer
   (git-review--review-buffer-name)))

(defvar git-review-conversation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'git-review-toggle-conversation)
    map))

(define-derived-mode git-review-conversation-mode fundamental-mode "Git Review Conversation"
  (setq-local mode-line-format nil)
  (read-only-mode))

;;;; Minor modes

(define-minor-mode git-review-comment-mode
  "Mode for `git-review' comment."
  :global nil
  :lighter " Git Review Comment"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'git-review-complete-comment)
            (define-key map (kbd "C-c C-k") #'git-review-quit-comment)
            map))

(define-minor-mode git-review-minor-mode
  "Minor mode for `git-review'."
  :global nil
  :lighter "Git Review"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'git-review-jump-to-control)
            (define-key map (kbd "C-c C-'") #'git-review-conversation-dwim)
            (define-key map (kbd "C-c C-k") #'git-review-kill-comment)
            (define-key map (kbd "<tab>") #'git-review-toggle-conversation)
            map))

(provide 'git-review)

;;; git-review.el ends here
