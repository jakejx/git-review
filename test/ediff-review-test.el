;;; git-review-test.el --- Tests for git-review.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Niklas Eklund

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for `git-review'.

;;; Code:

;;;; Requirements

(require 'ert)
(require 'git-review)

(ert-deftest git-review-test-test-differences-intersect-p ()
  (should (git-review--differences-intersect-p
           '((:begin 110 :end 111)
             (:begin 138 :end 140))
           '((:begin 112 :end 114)
             (:begin 139 :end 141))))

  (should (git-review--differences-intersect-p
           '((:begin 3 :end 8))
           '((:begin 1 :end 2)
             (:begin 5 :end 6))))

  (should (not (git-review--differences-intersect-p
                '((:begin 18 :end 33))
                '((:begin 34 :end 40))))))

(ert-deftest git-review-test-ignore-file-p ()
  (let ((git-review-ignore-file-predicates
         '((lambda (file)
             (string-match "ba" file)))))
    (should (not (git-review--ignore-file-p "foo")))
    (should (git-review--ignore-file-p "bar"))
    (should (git-review--ignore-file-p "baz"))))

(ert-deftest git-review-test-file-metadata ()
  (let ((git-review-metadata-functions
         '((lambda (file)
             (when (string-match "bar" file)
               `(old . t)))
           (lambda (file)
             (when (string-match "foo" file)
               `(new . t))))))
    (should (equal (git-review--file-metadata "foo") '((new . t))))
    (should (equal (git-review--file-metadata "bar") '((old . t))))))

(provide 'git-review-test)

;;; git-review-test.el ends here
