;;; ediff-review-test.el --- Tests for ediff-review.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Niklas Eklund

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

;; Tests for `ediff-review'.

;;; Code:

;;;; Requirements

(require 'ert)
(require 'ediff-review)

(ert-deftest ediff-review-test-test-differences-intersect-p ()
  (should (ediff-review--differences-intersect-p
           '((:begin 110 :end 111)
             (:begin 138 :end 140))
           '((:begin 112 :end 114)
             (:begin 139 :end 141))))

  (should (ediff-review--differences-intersect-p
           '((:begin 3 :end 8))
           '((:begin 1 :end 2)
             (:begin 5 :end 6))))

  (should (not (ediff-review--differences-intersect-p
                '((:begin 18 :end 33))
                '((:begin 34 :end 40))))))

(provide 'ediff-review-test)

;;; ediff-review-test.el ends here
