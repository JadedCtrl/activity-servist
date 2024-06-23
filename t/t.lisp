;;;; activity-servist/tests: Testing all of activity-servist.

;; Copyright © 2024 Jaidyn Levesque <jadedctrl@posteo.at>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(defpackage :activity-servist/tests
  (:use :cl)
  (:nicknames "AS/T")
  (:export :run :run-with-summary))

(in-package :activity-servist/tests)

(defun run ()
  "Run tests from all activity-servist subpackages."
  (activity-servist/tests/signatures:run)
  (activity-servist/tests/activity-vocabulary:run))

(defun run-with-summary ()
  "Run tests with summary for all activity-servist subpackages."
  (lisp-unit2:with-summary()
    (run)))
