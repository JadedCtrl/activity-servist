;;;; activitypub-servist/tests/signatures: Testing activitypub-servist/signatures.

;; Copyright © 2023-2024 Jaidyn Levesque <jadedctrl@posteo.at>
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

(defpackage :activitypub-servist/tests/signatures
  (:use :cl :lisp-unit2))

(in-package :activitypub-servist/tests/signatures)


(define-test string-sha256sum (:tags '(misc))
  (assert-equal
   "erws/VxJ7XO5xQBqpwHIUwG0P4q1Ek2D4N053+E2Ib8="
   (ap-s/s::string-sha256sum "This is a testing string~! ♥ ĉu ne?~")))
