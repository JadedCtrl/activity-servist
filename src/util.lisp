;;;; util: General utilities used in several parts of activity-servist.

;; Copyright © 2025 Jaidyn Ann <jadedctrl@posteo.at>
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

(defpackage #:activity-servist/util
  (:use #:cl)
  (:nicknames "AS/U")
  (:export
   ;; Functions
   #:http-get
   ;; Symbols
   #:http-request-error #:http-get-error
   ;; Slots/Accessors
   :body :status))

(in-package #:activity-servist/util)


;;; Reader macros
;;; ————————————————————————————————————————
;; Look, we *love* the [multiple {bracket-types [style]}].
(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[
  (lambda (stream char)
    (declare (ignore char))
    (read-delimited-list #\] stream t)))

(set-macro-character #\} (get-macro-character #\)))
(set-macro-character #\{
  #'(lambda (stream char)
      (read-delimited-list #\} stream t)))
