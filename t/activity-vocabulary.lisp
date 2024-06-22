;;;; activitypub-servist/tests/activity-vocabulary: Testing activity-vocabulary.

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

(defpackage :activitypub-servist/tests/activity-vocabulary
  (:use :cl :lisp-unit2)
  (:nicknames "AP-S/T/AV")
  (:export :run))

(in-package :activitypub-servist/tests/activity-vocabulary)

(defun run ()
  "Run all ACTIVITY-VOCABULARY tests."
  (lisp-unit2:with-summary ()
    (lisp-unit2:run-tests :package :activitypub-servist/tests/activity-vocabulary)))



;;; Util
;;; ————————————————————————————————————————
(defmacro relative-pathname (path)
  "Return an absolute path adding the relative PATH to the system’s path."
  `(asdf:system-relative-pathname :activitypub-servist/tests/activity-vocabulary ,path))

(defmacro define-json-test (path tags)
  "Define a lisp-unit2 test for parsing of the given JSON file.
We compare the original JSON to that of the parsed-then-reserialized JSON,
ensuring they are semantically equivalent. White-space and key order are ignored."
  (let ((content (alexandria:read-file-into-string (relative-pathname path))))
    `(define-test ,(intern (string-upcase (pathname-name path))) (:tags ,tags)
       (assert-equal
        (hash-table-sorted-alist
         (yason:parse ,content))
        (hash-table-sorted-alist
         (yason:parse
          (yason:with-output-to-string* ()
            (yason:encode-object
             (av:parse ,content)))))))))

(defun sort-alist (alist predicate)
  "Sort an associative list by its keys."
  (sort alist
        (lambda (cell-a cell-b)
          (apply predicate (list (car cell-a) (car cell-b))))))

(defun hash-table-sorted-alist (table &optional (predicate #'string<))
  "Return a sorted associative list containing the keys and values of TABLE.
Any nested hash-tables found as values are also sorted, recursively."
  (sort-alist
   (mapcar (lambda (cell)
             (cons (car cell)
                   (if (hash-table-p (cdr cell))
                       (hash-table-sorted-alist (cdr cell))
                       (cdr cell))))
           (alexandria:hash-table-alist table))
   predicate))



;;; Test definitions
;;; ————————————————————————————————————————
;; Define a test for each ActivityVocabulary type’s example JSON.
;; Examples are taken from the spec:
;;   https://www.w3.org/TR/activitystreams-vocabulary/
(mapcar (lambda (file)
          (eval `(define-json-test ,file '(:core))))
        (uiop:directory-files
         (relative-pathname "t/activity-vocabulary/core/")))
