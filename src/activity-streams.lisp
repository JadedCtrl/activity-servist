;;;; activity-streams: Serialize/deserialize ActivityStreams objects.

;; Copyright © 2024 Jaidyn Ann <jadedctrl@posteo.at>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(defpackage #:activity-servist/activity-streams
  (:use #:cl)
  (:nicknames "AS/AS" "ACTIVITY-STREAMS")
  (:export
   ;; Functions
   :parse :encode
   :define-class-encoders
   ;; Globals
   :*ap-packages* :*default-class*
   ;; Classes
   :object
   ;; Accessors
   :object-@context :object-unsupported
   ;; Slots
   :@context :@type :type :@id :id :unsupported))

(in-package #:activity-servist/activity-streams)



;;; Globals
;;; ————————————————————————————————————————
(defparameter *ap-packages* (list :activity-servist/activity-vocabulary *package*)
  "A list of packages in which we should search for AP classes and slot-symbols
during JSON parsing. The class-name searched for is simply the value of the JSON
object’s “type” key. The package first in the list to export such a symbol
is the winner.")

(defparameter *default-class* 'activity-servist/activity-streams:object
  "The class used for ActivityStreams objects found during parsing that don’t
have a corresponding class defined. Notably, all keys and values without
corresponding slots are placed in the UNSUPPORTED slot.
The class you choose should inherit ACTIVITY-SERVIST/ACTIVITY-STREAMS:OBJECT.")

;; Private, internal variable.
(defparameter *@context* nil
  "Used in YASON:ENCODE to ensure that a single top-level @context can be
created where AP objects contain other AP objects in their slots.
This variable is overridden locally during encoding (LET), and should never be
modified globally (as we expect it to be nil in top-level objects.")



;;; Macros
;;; ————————————————————————————————————————
;; This macro and the following function are related to JSON serialization; see
;; the below “JSON serialization” section for other related functions.
(defmacro define-yason-encode-slots (class)
  "Define a YASON:ENCODE-SLOTS method for a CLASS, which simply encodes all of
CLASS’es slots with JSON keys based on the camel-cased slot name."
  (append
   `(defmethod yason:encode-slots progn ((obj ,class)))
   (mapcar (yason-encode-slot-function)
           (class-slots-activity-alist class))))

(defun yason-encode-slot-function ()
  "Helper-function for the DEFINE-YASON-ENCODE-SLOTS macro.
This returns a function to create a quoted function that should be called for each slot,
again and again, by YASON:ENCODE-SLOTS."
  (lambda (slot-key-pair)
    `(let ((key   ',(car slot-key-pair))
           (value (ignore-errors (slot-value obj ',(car slot-key-pair)))))
       (cond ((eq key '@context) ; Actually in YASON:ENCODE-OBJECT using *@context*
              (setq *@context* (merge-@contexts *@context* (object-@context obj))))
             ((eq key 'unsupported)
              ;; Keys/values without a slot are stored in this UNSUPPORTED alist.
              (mapcar (lambda (cell)
                        (yason:encode-object-element
                         (camel-case (symbol-name (car cell)))
                         (cdr cell)))
                      value))
             ((eq key 'type)     ; Encode type based on class-name or TYPE slot
              (yason:encode-object-element
               "type" (or value
                          (class-pretty-name (class-of obj)))))
             (value
              (yason:encode-object-element ,(cdr slot-key-pair) value))))))



;;; Core class
;;; ————————————————————————————————————————
(defclass object ()
  ((@context          :initform nil)
   (as/as:@type       :initform nil)
   (as/as:type        :initform nil)
   (as/as:@id         :initform nil)
   (as/as:id          :initform nil)
   (as/as:unsupported :initform nil :accessor object-unsupported)))



;;; Accessors
;;; ————————————————————————————————————————
(defgeneric object-@context (obj)
  (:documentation "Accessor for an object’s associated JSON-LD @context.
As @context can sometimes vary on an object’s contents, on-the-fly, this
method is invoked during JSON encoding of an object. The @CONTEXT
slot-value should be prioritized over the @CONTEXT value is calculated
by this method."))

(defmethod object-@context ((obj object))
  (or (slot-value obj '@context)
      "https://www.w3.org/ns/activitystreams"))

(defmethod (setf object-@context) (context (obj object))
  (setf (slot-value obj '@context) context))



;;; JSON parsing
;;; ————————————————————————————————————————
(defun parse (string)
  "Parse a string containing JSON into an ActivityPub object."
  (parse-table (yason:parse string)))

(defun parse-table (table)
  "Parse a hash-table corresponding to YASON-parsed JSON into an ActivityPub object."
  (let* ((found-class (car (find-registered-classes (param-case (gethash "type" table)))))
         (class       (or found-class (find-class *default-class*)))
         (obj         (make-instance class)))
    (loop for key being each hash-key of table
          for val being each hash-value of table
          do (let* ((slot-name (string-upcase (param-case key)))
                    (slot-sym  (car (find-registered-symbols slot-name)))
                    (val       (parse-value val)))
               (handler-case
                   (setf (slot-value obj slot-sym) val)
                 (error nil
                   (setf (slot-value obj 'unsupported)
                         (append (ignore-errors (slot-value obj 'unsupported))
                                 (list (cons (intern slot-name) val))))))))
    obj))

(defun parse-value (val)
  "Parse the value of a key found in YASON-parsed JSON.
All ActivityPub objects (hash-tables containing “type”) will be parsed into
ActivityPub objects; all others will parsed into associative lists."
  (typecase val
    (hash-table (maybe-parse-table val))
    (list       (mapcar (lambda (a)
                          (if (hash-table-p a)
                              (maybe-parse-table a)
                              a))
                        val))
    (t val)))

(defun maybe-parse-table (table)
  "If a hash-table seems to be a valid ActivityPub object, attempt parsing it
into one. Otherwise, parse it into an associative list."
  (if (gethash "type" table)
      (parse-table table)
      (alexandria:hash-table-alist table)))



;;; JSON serialization
;;; ————————————————————————————————————————
(defun define-class-encoders (classes)
  "For each class in CLASSES, define an YASON:ENCODE-SLOTS method for that class,
which only encodes slots unique to that class (as ENCODE-SLOTS is called for each
inherited class). Each slot’s name is converted to camel-case, as per convention."
  (mapcar (lambda (class)
            (closer-mop:finalize-inheritance class)
            (eval `(define-yason-encode-slots ,class)))
     classes))

(defun encode (obj &optional (stream *standard-output*))
  "Encode OBJ into JSON. A mere wrapper around YASON:ENCODE."
  (yason:encode obj stream))

(defmethod yason:encode ((obj object) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:encode-object obj)))

(defmethod yason:encode-object ((obj object))
  (typecase *@context*
    (null   ; If this is the top-level (non-nested) object, establish a @context.
     (let ((*@context* 'top-level))
       (yason:encode-object obj)))
    (symbol ; In the top-level, encode slots and then @context.
     (setq *@context* (object-@context obj))
     (yason:with-object ()
       (yason:encode-slots obj)
       (yason:encode-object-element "@context" *@context*)))
    (T      ; In nested objects, only encode slots — not *@context*.
     (yason:with-object ()
       (yason:encode-slots obj)))))

(defun class-slots-activity-alist (class)
  "Return an associative list containing CLASSes slots’ symbols consed with
their sanitized string keys appropriate for ActivityVocabular custom.
A class with slots MAP-AWAY and COLLECTION-AGAIN would return
  ((MAP-AWAY . “mapAway”)(COLLECTION-AGAIN . “collectionAgain”))"
  (alist-mapcdr #'camel-case
                (class-slots-alist class)))

(defun merge-@contexts (a b)
  "Given two @context lists, A and B, merge them into one JSON-LD @context list
containing both of their elements."
  (cond ((equal a b) a)
        ((not b)     a)
        ((not a)     b)
        ((and (listp a)
              (find b a :test #'equal))
         a)
        (T
         (merge-lists
          (if (listp a) a (list a))
          (if (listp b) b (list b))))))



;;; Util
;;; ————————————————————————————————————————
(defun camel-case (string)
  "Convert a STRING to camel-casing. That is, casingLikeThis.
Wrapper around STR:CAMEL-CASE, working around a bug that a non-alphanumeric
character at the start of the string gets erroneously (or at least undesireably,
to us) removed."
  (keep-nonalphanumeric-prefix string
                               (str:camel-case string)))

(defun param-case (string)
  "Convert a STRING to param-casing. That is, casing-like-this.
Wrapper around STR:PARAM-CASE, working around a bug that a non-alphanumeric
character at the start of the string gets erroneously (or at least undesireably,
to us) removed."
  (keep-nonalphanumeric-prefix string
                               (str:param-case string)))
(defun camel-case (str)
  "Convert a STRING to camel-casing.
Wrapper around STR:CAMEL-CASE, working around a bug that a non-alphanumeric
character at the start of the string gets erroneously (or at least undesireably,
to us) removed."
  (keep-nonalphanumeric-prefix str (str:camel-case str)))

(defun keep-nonalphanumeric-prefix (str child-str)
  "This ensures that a CHILD-STR derived from STR has the same nonalphanumeric
prefix as STR, as some functions like to remove such prefixes."
  (if (not (alphanumericp (aref str 0)))
      (concatenate 'string
                   (string (aref str 0))
                   child-str)
      child-str))

(defun class-pretty-name (class)
  "Return a CLASS’es name in a “pretty” (sentence-capitalized) string."
  (string-capitalize (symbol-name (class-name class))))

(defun merge-lists (a b)
  "Given lists A and B, merge them into one list non-redundantly — all unique
items in each will be contained in the resultant list."
  (append a (remove-if (lambda (item) (find item a :test #'equal)) b)))

(defun find-registered-symbols (str)
  "Find all symbols identified by string STR within packages in the
*ap-packages* list."
  (remove-if
   #'not
   (mapcar (lambda (package)
             (multiple-value-bind (sym context)
                 (find-symbol (string-upcase str) package)
               (unless (eq context :inherited)
                 sym)))
           *ap-packages*)))

(defun find-registered-classes (str)
  "Find all classes identified by string STR within pacakges in the
*ap-packages* list."
  (remove-if
   #'not
   (mapcar (lambda (sym) (find-class sym))
           (find-registered-symbols str))))

(defun alist-mapcdr (function alist)
  "Apply a FUNCTION to all values (cdrs) of an ALIST’s pairs. Returns a new ALIST
of the same keys, whose values are the results of FUNCTION."
  (mapcar
   (lambda (cell)
     (cons (car cell)
           (funcall function (cdr cell))))
   alist))

(defun class-slots-alist (class)
  "Return an associative list of a CLASS’es direct slots (by symbol) matched with
their names as strings. For instance, a class with slots MAP-AWAY and
COLLECTION-AGAIN would return:
  ((MAP-AWAY . “MAP-AWAY”)(COLLECTION-AGAIN . “COLLECTION-AGAIN”)"
  (mapcar
   (lambda (slot)
     (let ((name (closer-mop:slot-definition-name slot)))
      (cons name (symbol-name name))))
   (closer-mop:class-direct-slots class)))



;;; Defining YASON:ENCODE-SLOTS
;;; ————————————————————————————————————————
(as/as:define-class-encoders (list (find-class 'object)))
