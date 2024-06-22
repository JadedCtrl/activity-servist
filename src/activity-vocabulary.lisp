;;;; activity-vocabulary: Classes for ActivityVocabulary types.

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

(defpackage #:activitypub-servist/activity-vocabulary
  (:use #:cl)
  (:nicknames "AP-S/AV" "AV")
  (:shadow #:delete #:ignore #:listen #:read #:remove)
  ;; One should never USE this package, since some class-names shadow
  ;; core Common Lisp symbols! Beware! :P
  (:export
   ;; Functions
   :parse
   ;; Globals
   :*ap-packages*
   ;; Classes
   :accept :activity :add :announce :application :arrive :article :audio
   :collection :collection-page :create :delete :dislike :document :event :flag
   :follow :group :ignore :ignore :image :intransitive-activity :invite :join
   :leave :like :link :link :listen :move :note :object :offer
   :ordered-collection :ordered-collection-page :organization :page :person
   :place :profile :question :read :reject :relationship :remove :service
   :tentative-accept :tentative-reject :tombstone :travel :undo :update :video
   :view
   ;; Slots
   :@context :type
   :activity-actor :activity-instrument :activity-object :activity-origin
   :activity-result :activity-target
   :collection-current :collection-first :collection-items :collection-last
   :collection-total-items
   :collection-page-next :collection-page-part-of :collection-page-prev
   :link-height :link-href :link-hreflang :link-media-type :link-name
   :link-preview :link-rel :link-width
   :object-attachment :object-attributed-to :object-audience :object-bcc
   :object-bto :object-cc :object-content :object-context :object-duration
   :object-end-time :object-generator :object-icon :object-image
   :object-in-reply-to :object-location :object-media-type :object-name
   :object-preview :object-published :object-replies :object-start-time
   :object-summary :object-tag :object-to :object-type :object-updated
   :object-url
   :ordered-collection-page-start-index
   :place-accuracy :place-altitude :place-latitude :place-longitude
   :place-radius :place-units
   :profile-describes
   :relationship-object :relationship-relationship :relationship-subject
   :tombstone-former-type :tombstone-deleted))

(in-package #:activitypub-servist/activity-vocabulary)



;;; Globals
;;; ————————————————————————————————————————
(defparameter *ap-packages* (list *package*)
  "A list of packages in which we should search for AP classes and slot-symbols
during JSON parsing. The class-name searched for is simply the value of the JSON
object’s “type” key. The package first in the list to export such a symbol
is the winner.")

;; Private, internal variable.
(defparameter *@context* nil
  "Used in YASON:ENCODE to ensure that a single top-level @context can be
created where AP objects contain other AP objects in their slots.
This variable is overridden locally during encoding (LET), and should never be
modified globally (as we expect it to be nil in top-level objects.")



;;; Macros
;;; ————————————————————————————————————————
(defmacro defclass-w-accessors (name direct-superclasses slots &rest options)
  "Identical to DEFCLASS, but with one convenience: A slot definition, if being
simply a symbol, will default to a slot with an accessor and init-arg named after the
symbol. The init-arg will be “:symbol”, and the accessor will be “classname-symbol”.
For example, the following two forms are equivalent:
    (defclass-w-accessors PERSON () (AGE
                                     HEIGHT
                                     (NAME :INIT-FORM “Unknown”)))
    (defclass PERSON () ((AGE    :INIT-ARG :AGE    :ACCESSOR PERSON-AGE)
                         (HEIGHT :INIT-ARG :HEIGHT :ACCESSOR PERSON-HEIGHT)
                         (NAME   :INIT-FORM “Unknown”)))"
  `(defclass ,name ,direct-superclasses
     ,(mapcar
       (lambda (slot)
         (typecase slot
           (list slot)
           (t (list slot :accessor (intern (format nil "~A-~A" name slot))
                         :initarg  (intern (symbol-name slot) "KEYWORD")
                         :initform nil))))
       slots)
     ,@options))

(defmacro defclass-empty-children (name direct-children)
  "For each name in the list DIRECT-CHILDREN, a subclass of NAME will be created.
These new subclasses have no slots of its own — they will be empty derivatives
of NAME."
  (append
   '(progn)
   (mapcar (lambda (a)
             `(defclass ,a (,name) ()))
           direct-children)))

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
           (value (slot-value obj ',(car slot-key-pair))))
       (cond ((eq key '@context) ; Encoded in YASON:ENCODE-OBJECT using *@context*
              (setq *@context* (merge-@contexts *@context* value)))
             ((eq key 'type)     ; Encode type based on class-name or TYPE slot
              (yason:encode-object-element
               "type" (or value
                          (class-pretty-name (class-of obj)))))
             (value
              (yason:encode-object-element ,(cdr slot-key-pair) value))))))



;;; Core types
;;; ————————————————————————————————————————
;; https://www.w3.org/ns/activitystreams#Object
(defclass-w-accessors object ()
  (
   attachment attributed-to audience bcc bto cc content context
   duration end-time generator icon id image in-reply-to location
   media-type name preview published replies start-time summary
   tag to type updated url
   (@context :initform "https://www.w3.org/ns/activitystreams")))

;; https://www.w3.org/ns/activitystreams#Link
(defclass-w-accessors link ()
  (height href hreflang media-type name preview rel width))

;; https://www.w3.org/ns/activitystreams#Activity
(defclass-w-accessors activity (object)
  (actor instrument object origin result target))

;; Should be ordinary Activity, sans `object`.
;; https://www.w3.org/ns/activitystreams#IntransitiveActivity
(defclass intransitive-activity (activity) ())

;; https://www.w3.org/ns/activitystreams#Collection
(defclass-w-accessors collection (object)
  (current first items last total-items))

;; https://www.w3.org/ns/activitystreams#OrderedCollection
(defclass ordered-collection (collection) ())

;; https://www.w3.org/ns/activitystreams#CollectionPage
(defclass-w-accessors collection-page (collection)
  (next part-of prev))

;; https://www.w3.org/ns/activitystreams#OrderedCollectionPage
(defclass-w-accessors ordered-collection-page (collection-page)
  (start-index))



;;; Extended Activity types
;;; ————————————————————————————————————————
(defclass-empty-children activity
  (accept add announce create delete dislike flag follow ignore join leave
          like listen move offer read reject remove travel undo update view))

(defclass arrive (intransitive-activity) ())
(defclass ignore (block) ())
(defclass invite (offer) ())
(defclass question (intransitive-activity) ())
(defclass tentative-accept (accept) ())
(defclass tentative-reject (reject) ())



;;; Extended Actor types
;;; ————————————————————————————————————————
(defclass-empty-children object
  (application group organization person service))



;;; Extended Object types
;;; ————————————————————————————————————————
(defclass-empty-children object
  (article document event note))

(defclass-empty-children document
  (audio image page video))

;; https://www.w3.org/ns/activitystreams#Place
(defclass-w-accessors place (object)
  (accuracy altitude latitude longitude radius units))

;;  https://www.w3.org/ns/activitystreams#Profile
(defclass-w-accessors profile (object)
  (describes))

;; https://www.w3.org/ns/activitystreams#Relationship
(defclass-w-accessors relationship (object)
  (object relationship subject))

;; https://www.w3.org/ns/activitystreams#Tombstone
(defclass-w-accessors tombstone (object)
  (former-type deleted))



;;; Extended Link types
;;; ————————————————————————————————————————
(defclass-empty-children link
  (mention))



;;; JSON parsing
;;; ————————————————————————————————————————
(defun parse (string)
  "Parse a string containing JSON into an ActivityPub object."
  (parse-table (yason:parse string)))

(defun parse-table (table)
  "Parse a hash-table corresponding to YASON-parsed JSON into an ActivityPub object."
  (let* ((class (car (find-registered-classes (gethash "type" table))))
         (obj   (make-instance class)))
    (loop for key being each hash-key of table
          for val being each hash-value of table
          do (let ((slot-sym (car (find-registered-symbols key)))
                   (val      (parse-value val)))
               (when slot-sym
                 (setf (slot-value obj slot-sym) val))))
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
;; Note-worthy: See the above-defined DEFINE-YASON-ENCODE-SLOTS.
(defmethod yason:encode ((obj object) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:encode-object obj)))

(defmethod yason:encode-object ((obj object))
  (typecase *@context*
    (null   ; If this is the top-level (non-nested) object, establish a @context.
     (let ((*@context* 'top-level))
       (yason:encode-object obj)))
    (symbol ; In the top-level, encode slots and then @context.
     (setq *@context* (slot-value obj '@context))
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
  "Convert a STRING to camel-casing.
Wrapper around STR:CAMEL-CASE, working around a bug that a non-alphanumeric
character at the start of the string gets erroneously (or at least undesireably,
to us) removed."
  (if (not (alphanumericp (aref string 0)))
      (concatenate 'string
                   (string (aref string 0))
                   (str:camel-case string))
      (str:camel-case string)))

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
  (mapcar (lambda (package) (find-symbol (string-upcase str) package))
          *ap-packages*))

(defun find-registered-classes (str)
  "Find all classes identified by string STR within pacakges in the
*ap-packages* list."
  (mapcar (lambda (sym) (find-class sym))
          (find-registered-symbols str)))

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
;; On-the-fly define YASON:ENCODE-SLOTS for each of our distinct AP classes.
(mapcar (lambda (class)
          (closer-mop:finalize-inheritance class)
          (eval `(define-yason-encode-slots ,class)))
        (mapcar #'find-class
                '(object link activity collection collection-page
                  ordered-collection-page place profile relationship tombstone)))
