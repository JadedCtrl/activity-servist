;;;; json-λd: (Aspiring) parser and encoder for JSON-LD data

;; Copyright © 2024 Jaidyn Ann <jadedctrl@posteo.at>
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

(defpackage #:activity-servist/json-ld
  (:use #:cl)
  (:nicknames "AS/JSON-LD" "JSON-LD")
  (:export
   #:define-json-type #:json-ld-context))

(in-package #:activity-servist/json-ld)


;;; Globals
;;; ————————————————————————————————————————
(defvar *http-cache* (make-hash-table :test #'equal))
(defvar *json-types* (make-hash-table :test #'equal))



;;; Base class
;;; ————————————————————————————————————————
(defclass json-ld-type ()
  ((@context
    :initform nil
    :documentation
    "Used as an override for a class’es @context during encoding.
The method JSON-LD-CONTEXT is how the contents of encoded @context is
determined; to change a class’es default/calculated @context, override that
method. This slot is for changing a specific object’s @context.")
   (etc
    :initform nil
    :documentation
    "Components of the JSON object which, during parsing, did not match any specific
slot. This is often filled up in the case of undefined node-types or non-adherent
object definitions.")))

(defgeneric json-ld-context (obj)
  (:documentation
   "Returns a JSON-LD CLOS object’s @context, for use in JSON-encoding of the
object.
The implementation for the JSON-LD-TYPE class simply returns the activitystreams
URL.
If you would like to change @context on a class-level, override this method.
If you would like to change it on an object-level, set the @CONTEXT slot."))

(defmethod json-ld-context ((obj json-ld-type))
  (or (slot-value obj '@context)
      "https://www.w3.org/ns/activitystreams"))

(defmethod yason:encode-slots progn ((obj json-ld-type))
  (yason:encode-object-element "@context" (json-ld-context obj)))



;;; CLOS definition
;;; ————————————————————————————————————————
(defmacro define-json-type (names direct-superclasses context direct-slots &rest options)
  "Used to define a CLOS class and a JSON encoder/decoder for a JSON-LD node-type.
An instance of class will be output, instead of a hash-table, when parsing JSON-LD
with JSON-LD:PARSE. Instances of this class can be encoded into JSON with YASON:ENCODE.

NAMES is a pair with two values: The CLOS class-name, and the name used during
encoding as @type’s value. If only the CLOS class-name is provided, @type will
not be encoded for this object.

DIRECT-SUPERCLASSES is a list of JSON-LD CLOS classes, whose slots and context
this should inherit. JSON-LD-TYPE should be somewhere in the hierarchy, in order
to provide “@context”; if no superclasses are provided, JSON-LD-TYPE is default.

CONTEXT is a context hash-table, parsed from a JSON-LD context by JSON-LD:PARSE
or JSON-LD:PARSE-CONTEXT. Any terms defined in this context will be used to
resolve names in your class definition — including the class-name and slot-names.

DIRECT-SLOTS is a list of slots of the format:
  (SLOT-NAME PROPERTY-NAME SLOT-OPTION … SLOT-OPTION)

Where SLOT-NAME is the symbol corresponding to the slot and PROPERTY-NAME is the
name used in JSON parsing/encoding. Slots without a PROPERY-NAME will not be
encoded nor decoded in JSON.

SLOT-OPTIONS are key/value pairs in the format of DEFCLASS’es slot-options.
Keywords such as :INITFORM, :DOCUMENTATION, etc. can be used.
There are two keywords with behavior unlike DEFCLASS, however:
:REQUIRED and :ACCESSOR.

By default, a slot will have an init-form of NIL; this can of course be
overridden by putting :INITFORM yourself in the slot definition.
Set :REQUIRED to T to not set :INITFORM at all, effectively making the slot
“required.”

By default, a slot will have an accessor named after the class and slot, like
PLACE-RADIUS for the class PLACE and the slot RADIUS.
Set :ACCESSOR to NIL to define no accessor at all.

OPTIONS contains ordinary class options, in the format of DEFCLASS; for
instance, :DOCUMENTATION.

Here is a brief example partially defining the “Place” type from ActivityStreams:

  (define-json-type (place “Place”) (object) ctx
    ((altitude  “altitude”
                :documentation “Indicates the altitude of a place.”)
     (latitude  “latitude”
                :required T
                :documentation “The latitude of a place.”)
     (longitude “longitude”
                :required T
                :documentation “The longitude of a place.”)))"
  `(let ((json-class
           (define-json-clos-class ,names
               ,(or direct-superclasses `(json-ld-type))
             ,direct-slots ,options)))
     (define-json-type-encoder ,(car names) ,direct-slots)
     (register-json-type ',names ',direct-slots ,context)
     json-class))

(defmacro define-json-clos-class (names direct-superclasses direct-slots options)
  "Helper-macro for DEFINE-JSON-TYPE.
This actually defines the CLOS class for a JSON-LD node-type.
See DEFINE-JSON-TYPE’s docstring for a description of parameters."
  (append `(defclass ,(car names) ,direct-superclasses
             ,(mapcar (lambda (slot)
                        (let* ((our-slot-opts
                                 (cddr slot))
                               (slot-opts
                                 (json-type-slot-options (car names) (car slot)
                                                         our-slot-opts)))
                         (append (list (car slot))
                                 slot-opts)))
               direct-slots))
          options))

(defun json-type-slot-options (class-name slot-name slot-opts)
  "Return DEFCLASS-format slot options from DEFINE-JSON-TYPE-format SLOT-OPTS,
applying default slot-options, etc."
  (json-type-normalize-slot-options
   (merge-plists (json-type-implicit-slot-options class-name slot-name)
                 slot-opts)))

(defun json-type-implicit-slot-options (class-name slot-name)
  "Return default property-list slot options for a json-type CLOS class."
  (list :initform nil
        :accessor (intern (format nil "~A-~A" class-name slot-name))))

(defun json-type-normalize-slot-options (slot-opts)
  "Take property-list slot options from a DEFINE-JSON-TYPE format and massage it
into a DEFCLASS format."
  (let* ((required            (getf slot-opts :required))
         (sans-required       (alexandria:remove-from-plist slot-opts :required))
         (sans-initform-maybe (if required
                                  (alexandria:remove-from-plist sans-required :initform)
                                  sans-required))
         (sans-accessor-maybe (if (and (find :accessor slot-opts)
                                       (not (getf slot-opts :accessor)))
                                  (alexandria:remove-from-plist sans-initform-maybe :accessor)
                                  sans-initform-maybe)))
    sans-accessor-maybe))



;;; Encoding
;;; ————————————————————————————————————————
(defmacro define-json-type-encoder (class direct-slots)
  "Helper-macro for DEFINE-JSON-CLOS-CLASS.
This actually defines the YASON-encoder for a JSON-LD node-type.
CLASS is the class-name; see DEFINE-JSON-TYPE’s docstring about DIRECT-SLOTS."
  (append
   `(defmethod yason:encode-slots progn ((obj ,class)))
   (mapcar (lambda (slot)
             `(yason:encode-object-element ,(cadr slot) (slot-value obj ',(car slot))))
           direct-slots)))



;;; Parsing
;;; ————————————————————————————————————————
(defun register-json-type (names direct-slots context)
  "Register a JSON node-type. This allows PARSE to recognize the type (and
corresponding CLOS class) of a node."
  (let* ((ctx       (parse-context context))
         (type-iri  (getf (gethash (cadr names) ctx) :id))
         (type-name (or type-iri (cadr names))))
    (setf (gethash type-name *json-types*)
          (json-type-registry-list names ctx direct-slots))))

(defun json-type-registry-list (names parsed-context direct-slots)
  "Return a REGISTER-JSON-TYPE-formed registry entry, a simple list of the form:
  (TYPE-IRI (PROPERTY-NAME SLOT-NAME) ⋯ (PROPERTY-NAME SLOT-NAME))
… where TYPE-IRI is the (hopefully) fully-resolved IRI form of the node-type’s
name, though it might be unresolved if context was unprovided or lacking."
  (append (list (cons (car names) (cadr names)))
          (mapcar
           (lambda (slot)
             (when (cadr slot)
               (let* ((property-name (cadr slot))
                      (slot-name     (car slot))
                      (ctx-item      (gethash property-name parsed-context))
                      (url           (or (getf ctx-item :id)
                                         property-name)))
                 (cons url (cons slot-name property-name)))))
           direct-slots)))

(defun parse (str)
  "Parse the JSON-LD document contained in STR."
  (let ((ctx     (make-hash-table :test #'equal)) ; Parsed context: IRI→name,etc.
        (rev-ctx (make-hash-table :test #'equal)) ; Inversed ctx: name→IRI
        (parsed (yason:parse str)))
    (values (parse-item parsed ctx rev-ctx)
            ctx)))

(defun parse-item (item &optional ctx rev-ctx)
  "Parse an individual ITEM of a YASON-decoded JSON-LD document."
  (typecase item
    (hash-table (parse-table item ctx rev-ctx))
    (list       (mapcar (lambda (a) (parse-item a ctx)) item))
    (T          item)))

(defun parse-table (table &optional ctx rev-ctx)
  "Parse a JSON “node object” (as decoded by YASON into a hash-TABLE."
  (let ((ctx (parse-context (gethash "@context" table) ctx)))
    ;; Update our inverted context-table, so we can resolve property-names→IRIs.
    (when (not (eq (hash-table-count rev-ctx)
                   (hash-table-count ctx)))
      (copy-hash-table-to ctx rev-ctx)
      (invert-hash-table rev-ctx (lambda (val)
                                   (getf val :id))))
    ;; Now, actually parse.
    (let* ((parsed-table (parse-table-inplace table ctx))
           (type         (identify-json-type table ctx rev-ctx))
           (typedef      (gethash type *json-types*)))
      (if typedef
          (parse-table-into-object parsed-table typedef ctx rev-ctx) ; We prefer this!
          parsed-table)))) ; … but just in case you wanna use an undefined type…

(defun parse-table-inplace (table ctx)
  "Expand a YASON-parsed JSON-LD node-object in TABLE. That is, replace all
property-names (like, say, “duration”) with their uncompacted-IRI forms
(like “https://www.w3.org/ns/activitystreams#duration”).
CTX should be the parsed-context corresponding to the table."
  (maphash
   (lambda (old-key val)
     (let* ((key-ctx  (gethash old-key ctx))
            (key-iri  (getf key-ctx :id))
            (key-type (getf key-ctx :type))
            (new-key  (or key-iri old-key)))
       (when key-ctx
         (if (not (equal old-key new-key))
             (remhash old-key table))
         (setf (gethash new-key table)
               (parse-item
                val
                (or (and (hash-table-p val) (alexandria:copy-hash-table ctx))
                    ctx))))))
   table)
  table)

(defun parse-table-into-object (table type-def ctx rev-ctx)
  "Parse an expanded-form JSON-LD object (TABLE) into a CLOS object.
TYPE-DEF is a type-definition list of the form found in *JSON-TYPES* and made
by REGISTER-JSON-TYPE.
CTX is the according parsed-context, and REV-CTX is the reversed
(IRI → property-name) context."
  (let ((obj (make-instance (caar type-def))))
    (maphash
     (lambda (property value)
       (let* ((property-def  (assoc property type-def :test #'equal))
              (slot-name     (second property-def))
              (etc-value     (slot-value obj 'etc)))
         (if property-def
             (setf (slot-value obj slot-name) value)
             (setf (slot-value obj 'etc)
                   (append etc-value
                           (list (cons property value)))))))
     table)
    (setf (slot-value obj '@context) (gethash "@context" table))
    obj))

(defun identify-json-type (table ctx rev-ctx)
  "Given an parsed JSON-LD object’s hash-TABLE, return the name/IRI of the
JSON-type that best suits the object — using the types registered into
*JSON-TYPES* with REGISTER-JSON-TYPE."
  (let* ((type (gethash "@type" table)))
    (or (getf (gethash type ctx) :id)
        type)))



;;; Context-parsing
;;; ————————————————————————————————————————
(defun parse-context (ctx-list &optional (ctx (make-hash-table :test #'equal)))
  "Parse a JSON-LD map’s @context contents into a hash-table mapping string keys
to IRIs."
  (let ((ctx-list   (if (listp ctx-list) ctx-list (list ctx-list)))
        (unresolved '()))
    (mapcar (lambda (ctx-list-item)
              (nconc unresolved
                     (parse-context-item ctx ctx-list-item)))
            ctx-list)
    (repeat-parse-context ctx unresolved)))

(defun repeat-parse-context (ctx unresolved-terms)
  "Helper function for PARSE-CONTEXT.
Takes an association list of UNRESOLVED-TERMS, and repeats context-parsing until
all terms are resolved and parsed (or it’s clear that this isn’t possible).

These unresolved terms are terms mapped to compacted IRIs whose prefix hasn’t yet
been aprsed into CTX. See UNCOMPACT-IRI and COMPACT-IRI-P for more info on IRIs."
  (let* ((unresolved-table (alexandria:alist-hash-table unresolved-terms))
         (now-unresolved   (parse-context-map ctx unresolved-table)))
    (cond ((not now-unresolved)
           ctx)
          ((eq (length now-unresolved)
               (length unresolved-terms))
           (values ctx now-unresolved)
           (error 'iri-unresolved :unresolved now-unresolved))
          (T
           (repeat-parse-context ctx now-unresolved)))))

(defun parse-context-item (ctx item)
  "Parse an individual ITEM in a JSON-LD map’s @context array.
All terms found in the ITEM are then added to the CTX hash-table.
Returns an association list containing terms in the item that couldn’t be
resolved, likely compacted keys conned with compacted IRIs whose prefix hasn’t
yet been parsed into CTX."
  (typecase item
   (string     (parse-remote-context ctx item))
   (hash-table (parse-context-map    ctx item))
   (T           nil)))

(defun parse-remote-context (ctx uri)
  "Parse a remote JSON-LD context at URI, adding its terms to the CTX
hash-table."
  (let* ((headers '(("Accept" . "application/json,application/ld+json")))
         (str     (caching-http-get uri :headers headers))
         (parsed  (yason:parse str)))
    (parse-context (gethash "@context" parsed) ctx)))

(defun parse-context-map (ctx table)
  "Parse an map item of a JSON-LD @context (which has been parsed by YASON into
a hash-TABLE).
Add all terms found to the hash-table CTX.
Returns an association list of terms that couldn’t be resolved, likely compacted
IRI values whose prefix hasn’t yet been parsed into CTX."
  (let ((unresolvable '()))
    (maphash
     (lambda (term val)
       (let* ((id (typecase val
                    (string     val)
                    (hash-table (gethash "@id" val))))
              (iri (when (iri-p id) id))
              (uncompacted-iri (ignore-errors (uncompact-iri iri ctx)))
              (parsed-id (or uncompacted-iri
                             (and (ld-keyword-p id) id)))
              (type
                (typecase val
                  (string     nil)
                  (hash-table (gethash "@type" val)))))
         (cond ((and id (not parsed-id))
                (push (cons term iri) unresolvable))
               (T
                (setf (gethash term ctx) (list :id parsed-id :type type))))))
     table)
    unresolvable))



;;; IRI/keywords
;;; ————————————————————————————————————————
(defun ld-keyword-p (str)
  "Return whether or not a string is a JSON-LD keyword, as defined in the spec."
  (member (string-downcase str)
          '("@base" "@container" "@context" "@direction" "@graph" "@id" "@import"
            "@included" "@index" "@json" "@language" "@list" "@nest" "@none"
            "@prefix" "@propagate" "@protected" "@reverse" "@set" "@type"
            "@value" "@version" "@vocab")
          :test #'equal))

(defun uncompact-iri (iri ctx)
  "Given a compacted IRI, uncompact it into its normal form, with CTX being a
hash-table containing terms mapped to IRIs.
For instance, if CTX maps “xd” to “http://lol.net/ns#”, then:
    (uncompact-iri “xd:laughing” CTX) => “http://lol.net/ns#laughing”
https://www.w3.org/TR/json-ld11/#compact-iris"
  (if (compacted-iri-p iri)
      (destructuring-bind (prefix suffix)
          (str:split #\: iri)
        (let* ((prefix-ctx (gethash (string-downcase prefix) ctx))
               (prefix-iri (when prefix-ctx (getf prefix-ctx :id))))
          (if prefix-iri
              (format nil "~A~A" prefix-iri suffix)
              (error 'iri-prefix-not-found :iri prefix))))
      (if (not (iri-p iri))
          (error 'not-iri :iri iri)
          iri)))

(defun iri-p (str)
  "Return whether or not a string is an IRI, compacted or otherwise."
  (or (uncompacted-iri-p str)
      (compacted-iri-p str)))

(defun uncompacted-iri-p (str)
  "Return whether or not a string is an orindary IRI."
  (search "://" str))

(defun compacted-iri-p (str)
  "Return whether or not a string is an IRI in compacted “prefix:suffix” form.
https://www.w3.org/TR/json-ld11/#compact-iris"
  (and (find #\: str)
       (not (search "://" str))
       (not (equal str "_:"))))



;;; Conditions
;;; ————————————————————————————————————————
(define-condition iri-error (error)
  ((iri :initarg :iri :initform nil :accessor iri-error-iri)))

(define-condition not-iri (iri-error) ()
  (:report
   (lambda (condition stream)
     (format stream "“~A” is not a valid IRI." (iri-error-iri condition))))
  (:documentation
   "A string containing an IRI was expected, but something else was provided."))

(define-condition iri-prefix-not-found (iri-error) ()
  (:report
   (lambda (condition stream)
     (format stream "Failed to find IRI prefix in context: ~A" (iri-error-iri condition))))
  (:documentation
   "Attempted to resolve a compact IRI’s prefix, but its prefix hasn’t yet been
defined in the context."))

(define-condition context-error (error)
  ((unresolved :initarg :unresolved :initform nil :accessor context-error-unresolved)))

(define-condition iri-unresolved (context-error) ()
  (:report
   (lambda (condition stream)
     (format nil "Compact IRI(s) in context could not be resolved: ~A"
             (context-error-unresolved condition))))
  (:documentation
   "Compact IRI(s)’ prefixes couldn’t be resolved, having analyzed all context items."))



;;; Utility
;;; ————————————————————————————————————————
(defun caching-http-get (uri &key headers)
  "Makes a GET request to URI, returning the resultant string.
Each resultant string is cached in the *HTTP-CACHE* global variable; if the same
URI is requested more than once, the cached version will subsequently be
returned."
  (or (gethash uri *http-cache*)
      (setf (gethash uri *http-cache*)
            (http-get uri :headers headers))))

(defun http-get (uri &key headers)
  "Makes a GET request to URI, returning the resultant string."
  (dexador:get uri :headers headers :force-string 't))

(defun invert-hash-table (table &optional tf)
  "Return a copy of TABLE with the keys and values inverted.
All values are the new keys for the old keys, and all old keys are…
wait, I’m getting confused now. Oh, whatever, you know what’s up!

Optionally, a TF function can be provided, which will be executed
with the new key as its parameter, and whose return-value will be
the key. Useful for sanitization!"
  (let ((new-table (alexandria:copy-hash-table table)))
    (maphash (lambda (old-key old-val)
               (let ((new-key (if tf (funcall tf old-val) old-val)))
                 (remhash old-key new-table)
                 (setf (gethash new-key new-table) old-key)))
             new-table)
    new-table))

(defun copy-hash-table-to (from to &optional (clobber nil))
  "Shallowly copies the keys+values of hash-table FROM into TO.
If CLOBBER is set, old-values in TO will be overwritten."
  (maphash (lambda (key val)
             (let ((in-to (gethash key to)))
               (when (or (and clobber in-to)
                         (not in-to))
                     (setf (gethash key to) val))))
           from)
  to)

(defun plist-keys (plist)
  "Return a list of keys in a property list."
  (remove-if #'not
   (loop for item in plist
         for i from 0
         collect (when (evenp i) item))))

(defun merge-plists (a b)
  "Merge two property lists, favouring adding all properties of A to B not
pre-existing in B."
  (let ((a-keys (plist-keys a))
        (b-keys (plist-keys b)))
    (loop for key in a-keys
          do (when (not (find key b-keys))
               (setf (getf b key) (getf a key))))
    b))
