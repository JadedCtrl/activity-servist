;;;; json-λd: (Aspiring) parser and encoder for JSON-LD data

;; Copyright © 2024-2025 Jaidyn Ann <jadedctrl@posteo.at>
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
  (:nicknames "AS/JLD" "JSON-LD")
  (:export
   ;; Functions/Macros
   #:parse #:define-json-type
   ;; Symbols
   #:no-@context
   ;; Globals
   #:*default-json-type*
   ;; Classes
   #:object
   ;; Slots/Accessors
   :@context :@id :@type :@etc))

(in-package #:activity-servist/json-ld)


;;; Globals
;;; ————————————————————————————————————————
(defvar *default-json-type* "*"
  "When parsing JSON-LD, objects of unrecognized types will be assumed to be
of this type. Should be a string, the IRI corresponding to a registered type.
For example: “https://www.w3.org/ns/activitystreams#Object”

The default value “*” refers to the base JSON-LD:OBJECT type.")

(defvar *json-types*    (make-hash-table :test #'equal)
  "Stores descriptions of each JSON-type, mapping type-IRI to class-name and property-name to slot-name.
Used during encoding an object to JSON, for finding type/property-names from class/slot-names.
Keys are the type-IRI (e.g., (“https://www.w3.org/ns/activitystreams#Accept”), and values are an irregular association list, of the form:
  ((CLASS-NAME-SYMBOL . TYPE-NAME) (PROPERTY-IRI SLOT-NAME-SYMBOL . PROPERTY-NAME) ⋯)")

(defvar *http-cache*    (make-hash-table :test #'equal)
  "Caches context-texts fetched over HTTP.
Maps URLs to text-content, so we don’t have to download the same context again and again.")

(defvar *http-cache-dirs* (list (asdf:system-relative-pathname :activity-servist/json-ld "schema"))
  "A list of directories used for caching remote JSON-LD caches. The cache is read-only.")



;;; Base class
;;; ————————————————————————————————————————
(defclass object ()
  ((@context
    :initform nil
    :documentation
    "Used as an override for a class’es @context during encoding.
The method @CONTEXT is how the contents of encoded @context is
determined; to change a class’es default/calculated @context, override that
method. This slot is for changing a specific object’s @context.")
   (@id
    :accessor @id
    :documentation
    "Provides the globally unique identifier for an object.")
   (@type
    :accessor @type
    :documentation
    "Identifies the type of an object. Used to determine the corresponding CLOS-object.")
   (@etc
    :initform nil
    :accessor @etc
    :documentation
    "Components of the JSON object which, during parsing, did not match any specific
slot. This is often filled up in the case of undefined node-types or non-adherent
object definitions.")))

(setf (gethash "*" *json-types*)
      '((object)
        ("@context" @context . "@context")
        ("@id" @id . "@id")
        ("@type" @type . "@type")))

(defgeneric @context (obj)
  (:documentation
   "Returns a JSON-LD CLOS object’s @context, for use in JSON-encoding of the
object.
The implementation for the JSON-LD:OBJECT class simply returns the activitystreams
URL.
If you would like to change @context on a class-level, override this method.
If you would like to change it on an object-level, set the @CONTEXT slot."))

(defmethod @context ((obj object))
  (let ((slot-@context (slot-value obj '@context)))
    (unless (eq slot-@context 'no-@context)
      (or slot-@context
          "https://www.w3.org/ns/activitystreams"))))

(defmethod yason:encode-slots progn ((obj object))
  (flatten-contained-contexts obj)
  ;; We only want to encode @context, @id, and @type if the child class doesn’t
  ;; have its own alias for them.
  (labels ((property-alias (property-name type-def)
             "Return either PROPERTY-NAME or its alias. I.E., “@type” → “type”."
             (cddr (assoc property-name type-def :test #'equal)))
           (aliased-prop-p (property-name type-def)
             "Return whether or not PROPERTY-NAME is aliased."
             (not (equal property-name
                         (property-alias property-name type-def)))))
    (let* ((class-name      (class-name (class-of obj)))
           (type-def-w-name (class-json-type-definition class-name))
           (type-def        (cdr type-def-w-name))
           (don’t-context-p (and (aliased-prop-p "@context" type-def)
                                 (slot-value obj '@context)))
           (object-context  (@context obj))
           (object-id       (and (slot-boundp obj '@id)   (@id obj)))
           (object-type     (and (slot-boundp obj '@type) (@type obj)))
           (class-type      (cdar type-def-w-name)))
      ;; Encode @CONTEXT unless it’s done by DEFINE-JSON-TYPE-ENCODER’s encoder,
      ;; which only happens when “@context” is aliased & set at the object-level
      ;; (that is, the slot’s set and we don’t defer to the class).
      (when (and object-context (not don’t-context-p))
        (yason:encode-object-element "@context" object-context))
      ;; Encode @ID unless it’s done by DEFINE-JSON-TYPE-ENCODER’s encoder,
      ;; which is in the case of an alias.
      (when (and object-id (not (aliased-prop-p "@id" type-def)))
        (yason:encode-object-element "@id" object-id))
      ;; Encode @TYPE unless it’s done by DEFINE-JSON-TYPE-ENCODER’s encoder,
      ;; which only happens when “@type” is aliased & set at the object-level.
      (cond ((and object-type   (not (aliased-prop-p "@type" type-def)))
             (yason:encode-object-element "@type"    object-type))
            ((and class-type    (not object-type))
             (yason:encode-object-element (property-alias "@type" type-def)
                                          class-type)))))
  ;; Now encode all properties that don’t have dedicated slots, those in @ETC.
  (mapcar (lambda (alist-cell)
            (yason:encode-object-element (car alist-cell)
                                         (cdr alist-cell)))
          (@etc obj)))

(defmethod yason:encode ((obj object) &optional (stream *standard-output))
  (yason:with-output (stream)
    (yason:encode-object obj)))



;;; CLOS definition
;;; ————————————————————————————————————————
(defmacro define-json-type (names direct-superclasses context direct-slots &rest options)
  "Used to define or update a CLOS class and a JSON encoder/decoder for a
JSON-LD node-type. An instance of class will be output, instead of a hash-table,
when parsing JSON-LD with JSON-LD:PARSE. Instances of this class can be encoded
into JSON with YASON:ENCODE.

NAMES is a pair with two values: The CLOS class-name, and the name used during
encoding as @type’s value. If only the CLOS class-name is provided, @type will
not be encoded for this object.

DIRECT-SUPERCLASSES is a list of JSON-LD CLOS classes, whose slots and context
this should inherit. JSON-LD:OBJECT should be somewhere in the hierarchy, in order
to provide “@context”, “@id”, and “@type”; if no superclasses are provided,
JSON-LD:OBJECT is default.

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
There is one keyword with behavior unlike DEFCLASS, however — :ACCESSOR.

By default, a slot will have an accessor simply named after the slot.
Set :ACCESSOR to NIL to define no accessor at all.

OPTIONS contains ordinary class options, in the format of DEFCLASS (for example,
:DOCUMENTATION).

Here is a brief example partially defining the “Place” type from ActivityStreams:

  (define-json-type (place “Place”) (object) ctx
    ((altitude  “altitude” :initarg altitude
                :documentation “Indicates the altitude of a place.”)
     (latitude  “latitude” :initarg :latitude
                :documentation “The latitude of a place.”)
     (longitude “longitude” :initarg :longitude
                :documentation “The longitude of a place.”)))"
  `(let ((json-class
           (define-json-clos-class ,names
               ,(or direct-superclasses `(json-ld:object))
             ,direct-slots
             ,options)))
     (define-json-type-encoder ,(car names) ,direct-slots)
     (register-json-type ',names (or ',direct-superclasses '(json-ld:object)) ',direct-slots ,context)
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
   (merge-plists (json-type-implicit-slot-options slot-name)
                 slot-opts)))

(defun json-type-implicit-slot-options (slot-name)
  "Return default property-list slot options for a json-type CLOS class."
  (list :accessor (intern (format nil "~A" slot-name))))

(defun json-type-normalize-slot-options (slot-opts)
  "Take property-list slot options from a DEFINE-JSON-TYPE format and massage it
into a DEFCLASS format."
  (if (and (find :accessor slot-opts)
           (not (getf slot-opts :accessor)))
      (alexandria:remove-from-plist slot-opts :accessor)
      slot-opts))



;;; Encoding
;;; ————————————————————————————————————————
(defmacro define-json-type-encoder (class direct-slots)
  "Helper-macro for DEFINE-JSON-CLOS-CLASS.
This actually defines the YASON-encoder for a JSON-LD node-type.
CLASS is the class-name; see DEFINE-JSON-TYPE’s docstring about DIRECT-SLOTS."
  `(progn
     (defmethod yason:encode-slots progn ((obj ,class))
       ,(append
         `(progn)
         (mapcar (lambda (slot)
                   `(when (slot-boundp obj ',(car slot))
                      (yason:encode-object-element ,(cadr slot) (slot-value obj ',(car slot)))))
                 direct-slots)))))



;;; Parsing
;;; ————————————————————————————————————————
(defun register-json-type (names direct-superclasses direct-slots context)
  "Register a JSON node-type. This allows PARSE to recognize the type (and
corresponding CLOS class) of a node."
  (let* ((ctx       (parse-context context))
         (type-iri  (getf (gethash (cadr names) ctx) :id))
         (type-name (or type-iri (cadr names))))
    ;; Save the JSON-type.
    (setf (gethash type-name *json-types*)
          (json-type-registry-list names direct-superclasses ctx direct-slots))))

(defun json-type-registry-list (names direct-superclasses parsed-context direct-slots)
  "Return a REGISTER-JSON-TYPE-formed registry entry, a simple list of the form:
  (TYPE-IRI (PROPERTY-NAME SLOT-NAME) ⋯ (PROPERTY-NAME SLOT-NAME))
… where TYPE-IRI is the (hopefully) fully-resolved IRI form of the node-type’s
name, though it might be unresolved if context was unprovided or lacking."
  (remove-duplicates
   (append
    ;; The class-name and type-name.
    (list (cons (car names) (cadr names)))
    ;; Add the class’es direct slots.
    (mapcar
     (lambda (slot)
       (when (cadr slot)
         (let* ((property-name (cadr slot))
                (slot-name     (car slot))
                (ctx-item      (gethash property-name parsed-context))
                (url           (or (getf ctx-item :id)
                                   property-name)))
           (cons url (cons slot-name property-name)))))
     direct-slots)
    ;; Add the slots of parent-classes.
    (reduce (lambda (slots-a slots-b)
              (append slots-a slots-b))
            (mapcar (lambda (class-name)
                      (let* ((type-def (class-json-type-definition class-name)))
                        (when type-def
                          (cdr type-def))))
                    direct-superclasses)))
   :test (lambda (a b) (equal (car a) (car b)))
   :from-end 't))

(defun parse (str &optional always-object)
  "Parse the JSON-LD document contained in STR.
If ALWAYS-OBJECT is non-nil, even invalid JSON-LD objects (which would normally be
parsed into hash-tables) will be parsed into CLOS objects."
  (let ((ctx     (make-hash-table :test #'equal)) ; Parsed context
        (parsed (yason:parse str)))
    (when always-object
      (setf (gethash ".always-object" ctx) 't))
    (values (parse-item parsed ctx)
            ctx)))

(defun parse-item (item &optional ctx)
  "Parse an individual ITEM of a YASON-decoded JSON-LD document."
  (typecase item
    (hash-table (parse-table item ctx))
    (list       (mapcar (lambda (a) (parse-item a ctx)) item))
    (T          item)))

(defun parse-table (table &optional ctx)
  "Parse a JSON “node object” (as decoded by YASON into a hash-TABLE."
  (let ((ctx (parse-context (gethash "@context" table) ctx)))
    ;; Now, actually parse.
    (let* ((parsed-table (parse-table-inplace table ctx))
           (type         (identify-json-type parsed-table ctx))
           (type-def     (or (gethash type                *json-types*)
                             (gethash *default-json-type* *json-types*)))
           (valid-object-p type))
      (if (or valid-object-p (gethash ".always-object" ctx))
          (parse-table-into-object parsed-table type-def ctx)
          parsed-table))))

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

(defun parse-table-into-object (table type-def ctx)
  "Parse an expanded-form JSON-LD object (TABLE) into a CLOS object.
TYPE-DEF is a type-definition list of the form found in *JSON-TYPES* and made
by REGISTER-JSON-TYPE.
CTX is the according parsed-context."
  (let ((obj (make-instance (caar type-def))))
    (maphash
     (lambda (property value)
       (let* ((property-def  (assoc property type-def :test #'equal))
              (slot-name     (second property-def))
              (etc-value     (slot-value obj '@etc)))
         (if property-def
             (setf (slot-value obj slot-name) value)
             (setf (slot-value obj '@etc)
                   (append etc-value
                           (list (cons property value)))))))
     table)
    (setf (slot-value obj '@context) (gethash "@context" table))
    obj))

(defun identify-json-type (table ctx)
  "Given an parsed JSON-LD object’s hash-TABLE, return the name/IRI of the
JSON-type that best suits the object — using the types registered into
*JSON-TYPES* with REGISTER-JSON-TYPE."
  (let* ((type (gethash "@type" table)))
    (or (getf (gethash type ctx) :id)
        type)))

(defun class-json-type-name (class-name)
  "Return the name (IRI) of a registered JSON-type its CLOS class’es name."
  (loop for iri      being the hash-keys   in *json-types*
        for registry being the hash-values in *json-types*
        if (eq class-name (caar registry))
          return iri))

(defun superclass-json-type-name (class-name)
  "Return the name (IRI) of a registered JSON-type from the name of its CLOS class
or any inheriting thereof."
  (let ((class (find-class class-name)))
    (closer-mop:finalize-inheritance class)
    (dolist (superclass (closer-mop:class-precedence-list class))
      (let ((name (class-json-type-name (class-name superclass))))
        (if name (return name))))))

(defun class-json-type-definition (class-name)
  "Return the type-definition from the the registry of JSON types (*JSON-TYPES*),
based on a CLOS class-name. It is of the form,
  ((CLASS-NAME TYPE-NAME) (SLOT-NAME PROPERTY-NAME) ⋯ (SLOT-NAME PROPERTY-NAME))"
    (gethash (superclass-json-type-name class-name) *json-types*))



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
  (let* ((str     (caching-http-get uri))
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
                (setf (gethash term ctx)
                      (list :id parsed-id :type type))
                (setf (gethash (format nil ".~A" parsed-id) ctx)
                      term)))))
     table)
    unresolvable))



;;; Context-normalization
;;; ————————————————————————————————————————
(defun flatten-contained-contexts (obj)
  "“Flattens” the @CONTEXTs of a JSON-LD object and the @CONTEXTs of any contained
objects, recursively. That is, redundant @CONTEXT-definitions are removed — we
try to concentrate everything in the top-level object’s @CONTEXT slot.
This is useful for ensuring the same @CONTEXT doesn’t get output a million times
during JSON-encoding with YASON:ENCODE."
  (dolist (subobj (cdr (contained-json-objects obj)))
    (progn
      (let ((old-context    (@context obj))
            (old-subcontext (@context subobj)))
        (when (and old-subcontext
                   (not (equal old-context old-subcontext)))
          (setf (slot-value obj '@context)
                (append (if (listp old-context)
                            old-context
                            (list old-context))
                        old-subcontext))))
      (setf (slot-value subobj '@context) 'no-@context))))

(defun json-slot-values (obj)
  "Return the values of all registered slots/properties of a JSON-LD:OBJECT.
Unregistered slots that don’t get encoded/decoded are ignored."
  (let* ((type-def  (class-json-type-definition (class-name (class-of obj))))
         (slot-defs (cdr type-def)))
    (remove-if
     #'not
     (append
      (mapcar (lambda (slot-def)
                (let* ((slot-name  (cadr slot-def)))
                  (and (slot-exists-p obj slot-name)
                       (slot-boundp obj slot-name)
                       (slot-value obj slot-name))))
              slot-defs)
      (mapcar (lambda (etc-cons)
                (cdr etc-cons))
              (slot-value obj '@etc))))))

(defun contained-json-objects (item)
  "Given ITEM of arbitrary type, return all JSON-LD:OBJECTs contained within,
recursively. Lists, hash-tables, and the slots of JSON-LD:OBJECTs are explored."
  (typecase item
    (cons       (reduce
                 (lambda (a b)
                   (append a b))
                 (mapcar (lambda (a) (contained-json-objects a))
                         item)))
    (hash-table (let ((ret '()))
                  (maphash
                   (lambda (k v)
                     (setq ret
                           (append ret (contained-json-objects v))))
                   item)
                  ret))
    (object     (append
                 (list item)
                 (reduce
                  (lambda (&optional b c)
                    (or b
                        (append b c))
                   (append b c))
                  (mapcar (lambda (a) (contained-json-objects a))
                          (json-slot-values item)))))))



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
(defun caching-http-get (uri &key (accept "application/json,application/ld+json"))
  "Makes a GET request to URI, returning the resultant string.
Each resultant string is cached in the *HTTP-CACHE* global variable; if the same
URI is requested more than once, the cached version will subsequently be
returned.
In addition, there is a filesystem cache; if a file named afer the URI
(sans the protocol, replacing slashes '/' with hyphens '-') is found in
the directories *HTTP-CACHE-DIRS*, its contents will be returned instead."
  (or (gethash uri *http-cache*)       ; Try our downloaded/read-file cache.
      (setf (gethash uri *http-cache*) ; Try our read-only filesystem cache...
            (let* ((file-leaf       (str:replace-all "/" "-" (uri-sans-scheme uri)))
                   (cached-filepath (find-file file-leaf *http-cache-dirs*)))
              (when cached-filepath
                (alexandria:read-file-into-string cached-filepath))))
      (setf (gethash uri *http-cache*) ; If not cached, download & cache it.
            (as/u:http-get uri :accept accept))))

(defun find-file (file-leaf dirs)
  "Search for a file of the given name FILE-LEAF within directories DIRS.
Returns the first found matching file."
  (or (find file-leaf (uiop:directory-files (format nil "~A/*" (car dirs)))
            :test (lambda (a b)
                    (equal a (file-namestring b))))
      (unless (not (cdr dirs))
        (get-file-from-dirs file-leaf (cdr dirs)))))

(defun uri-sans-scheme (uri)
  "Returns a URI string without its scheme."
  (str:replace-all
   (format nil "~A://" (quri:uri-scheme (quri:uri "https://dad.com/")))
   ""
   uri))

(defun plist-keys (plist)
  "Return a list of keys in a property list."
  (remove-if #'not
   (loop for item in plist
         for i from 0
         collect (when (evenp i) item))))

(defun merge-plists (a b &optional clobberp)
  "Merge two property lists, adding all properties of A to B not pre-existing
in B. If CLOBBERP is set, pre-existing properties of B will be overwritten."
  (let ((a-keys (plist-keys a))
        (b-keys (plist-keys b)))
    (dolist (key a-keys)
      (when (or clobberp (not (find key b-keys)))
        (setf (getf b key) (getf a key))))
    b))
