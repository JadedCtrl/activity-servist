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
  (:nicknames "AS/JSON-LD" "JSON-LD"))

(in-package #:activity-servist/json-ld)


;;; Globals
;;; ————————————————————————————————————————
(defvar *http-cache* (make-hash-table :test #'equal))



;;; Parsing
;;; ————————————————————————————————————————
(defun parse (str)
  "Parse the JSON-LD document contained in STR."
  (let ((ctx    (make-hash-table :test #'equal))
        (parsed (yason:parse str)))
    (values (parse-item parsed ctx)
            ctx)))

(defun parse-item (item &optional ctx)
  "Parse an individual ITEM of a YASON-decoded JSON-LD document."
  (typecase item
    (hash-table (parse-object item ctx))
    (list       (mapcar (lambda (a) (parse-item a ctx)) item))
    (T          item)))

(defun parse-object (table &optional ctx)
  "Parse a JSON “node object” (as decoded by YASON into a hash-TABLE."
  (let ((ctx (parse-context (gethash "@context" table) ctx)))
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
    table))



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
   (:otherwise nil)))

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
