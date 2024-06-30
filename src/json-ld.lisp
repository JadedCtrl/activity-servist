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

(defun parse (str)
  (let ((ctx    (make-hash-table :test #'equal))
        (parsed (yason:parse str)))
    (values (parse-item parsed ctx)
            ctx)))

(defun parse-item (item &optional ctx)
  (typecase item
    (hash-table (parse-object item ctx))
    (list       (mapcar (lambda (a) (parse-item a ctx)) item))
    (T          item)))

(defun parse-object (table &optional ctx)
  (let ((ctx (parse-context (gethash "@context" table) ctx)))
    (maphash
     (lambda (key val)
       (alexandria:when-let ((key-iri (gethash key ctx)))
         (remhash key table)
         (setf (gethash key-iri table)
               (parse-item val ctx))))
     table)
    table))

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
  "Helper function for PARSE-CONTEXT. Takes an association list of
UNRESOLVED-TERMS, and repeats context parsing  until iterations aren’t useful or
all terms are resolved and parsed.
These unresolved terms are generally keys mapped to compacted IRIs, whose prefix
(which ought be a separate term in CTX) hasn’t yet been parsed. A compacted IRI
looks like “prefix:suffix”.
  UNRESOLVED-TERMS => ((“Laughing” . “xd:laughing”))"
  (let* ((unresolved-table (alexandria:alist-hash-table unresolved-terms))
         (now-unresolved   (parse-context-map ctx unresolved-table)))
    (cond ((not now-unresolved)
           ctx)
          ((eq (length now-unresolved)
               (length unresolved-terms))
           (values ctx now-unresolved)
           (error 'unresolved :message
                  (format nil "Compact IRI could not be resolved: ~A" now-unresolved)))
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
  "Parse a remote JSON-LD context at URI, adding its keys to the CTX
hash-table. Currently a stub, unimplemented."
  nil)

(defun parse-context-map (ctx table)
  "Parse an map item of a JSON-LD @context (which has been parsed by YASON into
a hash-TABLE).
Add all terms found to the hash-table CTX.
Returns an association list of terms that couldn’t be resolved, likely compacted
IRI values whose prefix hasn’t yet been parsed into CTX."
  (let ((unresolvable '()))
    (maphash
     (lambda (term val)
       (let* ((iri
                (typecase val
                  (string val)
                  (hash-table (gethash "@id" val))))
              (noncompact-iri
                (if (ld-keyword-p iri)
                    iri
                    (uncompact-iri iri ctx))))
         (cond ((and (compacted-iri-p iri)
                     (not noncompact-iri))
                (push (cons term iri) unresolvable))
               ((not (gethash term ctx))
                (setf (gethash term ctx) noncompact-iri)))))
     table)
    unresolvable))

(defun ld-keyword-p (str)
  "Return whether or not a string is a JSON-LD keyword."
  (member (string-downcase str)
          '("@base" "@container" "@context" "@direction" "@graph" "@id" "@import"
            "@included" "@index" "@json" "@language" "@list" "@nest" "@none"
            "@prefix" "@propagate" "@protected" "@reverse" "@set" "@type"
            "@value" "@version" "@vocab")))

(defun uncompact-iri (iri ctx)
  "Given a comapcted IRI, uncompact it into its full form, with CTX being a
hash-table containing its context."
  (if (compacted-iri-p iri)
      (destructuring-bind (prefix suffix)
          (str:split #\: iri)
        (alexandria:when-let ((prefix-iri (gethash (string-downcase prefix) ctx)))
         (format nil "~A~A" prefix-iri suffix)))
      iri))

(defun compacted-iri-p (iri)
  "Return whether or not an IRI is in compacted prefix:suffix form."
  (and (find #\: iri)
       (not (search "://" iri))
       (not (equal iri "_:"))))
