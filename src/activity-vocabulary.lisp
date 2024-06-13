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
  ;; One should never USE this package, since some class-names shadow
  ;; core Common Lisp symbols! Beware! :P
  (:shadow #:delete #:ignore #:listen #:read #:remove)
  (:nicknames "AP-S/AV" "AV"))

(in-package #:activitypub-servist/activity-vocabulary)


;;; Macros
;;; ————————————————————————————————————————
(defmacro defclass-w-accessors (name direct-superclasses slots &rest options)
  "Identical to DEFCLASS, but with one convenience: A slot definition, if being
simply a symbol, will default to a slot with an accessor and init-arg named after the
symbol. The init-arg will be “:symbol”, and the accessor will be “classname-symbol”.
For instance,

    (defclass-w-accessors PERSON () (AGE
                                     HEIGHT
                                     (NAME :INIT-FORM “Unknown”)))
```
is equivalent to
```
    (defclass PERSON () ((AGE    :INIT-ARG :AGE    :ACCESSOR PERSON-AGE)
                         (HEIGHT :INIT-ARG :HEIGHT :ACCESSOR PERSON-HEIGHT)
                         (NAME   :INIT-FORM “Unknown”)))
```"
  `(defclass ,name ,direct-superclasses
     ,(mapcar
       (lambda (slot)
         (typecase slot
           (list slot)
           (t (list slot :accessor (intern (format nil "~A-~A" name slot))
                         :initarg  (intern (symbol-name slot) "KEYWORD")))))
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



;;; Core types
;;; ————————————————————————————————————————
;; https://www.w3.org/ns/activitystreams#Object
(defclass-w-accessors object ()
  (
   attachment attributed-to audience bcc bto cc content context
   duration end-time generator icon image in-reply-to location
   media-type name preview published replies start-time summary
   tag to updated url))

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
(defclass-empty-children actor
  (application group organization person service))
