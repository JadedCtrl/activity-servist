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
  (:nicknames "AP-S/AV" "AV"))

(in-package #:activitypub-servist/activity-vocabulary)


;;; Macros
;;; ————————————————————————————————————————
(defmacro defclass-w-accessors (name direct-superclasses slots &rest options)
  `(defclass ,name ,direct-superclasses
     ,(mapcar
       (lambda (slot)
         (typecase slot
           (list slot)
           (t (list slot :accessor slot :initarg (intern (symbol-name slot) "KEYWORD")))))
       slots)
     ,@options))



;;; Core types
;;; ————————————————————————————————————————
;; https://www.w3.org/ns/activitystreams#Object
(defclass-w-accessors av-object ()
  (
   attachment attributed-to audience bcc bto cc content context
   duration end-time generator icon image in-reply-to location
   media-type name preview published replies start-time summary
   tag to updated url))

;; https://www.w3.org/ns/activitystreams#Link
(defclass-w-accessors av-link ()
  (height href hreflang media-type name preview rel width))

;; https://www.w3.org/ns/activitystreams#Activity
(defclass-w-accessors av-activity (av-object)
  (actor instrument object origin result target))

;; Should be ordinary Activity, sans `object`.
;; https://www.w3.org/ns/activitystreams#IntransitiveActivity
(defclass av-intransitive-activity (av-activity) ())

;; https://www.w3.org/ns/activitystreams#Collection
(defclass-w-accessors av-collection (av-object)
  (current first items last total-items))

;; https://www.w3.org/ns/activitystreams#OrderedCollection
(defclass av-ordered-collection (av-collection) ())

;; https://www.w3.org/ns/activitystreams#CollectionPage
(defclass-w-accessors av-collection-page (av-collection)
   (next part-of prev))

;; https://www.w3.org/ns/activitystreams#OrderedCollectionPage
(defclass-w-accessors av-ordered-collection-page (av-collection-page)
  (startIndex))




;;; Extended Activity types
;;; ————————————————————————————————————————
(defclass-w-accessors av-
