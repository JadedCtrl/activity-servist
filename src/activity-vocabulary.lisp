;;;; activity-vocabulary: Base classes for ActivityStreams.

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

(defpackage #:activity-servist/activity-vocabulary
  (:use #:cl)
  (:nicknames "AS/AV" "ACTIVITY-VOCABULARY")
  (:shadow #:block #:delete #:ignore #:listen #:read #:remove)
  ;; One should never USE this package, since some class-names shadow
  ;; core Common Lisp symbols! Beware! :P
  (:export
   ;; Classes
   :accept :activity :add :announce :application :arrive :article :audio :block
   :collection :collection-page :create :delete :dislike :document :event :flag
   :follow :group :ignore :ignore :image :intransitive-activity :invite :join
   :leave :like :link :listen :move :note :object :offer :ordered-collection
   :ordered-collection-page :organization :page :person :place :profile
   :question :read :reject :relationship :remove :service :tentative-accept
   :tentative-reject :tombstone :travel :undo :update :video :view
   ;; Slots
   :activity-actor :activity-instrument :activity-object :activity-origin
   :activity-result :activity-target
   :collection-current :collection-first :collection-items :collection-last
   :collection-total-items
   :collection-page-next :collection-page-part-of :collection-page-prev
   :link-height :link-href :link-hreflang :link-media-type :link-name
   :link-preview :link-rel :link-summary :link-width
   :object-attachment :object-attributed-to :object-audience :object-bcc
   :object-bto :object-cc :object-content :object-context :object-duration
   :object-end-time :object-generator :object-icon :object-image
   :object-in-reply-to :object-location :object-media-type :object-name
   :object-preview :object-published :object-replies :object-start-time
   :object-summary :object-tag :object-to :object-type :object-updated
   :object-url
   :ordered-collection-page-start-index ordered-collection-ordered-items
   :place-accuracy :place-altitude :place-latitude :place-longitude
   :place-radius :place-units
   :question-all-of :question-closed :question-one-of
   :profile-describes
   :relationship-object :relationship-relationship :relationship-subject
   :tombstone-former-type :tombstone-deleted))

(in-package #:activity-servist/activity-vocabulary)

(setq activity-servist/activity-streams:*default-class*
      'activity-servist/activity-vocabulary:object)


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



;;; Core types
;;; ————————————————————————————————————————
;; https://www.w3.org/ns/activitystreams#Object
(defclass-w-accessors object (activity-servist/activity-streams:object)
  (
   attachment attributed-to audience bcc bto cc content context
   duration end-time generator icon id image in-reply-to location
   media-type name preview published replies start-time summary
   tag to updated url))

;; https://www.w3.org/ns/activitystreams#Link
;; “summary” here isn’t real! It’s not a property Link should have (just
;; looking at Link’s properties), but it’s implied by the Mention example.
(defclass-w-accessors link (activity-servist/activity-streams:object)
  (height href hreflang media-type name preview rel summary width))

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
;; Funnily enough, “orderedItems” is actually a ghost! It’s only *implied*. :-P
;; https://jam.xwx.moe/notice/AjE1LkpLoBvWmDUmK8
(defclass-w-accessors ordered-collection (collection)
  (ordered-items))

;; https://www.w3.org/ns/activitystreams#CollectionPage
(defclass-w-accessors collection-page (collection)
  (next part-of prev))

;; https://www.w3.org/ns/activitystreams#OrderedCollectionPage
(defclass-w-accessors ordered-collection-page (collection-page ordered-collection)
  (start-index))



;;; Extended Activity types
;;; ————————————————————————————————————————
(defclass-empty-children activity
  (accept add announce block create delete dislike flag follow ignore join leave
        like listen move offer read reject remove travel undo update view))

(defclass arrive (intransitive-activity) ())
(defclass ignore (block) ())
(defclass invite (offer) ())
(defclass question (intransitive-activity) ())
(defclass tentative-accept (accept) ())
(defclass tentative-reject (reject) ())

(defclass-w-accessors question (intransitive-activity)
  (any-of closed one-of))



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



;;; Defining YASON:ENCODE-SLOTS
;;; ————————————————————————————————————————
;; On-the-fly define YASON:ENCODE-SLOTS for each of our distinct AP classes.
(as/as:define-class-encoders
    (mapcar #'find-class
            '(object link activity collection collection-page ordered-collection
              ordered-collection-page place profile question relationship tombstone)))
