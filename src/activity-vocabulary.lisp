;;;; activity-vocabulary: Base vocabulary classes for ActivityStreams.

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

(defpackage #:activity-servist/vocab/activity
  (:use #:cl)
  (:nicknames "AS/V/A" "ACTIVITY-VOCABULARY" "ACTIVITY")
  (:shadow #:block #:delete #:first #:ignore #:last #:listen #:read #:remove)
  ;; One should never USE this package, since some class-names shadow
  ;; core Common Lisp symbols! Beware! :P
  (:export
   ;; Classes
   :accept :activity :actor :add :announce :application :arrive :article :audio
   :block :collection :collection-page :create :delete :dislike :document
   :event :flag :follow :group :ignore :image :intransitive-activity :invite
   :join :leave :like :link :listen :mention :move :note :object :offer
   :ordered-collection :ordered-collection-page :organization :page :person
   :place :profile :question :read :reject :relationship :remove :service
   :tentative-accept :tentative-reject :tombstone :travel :undo :update :video
   :view
   ;; Slots/Accessors
   :accuracy :actor :altitude :any-of :arrive :attachment :attributed-to
   :audience :bcc :bto :cc :closed :content :context :current :deleted
   :describes :duration :endpoints :end-time :first :followers :following
   :former-type :generator :height :href :hreflang :icon :image :inbox
   :in-reply-to :instrument :items :last :latitude :liked :location :longitude
   :media-type :name :next :object :one-of :ordered-items :origin :outbox
   :part-of :preferred-username :prev :preview :published :radius :rel
   :relationship :replies :result :start-index :start-time :streams :subject
   :summary :tag :target :to :total-items :units :updated :url :width))

(in-package #:activity-servist/vocab/activity)

(defmacro define-json-empty-types (superclasses context &rest direct-children)
  "For each list of DIRECT-CHILDREN, a “hollow” JSON subtype and CLOS subclass
ofE SUPERCLASSES will be created, with the given JSON-LD context CONTEXT.
These new subclasses have no slots of its own — they will be empty derivatives
of SUPERCLASSES.

Items of DIRECT-CHILDREN should be of the form,
   (CLASS-NAME “typeName” “Documenation-string describing the subclass.”)"
  (append
   '(progn)
   (mapcar (lambda (subclass-list)
             (let ((class-name    (cl:first subclass-list))
                   (type-name     (second   subclass-list))
                   (documentation (third    subclass-list)))
               `(json-ld:define-json-type (,class-name ,type-name) ,superclasses ,context
                  ()
                  (:documentation ,documentation))))
           direct-children)))



;;; Core types
;;; ————————————————————————————————————————
;; https://www.w3.org/ns/activitystreams#Object
;; The root of all evil in the world.
(json-ld::define-json-type (object "Object") () "https://www.w3.org/ns/activitystreams"
  ((json-ld:@id
    "id"
    :documentation "Provides the globally unique identifier for an Object.")
   (json-ld:@type
    "type"
    :documentation "Identifies the Object type. Multiple values may be specified.")
   (attachment
    "attachment"
    :documentation "Identifies a resource attached or related to an object that potentially requires special handling. The intent is to provide a model that is at least semantically similar to attachments in email.")
   (attributed-to
    "attributedTo"
    :documentation "Identifies one or more entities to which this object is attributed. The attributed entities might not be Actors. For instance, an object might be attributed to the completion of another activity.")
   (audience
    "audience"
    :documentation "Identifies one or more entities that represent the total population of entities for which the object can considered to be relevant.")
   (content
    "content"
    :documentation "The content or textual representation of the Object encoded as a JSON string. By default, the value of content is HTML. The media-type property can be used in the object to indicate a different content type.
The content MAY be expressed using multiple language-tagged values. ")
   (context
    "context"
    :documentation "Identifies the context within which the object exists or an activity was performed.
The notion of “context” used is intentionally vague. The intended function is to serve as a means of grouping objects and activities that share a common originating context or purpose. An example could be all activities relating to a common project or event.")
   (name
    "name"
    :documentation "A simple, human-readable, plain-text name for the object. HTML markup MUST NOT be included. The name MAY be expressed using multiple language-tagged values.")
   (end-time
    "endTime"
    :documentation "The date and time describing the actual or expected ending time of the object. When used with an Activity object, for instance, the end-time property specifies the moment the activity concluded or is expected to conclude.")
   (generator
    "generator"
    :documentation "Identifies the entity (e.g. an application) that generated the object.")
   (icon
    "icon"
    :documentation "Indicates an entity that describes an icon for this object. The image should have an aspect ratio of one (horizontal) to one (vertical) and should be suitable for presentation at a small size.")
   (image
    "image"
    :documentation "Indicates an entity that describes an image for this object. Unlike the icon property, there are no aspect ratio or display size limitations assumed.")
   (in-reply-to
    "inReplyTo"
    :documentation "Indicates one or more entities for which this object is considered a response.")
   (location
    "location"
    :documentation "Indicates one or more physical or logical locations associated with the object.")
   (preview
    "preview"
    :documentation "Identifies an entity that provides a preview of this object.")
   (published
    "published"
    :documentation "The date and time describing the actual or expected starting time of the object. When used with an Activity object, for instance, the start-time property specifies the moment the activity began or is scheduled to begin.")
   (replies
    "replies"
    :documentation "Identifies a Collection containing objects considered to be responses to this object.")
   (start-time
    "startTime"
    :documentation "The date and time describing the actual or expected starting time of the object. When used with an Activity object, for instance, the start-time property specifies the moment the activity began or is scheduled to begin.")
   (summary
    "summary"
    :documentation "A natural language summarization of the object encoded as HTML. Multiple language tagged summaries MAY be provided.")
   (tag
    "tag"
    :documentation "One or more “tags” that have been associated with an objects. A tag can be any kind of Object. The key difference between attachment and tag is that the former implies association by inclusion, while the latter implies associated by reference.")
   (updated
    "updated"
    :documentation "The date and time at which the object was updated.")
   (url
    "url"
    :documentation "Identifies one or more links to representations of the object.")
   (to
    "to"
    :documentation "Identifies an entity considered to be part of the public primary audience of an Object.")
   (bto
    "bto"
    :documentation "Identifies an Object that is part of the private primary audience of this Object.")
   (cc
    "cc"
    :documentation "Identifies an Object that is part of the public secondary audience of this Object.")
   (bcc
    "bcc"
    :documentation "Identifies one or more Objects that are part of the private secondary audience of this Object.")
   (media-type
    "mediaType"
    :documentation "Identifies the MIME media type of the value of the content property. If not specified, the content property is assumed to contain text/html content.")
   (duration
    "duration"
    :documentation "When the object describes a time-bound resource, such as an audio or video, a meeting, etc, the duration property indicates the object's approximate duration. The value MUST be expressed as an xsd:duration as defined by [ xmlschema11-2], section 3.3.6 (e.g. a period of 5 seconds is represented as “PT5S”).")

   ;; Required actor slots
   (inbox
    "inbox"
    :documentation "A reference to an OrderedCollection comprised of all the messages received by the actor.")
   (outbox
    "outbox"
    :documentation "An OrderedCollection comprised of all the messages produced by the actor.")
   (following
    "following"
    :documentation "A link to a collection of the actors that this actor is following.")
   (followers
    "followers"
    :documentation "A link to a collection of the actors that follow this actor.")

   ;; Optional Actor slots
   (liked
    "liked"
    :documentation "A link to a collection of objects this actor has liked.")
   (streams
    "streams"
    :documentation "A list of supplementary Collections which may be of interest.")
   (preferred-username
    "preferredUsername"
    :documentation "A short username which may be used to refer to the actor, with no uniqueness guarantees.")
   (endpoints
    "endpoints"
    :documentation "A JSON object which maps additional (typically server/domain-wide) endpoints which may be useful either for this actor or someone referencing this actor. This mapping may be nested inside the actor document as the value or may be a link to a JSON-LD document with these properties.
May include the properties “proxyUrl”, “oauthAuthorizationEndpoint”, “oauthTokenEndpoint”, “provideClientKey”, “signClientKey”, and “sharedInbox”.
See the spec for details on these properties: https://www.w3.org/TR/activitypub/#proxyUrl"))

  ;; Class docstring
  (:documentation
   "Describes an object of any kind. The Object type serves as the base type for most of the other kinds of objects defined in the Activity Vocabulary, including other Core types such as Activity, IntransitiveActivity, Collection and OrderedCollection."))


;; https://www.w3.org/TR/activitypub/#x4-actors
;; A dummy ACTOR class, for convenience sake.
(defclass actor ()
  ()
  (:documentation "A dummy-class that represents an Actor ActivityPub object.
There is, in reality, no “Actor” supertype in ActivityPub; but it might be useful in practice to treat all Actor-subtypes similarly in some cases, like method definition. Hence the existence of this class."))


;; https://www.w3.org/ns/activitystreams#Link
;; “summary” here isn’t real! It’s not a property Link should have (just
;; looking at Link’s properties), but it’s implied by the Mention example.
(json-ld:define-json-type (link "Link") ()
  "https://www.w3.org/ns/activitystreams"
  ((json-ld:@id
    "id"
    :documentation "Provides the globally unique identifier for a Link.")
   (json-ld:@type
    "type"
    :documentation "Identifies the Link type. Multiple values may be specified.")
   (height
    "height"
    :documentation "On a Link, specifies a hint as to the rendering height in device-independent pixels of the linked resource.")
   (href
    "href"
    :documentation "The target resource pointed to by a Link.")
   (hreflang
    "hreflang"
    :documentation "Hints as to the language used by the target resource. Value MUST be a [BCP47] Language-Tag.")
   (media-type
    "mediaType"
    :documentation "Identifies the MIME media type of the referenced resource.")
   (name
    "name"
    :documentation "A simple, human-readable, plain-text name for the object. HTML markup MUST NOT be included. The name MAY be expressed using multiple language-tagged values. ")
   (preview
    "preview"
    :documentation "Identifies an entity that provides a preview of this object. ")
   (rel
    "rel"
    :documentation "A link relation associated with a Link. The value MUST conform to both the [HTML5] and [RFC5988] “link relation” definitions.
In the [HTML5], any string not containing the “space” U+0020, “tab” (U+0009), “LF” (U+000A), “FF” (U+000C), “CR” (U+000D) or “,” (U+002C) characters can be used as a valid link relation.")
   (summary
    "summary"
    :documentation "A natural language summarization of the object encoded as HTML. Multiple language tagged summaries MAY be provided.")
   (width
    "width"
    :documentation "Specifies a hint as to the rendering width in device-independent pixels of the linked resource.")))


;; https://www.w3.org/ns/activitystreams#Activity
(json-ld:define-json-type (activity "Activity") (object)
  "https://www.w3.org/ns/activitystreams"
  ((actor
    "actor"
    :documentation "Describes one or more entities that either performed or are expected to perform the activity. Any single activity can have multiple actors. The actor MAY be specified using an indirect Link.")
   (object
    "object"
    :documentation "Describes the direct object of the activity. For instance, in the activity “John added a movie to his wishlist”, the object of the activity is the movie added.")
   (target
    "target"
    :documentation "Describes the indirect object, or target, of the activity. The precise meaning of the target is largely dependent on the type of action being described but will often be the object of the English preposition ”to”. For instance, in the activity “John added a movie to his wishlist”, the target of the activity is John's wishlist. An activity can have more than one target.")
   (result
    "result"
    :documentation "Describes the result of the activity. For instance, if a particular action results in the creation of a new resource, the result property can be used to describe that new resource.")
   (origin
    "origin"
    :documentation "Describes an indirect object of the activity from which the activity is directed. The precise meaning of the origin is the object of the English preposition “from”. For instance, in the activity “John moved an item to List B from List A”, the origin of the activity is “List A”.")
   (instrument
    "instrument"
    :documentation "Identifies one or more objects used (or to be used) in the completion of an Activity."))
  (:documentation "An Activity is a subtype of Object that describes some form of action that may happen, is currently happening, or has already happened. The Activity type itself serves as an abstract base type for all types of activities. It is important to note that the Activity type itself does not carry any specific semantics about the kind of action being taken."))


;; Should be ordinary Activity, sans `object`.
;; https://www.w3.org/ns/activitystreams#IntransitiveActivity
(json-ld:define-json-type (intransitive-activity "IntransitiveActivity") (activity)
  "https://www.w3.org/ns/activitystreams"
  ()
  (:documentation "Instances of IntransitiveActivity are a subtype of Activity representing intransitive actions. The object property is therefore inappropriate for these activities."))


;; https://www.w3.org/ns/activitystreams#Collection
(json-ld:define-json-type (collection "Collection") (object)
  "https://www.w3.org/ns/activitystreams"
  ((total-items
    "totalItems"
    :documentation "A non-negative integer specifying the total number of objects contained by the logical view of the collection. This number might not reflect the actual number of items serialized within the Collection object instance.")
   (current
    "current"
    :documentation "In a paged Collection, indicates the page that contains the most recently updated member items.")
   (first
    "first"
    :documentation "In a paged Collection, indicates the furthest preceeding page of items in the collection. ")
   (last
    "last"
    :documentation "In a paged Collection, indicates the furthest proceeding page of the collection.")
   (items
    "items"
    :documentation "Identifies the items contained in a collection. The items might be ordered or unordered."))
  (:documentation "A Collection is a subtype of Object that represents ordered or unordered sets of Object or Link instances."))


;; https://www.w3.org/ns/activitystreams#OrderedCollection
;; Funnily enough, “orderedItems” is actually a ghost! It’s only *implied*. :-P
;; https://jam.xwx.moe/notice/AjE1LkpLoBvWmDUmK8
(json-ld:define-json-type (ordered-collection "OrderedCollection") (collection)
  "https://www.w3.org/ns/activitystreams"
  ((ordered-items
    "orderedItems"
    :documentation "Identifies the items contained in a collection. The items are necessarily ordered."))
  (:documentation "A subtype of Collection in which members of the logical collection are assumed to always be strictly ordered."))


;; https://www.w3.org/ns/activitystreams#CollectionPage
(json-ld:define-json-type (collection-page "CollectionPage") (collection)
  "https://www.w3.org/ns/activitystreams"
  ((part-of
    "partOf"
    :documentation "Identifies the Collection to which a CollectionPage objects items belong.")
   (next
    "next"
    :documentation "In a paged Collection, indicates the next page of items.")
   (prev
    "prev"
    :documentation "In a paged Collection, identifies the previous page of items."))
  (:documentation "Used to represent distinct subsets of items from a Collection."))


;; https://www.w3.org/ns/activitystreams#OrderedCollectionPage
(json-ld:define-json-type (ordered-collection-page "OrderedCollectionPage") (collection-page ordered-collection)
  "https://www.w3.org/ns/activitystreams"
  ((start-index
    "startIndex"
    :documentation "A non-negative integer value identifying the relative position within the logical view of a strictly ordered collection."))
  (:documentation "Used to represent ordered subsets of items from an OrderedCollection."))



;;; Extended Activity types
;;; ————————————————————————————————————————
(define-json-empty-types (activity) "https://www.w3.org/ns/activitystreams"
  (accept   "Accept"   "Indicates that the actor accepts the object. The target property can be used in certain circumstances to indicate the context into which the object has been accepted.")
  (add      "Add"    "Indicates that the actor has added the object to the target. If the target property is not explicitly specified, the target would need to be determined implicitly by context. The origin can be used to identify the context from which the object originated.")
  (create   "Create"   "Indicates that the actor has created the object.")
  (delete   "Delete"   "Indicates that the actor has deleted the object. If specified, the origin indicates the context from which the object was deleted.")
  (follow   "Follow"   "Indicates that the actor is “following” the object. Following is defined in the sense typically used within Social systems in which the actor is interested in any activity performed by or on the object. The target and origin typically have no defined meaning.")
  (ignore   "Ignore"   "Indicates that the actor is ignoring the object. The target and origin typically have no defined meaning.")
  (join     "Join"     "Indicates that the actor has joined the object. The target and origin typically have no defined meaning.")
  (leave    "Leave"    "Indicates that the actor has left the object. The target and origin typically have no meaning.")
  (like     "Like"     "Indicates that the actor likes, recommends or endorses the object. The target and origin typically have no defined meaning.")
  (offer    "Offer"    "Indicates that the actor is offering the object. If specified, the target indicates the entity to which the object is being offered.")
  (reject   "Reject"   "Indicates that the actor is rejecting the object. The target and origin typically have no defined meaning.")
  (remove   "Remove"   "Indicates that the actor is removing the object. If specified, the origin indicates the context from which the object is being removed.")
  (undo     "Undo"     "Indicates that the actor is undoing the object. In most cases, the object will be an Activity describing some previously performed action (for instance, a person may have previously “liked” an article but, for whatever reason, might choose to undo that like at some later point in time).
The target and origin typically have no defined meaning.")
  (update   "Update"   "Indicates that the actor has updated the object. Note, however, that this vocabulary does not define a mechanism for describing the actual set of modifications made to object.
The target and origin typically have no defined meaning.")
  (view     "View"     "Indicates that the actor has viewed the object.")
  (listen   "Listen"   "Indicates that the actor has listened to the object.")
  (read     "Read"     "Indicates that the actor has read the object.")
  (move     "Move"     "Indicates that the actor has moved object from origin to target. If the origin or target are not specified, either can be determined by context.")
  (travel   "Travel"   "Indicates that the actor is traveling to target from origin. Travel is an IntransitiveObject whose actor specifies the direct object. If the target or origin are not specified, either can be determined by context.")
  (announce "Announce" "Indicates that the actor is calling the target's attention the object.
The origin typically has no defined meaning.")
  (flag     "Flag"     "Indicates that the actor is “flagging” the object. Flagging is defined in the sense common to many social platforms as reporting content as being inappropriate for any number of reasons.")
  (dislike  "Dislike"  "Indicates that the actor dislikes the object."))


(json-ld:define-json-type (question "Question") (intransitive-activity)
  "https://www.w3.org/ns/activitystreams"
  ((one-of
    "oneOf"
    :documentation "Identifies an exclusive option for a Question. Use of oneOf implies that the Question can have only a single answer. To indicate that a Question can have multiple answers, use anyOf.")
   (any-of
    "anyOf"
    :documentation "Identifies an inclusive option for a Question. Use of anyOf implies that the Question can have multiple answers. To indicate that a Question can have only one answer, use oneOf.")
   (closed
    "closed"
    :documentation "Indicates that a question has been closed, and answers are no longer accepted."))
  (:documentation "Represents a question being asked. Question objects are an extension of IntransitiveActivity. That is, the Question object is an Activity, but the direct object is the question itself and therefore it would not contain an object property.
Either of the ANY-OF and ONE-OF properties MAY be used to express possible answers, but a Question object MUST NOT have both properties."))


(json-ld:define-json-type (arrive "Arrive") (intransitive-activity)
  "https://www.w3.org/ns/activitystreams"
  ((arrive
    "Arrive"
    :documentation "Indicates that the actor has arrived at the location. The origin can be used to identify the context from which the actor originated. The target typically has no defined meaning."))
  (:documentation "An IntransitiveActivity that indicates that the actor has arrived at the location. The origin can be used to identify the context from which the actor originated. The target typically has no defined meaning."))


(json-ld:define-json-type (block "Block") (ignore)
  "https://www.w3.org/ns/activitystreams"
  ()
  (:documentation "Indicates that the actor is blocking the object. Blocking is a stronger form of Ignore. The typical use is to support social systems that allow one user to block activities or content of other users. The target and origin typically have no defined meaning."))


(json-ld:define-json-type (invite "Invite") (offer)
  "https://www.w3.org/ns/activitystreams"
  ()
  (:documentation "A specialization of Offer in which the actor is extending an invitation for the object to the target."))


(json-ld:define-json-type (tentative-accept "TentativeAccept") (accept)
  "https://www.w3.org/ns/activitystreams"
  ()
  (:documentation "A specialization of Accept indicating that the acceptance is tentative."))


(json-ld:define-json-type (tentative-reject "TentativeReject") (reject)
  "https://www.w3.org/ns/activitystreams"
  ()
  (:documentation "A specialization of Reject indicating that the rejection is tentative."))



;;; Extended Actor types
;;; ————————————————————————————————————————
(define-json-empty-types (object actor) "https://www.w3.org/ns/activitystreams"
  (application  "Application"  "Describes a software application.")
  (group        "Group"        "Represents a formal or informal collective of Actors.")
  (organization "Organization" "Represents an organization.")
  (person       "Person"       "Represents an individual person.")
  (service      "Service"      "Represents a service of any kind."))



;;; Extended Object types
;;; ————————————————————————————————————————
(define-json-empty-types (object) "https://www.w3.org/ns/activitystreams"
  (article  "Article"  "Represents any kind of multi-paragraph written work.")
  (document "Document" "Represents a document of any kind.")
  (note     "Note"     "Represents a short written work typically less than a single paragraph in length.")
  (event    "Event"    "Represents any kind of event."))


(define-json-empty-types (document) "https://www.w3.org/ns/activitystreams"
  (audio    "Audio"    "Represents an audio document of any kind.")
  (image    "Image"    "An image document of any kind.")
  (video    "Video"    "Represents a video document of any kind.")
  (page     "Page"     "Represents a Web Page."))


;; https://www.w3.org/ns/activitystreams#Place
(json-ld:define-json-type (place "Place") (object)
  "https://www.w3.org/ns/activitystreams"
  ((accuracy
    "accuracy"
    :documentation "Indicates the accuracy of position coordinates on a Place objects. Expressed in properties of percentage. e.g. “94.0” means “94.0% accurate”.")
   (altitude
    "altitude"
    :documentation "Indicates the altitude of a place. The measurement units is indicated using the units property. If units is not specified, the default is assumed to be “m” indicating meters.")
   (latitude
    "latitude"
    :documentation "The latitude of a place.")
   (longitude
    "longitude"
    :documentation "The longitude of a place.")
   (radius
    "radius"
    :documentation "The radius from the given latitude and longitude for a Place. The units is expressed by the units property. If units is not specified, the default is assumed to be “m” indicating “meters”.")
   (units
    "units"
    :documentation "Specifies the measurement units for the radius and altitude properties on a Place object. If not specified, the default is assumed to be “m” for “meters”."))
  (:documentation "Represents a logical or physical location."))


;;  https://www.w3.org/ns/activitystreams#Profile
(json-ld:define-json-type (profile "Profile") (object)
  "https://www.w3.org/ns/activitystreams"
  ((describes
    "describes"
    :documentation "The describes property identifies the object described by a Profile."))
  (:documentation "A Profile is a content object that describes another Object, typically used to describe Actor Type objects. The describes property is used to reference the object being described by the profile."))


;; https://www.w3.org/ns/activitystreams#Relationship
(json-ld:define-json-type (relationship "Relationship") (object)
  "https://www.w3.org/ns/activitystreams"
  ((subject
    "subject"
    :documentation "The subject property identifies one of the connected individuals. For instance, for a Relationship object describing “John is related to Sally”, subject would refer to John.")
   (object
    "object"
    :documentation "Describes the entity to which the subject is related.")
   (relationship
    "relationship"
    :documentation "The relationship property identifies the kind of relationship that exists between subject and object."))
  (:documentation "Describes a relationship between two individuals. The subject and object properties are used to identify the connected individuals."))


;; https://www.w3.org/ns/activitystreams#Tombstone
(json-ld:define-json-type (tombstone "Tombstone") (object)
  "https://www.w3.org/ns/activitystreams"
  ((former-type
    "formerType"
    :documentation "The formerType property identifies the type of the object that was deleted.")
   (deleted
    "deleted"
    :documentation "The deleted property is a timestamp for when the object was deleted."))
  (:documentation "A Tombstone represents a content object that has been deleted. It can be used in Collections to signify that there used to be an object at this position, but it has been deleted."))


(json-ld:define-json-type (mention "Mention") (link) "https://www.w3.org/ns/activitystreams"
  ()
  (:documentation "A specialized Link that represents an @mention."))
