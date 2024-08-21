;;;; activity-streams: Serialize/deserialize ActivityStreams objects.

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

(defpackage #:activity-servist/activity-streams
  (:use #:cl)
  (:nicknames "AS/AS" "ACTIVITY-STREAMS")
  (:export
   ;; Classes
   :object
   ;; Accessors
   #'object-actor #'object-attachment #'object-attributed-to #'object-audience
   #'object-content #'object-context #'object-name #'object-end-time
   #'object-generator #'object-icon #'object-image #'object-in-reply-to
   #'object-location #'object-preview #'object-published #'object-replies
   #'object-start-time #'object-summary #'object-tag #'object-updated
   #'object-url #'object-to #'object-bto #'object-cc #'object-bcc
   #'object-media-type #'object-duration
   ;; Slots
   :attachment :attributed-to :audience :content :context :name :end-time
   :generator :icon :image :in-reply-to :location :preview :published :replies 
   :start-time :summary :tag :updated :url :to :bto :cc :bcc :media-type :duration))

(in-package #:activity-servist/activity-streams)


;;; Core class
;;; ————————————————————————————————————————
(json-ld::define-json-type (object "Object") () "https://www.w3.org/ns/activitystreams"
  ((actor
    "actor"
    :documentation "Describes one or more entities that either performed or are expected to perform the activity. Any single activity can have multiple actors. The actor MAY be specified using an indirect Link.")
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
    :documentation "When used on a Link, identifies the MIME media type of the referenced resource.
When used on an Object, identifies the MIME media type of the value of the content property. If not specified, the content property is assumed to contain text/html content.")
   (duration
    "duration"
    :documentation "When the object describes a time-bound resource, such as an audio or video, a meeting, etc, the duration property indicates the object's approximate duration. The value MUST be expressed as an xsd:duration as defined by [ xmlschema11-2], section 3.3.6 (e.g. a period of 5 seconds is represented as “PT5S”)."))
  (:documentation
   "Describes an object of any kind. The Object type serves as the base type for
most of the other kinds of objects defined in the Activity Vocabulary,
including other Core types such as Activity, IntransitiveActivity, Collection
and OrderedCollection."))
