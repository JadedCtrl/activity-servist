;;;; litepub: Common ActivityPub JSON-types, borrowed from Pleroma

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

(defpackage #:activity-servist/vocab/litepub
  (:use #:cl)
  (:nicknames "AS/V/LP" "LITEPUB")
  (:export
   ;; Classes
   :activity :note :object :update
   :chat-message :emoji :emoji-react :hashtag :property-value
   ;; Slots/Accessors
   :also-known-as :atom-uri :capabilities :conversation :direct-message-p
   :discoverablep :former-representations :invisiblep :list-message
   :manually-approves-followers-p :non-anonymous :public-key :quote-uri
   :quote-url :sensitivep :value))

(in-package #:activity-servist/vocab/litepub)


;;; Globals
;;; ————————————————————————————————————————
(defvar *litepub-uri* "https://jam.xwx.moe/schemas/litepub-0.1.jsonld"
  "The “Litepub” flavour we use is nicked directly from Pleroma; there is not a
canonical URL for it. This URI will be used in encoded LITEPUB:OBJECTs in the
@CONTEXT.
Defaults to a copy at jam.xwx.moe — because why not? ¯\_(ツ)_/¯")

(unless +new-classes+
  (defconstant +new-classes+ '(chat-message emoji emoji-react hashtag property-value)
    "Simple list of classes/JSON types defined in this package.
Used by our overloaded JSON-LD:@CONTEXT to help choose the appropriate JSON-LD context."))

;; This isn’t pretty… but it helps us avoid bringing in another dependency! :^)
;; (closer-mop, that is.)
(unless +new-slots+
  (defconstant +new-slots+
    '(atom-uri sensitivep non-anonymous direct-message-p former-representations
      public-key discoverablep manually-approves-followers-p also-known-as
      capabilities            ; Object ←↑
      invisiblep list-message ; Activity
      conversation            ; Update
      quote-url quote-uri)    ; Note
    "List of slots added to subclasses corresponding AS/VOCAB/ACTIVITY classes.
For example, slots in our OBJECT that aren’t in AS/V/A:OBJECT.
Used by our overloaded JSON-LD:@CONTEXT to help choose the appropriate JSON-LD context."))


;;; Core types
;;; ————————————————————————————————————————
(json-ld:define-json-type (object "Object") (as/v/a:object) *litepub-uri*
  ((atom-uri
    "atomUri"
    :documentation "A string containing a URI to an Atom-feed alternative representation of an object.
Potentially deprecated/very uncommon.")
   ;; https://docs.joinmastodon.org/spec/activitypub/#sensitive
   (sensitivep
    "sensitive"
    :documentation "A boolean value, representing whether or not an Object’s content is not necessarily generally appropriate. This will often hide the content, to some clients. SUMMARY will often be displayed in place of the content.")
   (non-anonymous
    "nonAnonymous"
    :documentation "I had no luck finding what this might mean, to be honest. ¯\_(ツ)_/¯
Likely deprecated/highly uncommon.")
   (direct-message-p
    "directMessage"
    :documentation "A boolean value commonly used to mark a Note as non-public, a direct message to be visible only to those in TO.
Seemingly may be set in the Activity modifying the Note, or the Note itself.")
   (former-representations
    "formerRepresentations")

   ;; Actor properties
   (public-key
    "publicKey"
    :documentation "Contains an object representing a definition of the Actor’s public key, used for HTTP signatures.
Generally contains the properties “id”, “owner”, “publicKeyPem”.")
   ;; https://docs.joinmastodon.org/spec/activitypub/#discoverable
   (discoverablep
    "discoverable"
    :documentation "A boolean value reflecting whether or not an Actor’s profile should be publically discoverable.")
   ;; https://docs.joinmastodon.org/spec/activitypub/#as
   (manually-approves-followers-p
    "manuallyApprovesFollowers"
    :documentation "A boolean value, communicating whether or not an Actor screens follow-requests.")
   ;; https://docs.joinmastodon.org/spec/activitypub/#Move
   (also-known-as
    "alsoKnownAs"
    :documentation "When moving between two accounts, the old account sets this property to the URI of the new account.")
   (capabilities
    "capabilities"
    :documentation "Contains a hash-table of capability-names mapped to a boolean, marking this Actor’s (server’s) support of capability.
One known capabilitity-name is Pleroma’s “acceptsChatMessages”.")))


(defmethod json-ld:@context ((obj litepub:object))
  (let ((lp-context (list "https://www.w3.org/ns/activitystreams" *litepub-uri*)))
    ;; Only use LitePub context for newly-defined classes.
    (if (member obj +new-classes+ :test #'typep)
        lp-context
        ;; … or for derivative classes with new slots.
        (or (dolist (slot +new-slots+)
              (when (and (slot-exists-p obj slot)
                         (slot-boundp obj slot))
                (return lp-context)))
            (call-next-method)))))


(json-ld:define-json-type (activity "Activity") (as/v/a:activity object) *litepub-uri*
  (;; https://blog.dereferenced.org/leveraging-json-ld-compound-typing-for-behavioural-hinting-in-activitypub
   (invisiblep
    "invisible"
    :documentation "A boolean value hinting as to whether or not the result of an Activity should be invisible to the end-user.
Potentially deprecated/very uncommon.")
   (list-message
    "list-message"))
  (:documentation "An Activity is a subtype of Object that describes some form of action that may happen, is currently happening, or has already happened. The Activity type itself serves as an abstract base type for all types of activities. It is important to note that the Activity type itself does not carry any specific semantics about the kind of action being taken."))


;; https://schema.org/PropertyValue
(json-ld:define-json-type (property-value "PropertyValue")
    (litepub:object json-ld:object) *litepub-uri*
  (;; https://schema.org/value
   (value
    "value"
    :documentation "The value of a property value node."))
  (:documentation
   "Commonly used to store custom metadata on a Person, stored in its ATTACHMENT slot."))



;;; Extended Activity types
;;; ————————————————————————————————————————
(json-ld:define-json-type (update "Update") (as/v/a:update activity) *litepub-uri*
  (;; https://ostatus.github.io/spec/OStatus%201.0%20Draft%202.html#rfc.section.6
   (conversation
    "conversation"
    :documentation "When an update is part of a distributed conversation, this is the URI of that conversation.
Likely deprecated/highly uncommon."))
  (:documentation "Indicates that the actor has updated the object. Note, however, that this vocabulary does not define a mechanism for describing the actual set of modifications made to object.
The target and origin typically have no defined meaning."))


;; https://codeberg.org/fediverse/fep/src/branch/main/fep/c0e0/fep-c0e0.md
(json-ld:define-json-type (emoji-react "EmojiReact") (as/v/a:like activity) *litepub-uri*
  ()
  (:documentation "This activity is similar to Like activity. In addition to standard properties of Like activity, EmojiReact activity MUST have a content property. Reaction content MUST be either a single unicode grapheme, or a shortcode of a custom emoji. If custom emoji is used, EmojiReact activity MUST have a tag property containing a single Emoji object."))



;;; Extended Object types
;;; ————————————————————————————————————————
;; https://docs.joinmastodon.org/spec/activitypub/#Emoji
(json-ld:define-json-type (emoji "Emoji") (object) *litepub-uri*
  ()
  (:documentation "Represents a custom-emoji, with a shortcode (NAME), ID, and ICON (containing MEDIA-TYPE and URL)."))


(json-ld:define-json-type (note "Note") (as/v/a:note object) *litepub-uri*
  (;; https://misskey-hub.net/ns#_misskey_quote
   (quote-url
    "quoteUrl"
    :accessor nil
    :documentation "Signifies that this Note is “quoting” another Note. Its value is another Note’s ID.
Effectively equivalent to QUOTE-URI.
One of QUOTE-URL (as:quoteUrl) or QUOTE-URI (fedibird:quoteUri) is to be deprecated, and the other ought be preferred.
It is, however, unclear which one will win out in the end. The implementer prefers QUOTE-URL.")
   ;; https://misskey-hub.net/ns#_misskey_quote
   (quote-uri
    "quoteUri"
    :accessor nil
    :documentation "Signifies that this Note is “quoting” another Note. Its value is another Note’s ID.
Effectively equivalent to QUOTE-URL.
One of QUOTE-URL (as:quoteUrl) or QUOTE-URI (fedibird:quoteUri) is to be deprecated, and the other ought be preferred.
It is, however, unclear which one will win out in the end. The implementer prefers QUOTE-URL."))
  (:documentation "Represents a short written work typically less than a single paragraph in length."))


;; https://docs-develop.pleroma.social/backend/development/ap_extensions/#chatmessages
(json-ld:define-json-type (chat-message "ChatMessage") (note) *litepub-uri*
  ()
  (:documentation "Represents a private and one-on-one chat-message.
Similar to Notes in creation and use, but TO may contain only one recipient.
Potentially very uncommon — it is used by at least Pleroma."))


(defgeneric note-quote-url (obj)
  (:documentation "Accessor for a NOTE’s “quote-URL”, as used for quote-posts in Pleroma, Misskey, etc.
Will set/get the value of either QUOTE-URL or QUOTE-URI, depending on which is currently in use.
In case of doubt, QUOTE-URL is preferred."))

(defmethod note-quote-url ((obj note))
  (or (and (slot-boundp note 'quote-url) (slot-value note 'quote-url))
      (and (slot-boundp note 'quote-uri) (slot-value note 'quote-uri))))

(defgeneric (setf note-quote-url) (obj value))
(defmethod (setf note-quote-url) ((obj note) value)
  (if (slot-boundp note 'quote-uri)
      (setf (slot-value note 'quote-uri) value)
      (setf (slot-value note 'quote-url) value)))



;;; Extended Link types
;;; ————————————————————————————————————————
;; https://docs.joinmastodon.org/spec/activitypub/#Hashtag
(json-ld:define-json-type (hashtag "Hashtag") (as/v/a:link object) *litepub-uri*
  ()
  (:documentation "Similar to Mentions, a Hashtag is used to link a post to given topics. Should be stored in a TAG slot, and contain NAME (#hashtag) and HREF (link to a server’s hashtag listing)."))
