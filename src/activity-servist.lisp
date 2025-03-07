;;;; activity-servist: An ActivityPub server framework.

;; Copyright © 2023-2025 Jaidyn Levesque <jadedctrl@posteo.at>
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

(defpackage #:activity-servist
  (:use #:cl)
  (:nicknames "AS" "ACTIVITYPUB")
  (:export
   ;; Functions
   :server :start-server
   :send
   ;; Methods
   :receive :store
   ;; Globals
   *config* *debug* *logs*))

(in-package #:activity-servist)


;;; Globals
;;; ————————————————————————————————————————
[defvar *config* '(:host "http://localhost:8080" :address "127.0.0.1" :port 8080
                   :inbox-path "inbox")
  "Configuration for the server, a property-list.
There are three optional properties:
• :HOST, the public-facing URI of the server.
• :ADDRESS, the address the server is exposed on.
• :PORT, the port the server is exposed on.
• :INBOX-PATH, the URI for your server’s shared/private inboxes.

:INBOX-PATH is relative to :HOST; by default, it is “inbox”, which corresponds to
“http://localhost:8080/inbox”. The recipient(s) of any objects sent to the
inbox should be deduced by the objects’ contents.

There is one required property:
• :RETRIEVE, a function used as a callback by activity-servist.

:RETRIEVE should be a function of (RETRIEVE URI)
This function should simply return an object from your storage, queried by a URI.
The URI parameter is going to be either an @ID or an account-URI of the form
“acct:username@hostname”."]

(defvar       *logs*  nil "A list of incoming Clack HTTP requests, used for debugging.")
(defparameter *debug* nil "Whether or not debugging-mode is on. More verbose errors, detailed logging, etc.")

(defparameter *private-key* nil
  "The private RSA key used for signing outgoing HTTP requests, as a PEM string.")

(defun directories ()
  "Alist of the server's paths and their response functions."
  `((".well-known/webfinger" . http-webfinger)
    (".well-known/host-meta" . http-host-meta)
    ("inbox"                 . http-inbox)
    (""                      . http-object)))  ; By default, assume object.



;;; Callbacks
;;; ————————————————————————————————————————
(defun retrieve (uri)
  "Runs the user-defined callback RETRIEVE, as stored in *CONFIG*.
Returns the object associated with the given URI from our object-store."
  (let {[func (getf *config* :retrieve)]}
    (if func
        (funcall func uri)
        (error "No RETRIEVE function found in ACTIVITY-SERVIST:*CONFIG*."))))

(defgeneric receive (obj)
  (:documentation
   "Called when an OBJECT is “sent” to activity-servist’s HTTP inbox.
This is done by other servers, and is how activities and objects get federated
with ActivityPub.

To receive objects, you should overload this generic with (at the bare minimum)
a method accepting JSON-LD:OBJECTs. Doing so is required — not defining this
method will cause an error when an object is sent to the inbox.

By default, there is a :BEFORE-method defined, which fetches received Activity’s
Actors, and then calls RECEIVE on them in turn."))

;; We want to make sure that activity’s actors are being retrieved and stored,
;; so that we can validate HTTP signatures (when that gets implemented).
(defmethod receive :before ((obj activity-vocabulary:activity))
  (let* {[actor-uri (ignore-errors (activity-vocabulary:actor obj))]}
    (when actor-uri
      (or (retrieve actor-uri)
          (fetch-and-store actor-uri)))))

(defgeneric store (obj)
  (:documentation
   "Stores an object in a object-store accessible with the callback function
RETRIEVE, likely for caching purposes.

You should overload this generic with a method accepting JSON-LD:OBJECTs, as it
is necessary for activity-servist to function. When called by activity-servist,
this is solely used to store fetched foreign objects."))

;;(defun send (obj ))



;;; Fetching foreign objects
;;; ————————————————————————————————————————
;; “I will stab you in the eye, with a foreign object.”
;;    — The Mountain Goats, “Foreign Object” (2015)

(defun fetch (obj-uri)
  "Fetch & parse an ActivityPub object from a foreign server; returning the object.
Will throw a FETCH-ERROR if the HTTP request fails."
  (handler-case
      (let* {[json-ld-mimetypes
              "application/activity+json,application/ld+json"]
             [response-body
              (dexador:get obj-uri :headers `(("Accept" . ,json-ld-mimetypes)))]}
        (json-ld:parse response-body))
    (dexador.error:http-request-failed (err)
      (error 'fetch-error :status (dexador.error:response-status err)
                          :body   (dexador.error:response-body err)))))

(defun fetch-and-store (obj-uri)
  "Fetch & parses an ActivityPub object from a foreign server; then try to pass it
along to our server for caching.
If it STOREs sans an error (de-facto rejecting the object), return the parsed object.
Otherwise, nil."
  (let {[obj (fetch obj-uri)]}
    (when (and obj (ignore-errors (store obj)))
      obj)))

(defun retrieve-or-fetch (obj-uri)
  "Attempt to RETRIEVE an ActivityPub object of the given OBJ-URI ID.
If not retrieved from the object store, then FETCH-AND-STORE it there.
Returns the object if it was retrieved or fetched; nil otherwise."
  (or (retrieve obj-uri)
      (fetch-and-store obj-uri)))

(define-condition fetch-error (error)
  ((status  :initarg :status)
   (body    :initarg :body))
  (:documentation "Thrown when we fail to fetch a resource, and get a non-2XX HTTP status code."))


;;; Signature HTTP-header parsing
;;; ————————————————————————————————————————
(defun signature-valid-p (env activity &key [current-time (get-universal-time)])
  "Return whether or not the Clack HTTP-request ENV’s signature is valid.
Only RSA-SHA256 signatures are supported.
Might provide a condition detailing the reason of the signature’s invalidity as
a second return-value.

Follows (mostly) the specification of:
https://swicg.github.io/activitypub-http-signature/"
  (handler-case
      (let* {[headers          (getf env :headers)]
             [signature-header (gethash "signature" headers)]
             [signature-alist  (if signature-header
                                   (signature-header-parse signature-header)
                                   (signal 'no-signature-header))]
             [algorithm        (assoc :algorithm signature-alist)]
             [signed-str       (signed-string env signature-alist :current-time current-time)]}
        (when (and algorithm (not (string-equal (cdr algorithm) "rsa-sha256")))
          (signal 'invalid-signature-algorithm :algorithm (cdr algorithm)))
        (when (not (matching-domains-p signature-alist activity))
          (signal 'invalid-signature-domain-mismatch))
        (as/s:signature-valid-p
         (gethash "https://w3id.org/security#publicKeyPem" (signature-key signature-alist))
         signed-str
         (cdr (assoc :signature signature-alist))))
    ;; The special case of an actor being deleted, and so their key inaccessible.
    ;; https://swicg.github.io/activitypub-http-signature/#handling-deletes-of-actors
    (fetch-error (err)
      (or (deleting-object-p)
          (signal 'invalid-signature-failed-fetch)))
    ;; The very normal case of the signature simply being invalid! :P
    (invalid-signature (err)
      (values nil err))))

(defun deleting-object-p (activity)
  "Whether or not ACTIVITY is a DELETE and the OBJECT of the ACTIVITY — purportedly,
what would be deleted — is actually deleted. Fetching the object must, in which
case, return a 404 or a 410 HTTP error."
  (when (typep activity 'as/v/a:delete)
    (handler-case
        (progn (dexador:get (as/v/a:object activity))
               nil)
      (dexador.error:http-request-failed (err)
        (let {[status (dexador.error:response-status err)]}
          (member status '(404 410)))))))

(defun matching-domains-p (signature-alist activity)
  "Returns whether or not the domain names within an ACTIVITY match, for ensuring
its signature is applicable: Those of the signature key’s, the actor’s @ID,
and the ACTIVITY’s @ID.
If these all match the same domain, and the signature is valid, we can safely say
the ACTIVITY did indeed come from that domain."
  (labels ((uri-string (slot-value)
             (typecase slot-value
               (string slot-value)
               (json-ld:object
                (json-ld:@id slot-value))))
           (domain-name (slot-value)
             (let {[uri-string (uri-string slot-value)]}
               (when uri-string
                 (quri:uri-domain (quri:uri uri-string))))))
    (equal*
      (remove-if #'not
       (list (domain-name activity)
             (domain-name (as/v/a:actor activity))
             (domain-name (assoc :keyid signature-alist)))))))

(defun signature-header-parse (signature-header)
  "Parses the signature header into an associative list of the form:
  '((:KEYID     . “https://jam.xwx.moe/users/jadedctrl#main-key”)
    (:ALGORITHM . “rsa-sha256”)
    (:HEADERS   . “(request-target) content-length date digest host”)
    (:SIGNATURE . “⋯”))"
  (mapcar (lambda (key=value)
            (destructuring-bind (key value)
                (str:split
                 #\≝ (str:replace-first "=" "≝" key=value)) ; Since a value might contain “=”
              (cons (intern (string-upcase key) "KEYWORD")
                    (string-trim '(#\") value))))
          (str:split #\, signature-header)))

(defun signed-string (env signature-alist &key [current-time (get-universal-time)])
  "Generate the string that was signed for the signature-header of the Clack HTTP request ENV.
Will error our if the request’s Digest or Date headers don’t match our calculated values."
  (let* {[headers      (getf env :headers)]
         [header-names (signed-header-names signature-alist)]}
    (reduce
     (lambda (a b) (format nil "~A~%~A" a b))
     (mapcar
      (lambda (header-name)
        (let {[header-value (gethash header-name headers)]}
          (str:string-case (string-downcase header-name)
            ;; (request-target) is a pseudo-header formatted like “post /inbox”.
            ("(request-target)"
             (format nil "~A: ~A ~A"
                     header-name
                     (string-downcase (symbol-name (getf env :request-method)))
                     (getf env :path-info)))
            ;; Calculate digest ourselves; never can’t trust the enemy!
            ("digest"
             (let {[our-digest
                     (format nil "SHA-256=~A" (as/s:string-sha256sum (body-contents env)))]}
               (if (equal our-digest header-value)
                   (format nil "~A: ~A" header-name our-digest)
                   (signal 'invalid-signature-digest :digest header-value :our-digest our-digest))))
            ;; They might be resending reqs, so ensure our clocks’re close enough.
            ;; I reckon two hours is a good-enough margin of error.
            ;; Or maybe I’m too lenient? ;P
            ("date"
             (let {[their-time (cl-date-time-parser:parse-date-time header-value)]}
               (if (< (abs (- current-time their-time))
                      7200) ; Two hours in seconds
                   (format nil "~A: ~A" header-name header-value)
                   (signal 'invalid-signature-date :date their-time :our-date current-time))))
            ;; … we can trust them on everything else, tho.
            (otherwise
             (format nil "~A: ~A" header-name header-value)))))
      header-names))))

(defun signed-header-names (signature-alist)
  "Return a list of the names of headers used in a SIGNATURE-ALIST’s signed string."
  (str:split #\space (cdr (assoc :headers signature-alist)) :omit-nulls 't))

(define-condition http-result (condition)
  ((status  :initarg :status  :initform 500 :reader http-status)
   (message :initarg :message :initform nil :reader http-message))
  (:documentation "A condition that can be returned as an HTTP result."))

(define-condition invalid-signature (http-result)
  ()
  (:default-initargs :status 401)
  (:documentation "Thrown when validation of an HTTP signature fails."))

(define-condition no-signature-header (invalid-signature)
  ()
  (:report (lambda (condition stream) (declare (ignore condition))
             (format stream "No signature header was provided! 🐄~%Take a look at:
https://swicg.github.io/activitypub-http-signature/#how-to-obtain-a-signature-s-public-key~&")))
  (:documentation
   "Thrown during HTTP signature-validation, when no signature header was provided at all."))

(define-condition invalid-signature-date (invalid-signature)
  ((date     :initarg :date     :initform nil)
   (our-date :initarg :our-date :initform nil))
  (:report (lambda (condition stream)
             (format stream "The given date “~A” is too far off from our own “~A”.~&"
                     (slot-value condition 'date) (slot-value condition 'our-date))))
  (:documentation
   "Thrown during HTTP signature-validation, when the given Date header is too far in the past/future."))

(define-condition invalid-signature-digest (invalid-signature)
  ((digest     :initarg :digest     :initform nil)
   (our-digest :initarg :our-digest :initform nil))
  (:report (lambda (condition stream)
             (format stream "The digest header “~A” doesn’t match our calculated “~A”.~&"
                     (slot-value condition 'digest) (slot-value condition 'our-digest))))
  (:documentation
   "Thrown during HTTP signature-validation, when the SHA256 digest header doesn’t match our calculated value."))

(define-condition invalid-signature-algorithm (invalid-signature)
  ((algorithm :initarg :algorithm :initform nil))
  (:report (lambda (condition stream)
             (format stream "The signature algorithm “~A” is invalid; we only support rsa-sha256.~&"
                     (slot-value condition 'algorithm))))
  (:documentation "Thrown during HTTP signature-validation, when the algorithm is unsupported."))

(define-condition invalid-signature-domain-mismatch (invalid-signature)
  ()
  (:report (lambda (condition stream) (declare (ignore condition))
             (format stream "There is a domain-name mismatch within the activity, and so we can’t say for sure the signature is valid.~%
Check the ID domain-names of the actor, the activity, and the signature-key.~&")))
  (:documentation "Thrown during HTTP signature-validation, when it's noticed that domains-names for ID URIs don't match."))

(define-condition invalid-signature-failed-fetch (invalid-signature)
  ()
  (:report (lambda (condition stream) (declare (ignore condition))
             (format stream "We were unable to fetch the signature’s public key. Either the server is napping, or the given key’s address is a lie. We are agnostic.")))
  (:documentation "Thrown during HTTP signature-validation, when the signature’s public key couldn’t be downloaded; and we have no cached version of it, either. And so, it is currently impossible to validate/invalidate the signature. We are agnostic."))



;;; Fetching public keys
;;; ————————————————————————————————————————
;; https://swicg.github.io/activitypub-http-signature/#how-to-obtain-a-signature-s-public-key
(defun signature-key (signature-alist)
  "Return a public key corresponding to the given an HTTP signature’s
SIGNATURE-ALIST (of SIGNATURE-HEADER-PARSE’s format).

Public keys are hash-tables, which should look more-or-less like so:
  @id = https://jam.xwx.moe/users/jadedctrl#main-key
  https://w3id.org/security#owner = https://jam.xwx.moe/users/jadedctrl
  https://w3id.org/security#publicKeyPem = -----BEGIN PUBLIC KEY-----[⋯]"
  (actor-key-of-id (signature-key-owner signature-alist)
                   (cdr (assoc :keyid signature-alist))))

(defun signature-key-owner (signature-alist)
  "Return a the owning actor (likely as a LITEPUB:PERSON) of the public key
corresponding to the given SIGNATURE-ALIST (of SIGNATURE-HEADER-PARSE's format)."
  (let* {[key-uri         (cdr (assoc :keyid signature-alist))]
         [maybe-owner-uri (car (str:split #\# key-uri))]
         ;; A common URI for keys is /users/user#keyname; so we try to save an HTTP request by
         ;; checking the object store for /users/user as an ID.
         [data            (or (retrieve          maybe-owner-uri)
                              (retrieve-or-fetch key-uri))]}
    (typecase data
      (hash-table
       (retrieve-or-fetch (gethash "https://w3id.org/security#owner" data)))
      (activity-vocabulary:person
       data))))

(defun actor-key-of-id (actor id)
  "Search through an ActivityPub ACTOR’s public keys, returning the one
whose @id matches ID.
The public key will be a hash-table; see SIGNATURE-KEY’s docstring for info."
  (let* {[key-or-keys (ignore-errors (litepub:public-key actor))]
         [keys        (if (listp key-or-keys) key-or-keys (list key-or-keys))]}
    (find id keys :test (lambda (key-id key)
                          (equal (gethash "@id" key) key-id)))))



;;; Host-info response
;;; ————————————————————————————————————————
(defun http-host-meta (&optional env path-items params)
  `(200 (:content-type "application/xrd+xml; charset=utf-8")
    (,(str:concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<XRD xmlns=\"http://docs.oasis-open.org/ns/xri/xrd-1.0\">
<link rel=\"lrdd\" type=\"application/xrd+xml\" template=\""
                  (getf *config* :host)
                  "/.well-known/webfinger?resource={uri}\"/>
</XRD>
"))))



;;; Webfinger response
;;; ————————————————————————————————————————
(defun http-webfinger (env path-items params)
  (webtentacle:server env (lambda (resource) (webfinger-resource-info resource))))

(defun webfinger-resource-info (resource)
  "Given a Webfinger RESOURCE, return a property-list of data on the given resource.
Will "
  (let {[obj (retrieve resource)]}
    (and obj (webfinger-info resource obj))))

(defgeneric webfinger-info (resource obj)
  (:documentation "Returns a property-list of Webfinger data on the given object.
Override this to provide custom Webfinger data; do what you please, but make sure
RESOURCE is set as the :SUBJECT value.

For information on the property-list’s format, see the dosctring of WEBTENTACLE:SERVER."))

;; A default implementation, which provides (likely) all of the information
;; necessary for most use-cases.
(defmethod webfinger-info (resource (obj json-ld:object))
  (let {[obj-uri (json-ld:@id obj)]}
    (list
     :subject resource
     :aliases (list obj-uri)
     :links
     ;; Note: Doesn’t provide the os_status subscribe link.
     `((href ,obj-uri
             rel "self"
             type "application/activity+json")
       (href ,obj-uri
             rel "self"
             type "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")))))



;;; Object requests
;;; ————————————————————————————————————————
(defun http-object (env path-items params)
  "If an ActivityPub object is requested, serve it (if such an object
can be found). Uses the callback :RETRIEVE, defined in *CONFIG*."
  (let* {[uri (reduce (lambda (a b) (format nil "~A/~A" a b))
                     (append (list (getf *config* :host)) path-items))]
         [obj (retrieve uri)]}
    (if obj
        (list 200 '(:content-type "application/activity+json")
              (list (yason:with-output-to-string* () (yason:encode-object obj))))
        `(404 (:content-type "text/plain")
          ("Nobody here but us chickens! 🐓")))))



;;; Inbox requests
;;; ————————————————————————————————————————
(defun http-inbox (env path-items params)
  "If one tries to send an activity to our inbox, pass it along to
the overloaded RECEIVE method."
  (let* {[contents      (body-contents env)]
         [json-contents (json-ld:parse contents)]}
    (multiple-value-bind (signature-valid-p signature-error)
        (signature-valid-p env json-contents)
      (cond (signature-error (signal signature-error))
            ((not signature-valid-p)
             (signal 'http-result :status 401 :message "Failed to verify signature. Heck! TvT"))
            ((receive json-contents)
             '(200 (:content-type "text/plain") ("You win!")))))))
;;


;;; Sending a note
;;; ————————————————————————————————————————
(defun send (from to object &key (private-key *private-key*))
  "Sends an OBJECT to a user in the fediverse.

FROM is either a PERSON object or a user-id (URL) string.
TO is either a PERSON object or the destination’s inbox URI.
PRIVATE-KEY is an RSA private key as a PEM string."
  (let* {[inbox-uri (or (and (stringp to) to)
                        (as/v/a:inbox to))]
         [sender-id (or (and (stringp from) from)
                        (json-ld:@id from))]
         [json      (yason:with-output-to-string* () (yason:encode-object object))]
         [headers   (make-send-headers inbox-uri sender-id private-key json)]}
    (dexador:post inbox-uri :content json :headers headers)))

(defvar +date-header-datetime-format+
  '(:short-weekday ", " (:day 2) " " :short-month " " (:year 4) " "
    (:hour 2) #\: (:min 2) #\: (:sec 2) " " :timezone))


(defun make-send-headers (inbox-uri sender-id private-pem content)
  "Returns an association list of headers to be used in the POST request to INBOX-URI,
sending CONTENT from fediverse user SENDER-ID.
Headers are signed with the RSA key PRIVATE-PEM."
  (let* ((inbox-quri (quri:uri inbox-uri))
         (digest-header (str:concat "SHA-256=" (as/s:string-sha256sum content)))
         (date-header
           (let ((local-time:*default-timezone* local-time:+gmt-zone+))
             (local-time:format-timestring
              nil (local-time:now)
              :format +date-header-datetime-format+)))
         (signed-headers
           (concatenate
            'string
            (format nil "(request-target): post ~A~%" (quri:uri-path inbox-quri))
            (format nil "host: ~A~%" (quri:uri-host inbox-quri))
            (format nil "date: ~A~%" date-header)
            (format nil "digest: ~A" digest-header)))
         (signature        (as/s:sign-string private-pem signed-headers))
         (signature-header (str:concat "keyId=\"" sender-id "#main-key\","
                                       "algorithm=\"rsa-sha256\","
                                       "headers=\"(request-target) host date digest\","
                                       "signature=\"" signature "\"")))
    `(("Date" . ,date-header)
      ("Digest" . ,digest-header)
      ("Signature" . ,signature-header)
      ("Host" . ,(quri:uri-host inbox-quri))
      ("Content-Length" . ,(length content))
      ("Accept" . "application/activity+json")
      ("Content-Type" . "application/activity+json"))))



;;; Misc. responses
;;; ————————————————————————————————————————
(defun http-404 (env path-items params)
  "The default 404 response."
  '(404 (:content-type "text/plain")
    ("Nobody here but us chickens! 🐓")))



;;; Invocation
;;; ————————————————————————————————————————
(defmacro handle-server-errors (&body body)
  `(handler-case
       ,@body
     ;; For our pretty user-facing errors, return the status and message.
     (http-result (err)
       (logs-push err)
       (list (slot-value  err 'status) '(:content-type "text/plain")
             (list (or (slot-value err 'message)
                       (princ-to-string err)))))
    ;; For non-pretty errors, give a cryptic message (unless in *debug*-mode).
    (condition (err)
      (logs-push err)
      (list 500 '(:content-type "text/plain")
            (list (or (and *debug* (princ-to-string err))
                      "I am ERROR. 🥴"))))))

(defun server (env)
  "Returns the response data for Clack, given the request property-list ENV."
  (logs-push env)
  (let* {[path   (pathname-sans-parameters (getf env :request-uri))]
         [params (pathname-parameters      (getf env :request-uri))]
         [response-function
           (or (assoc-by-path (directories) (pathname-components path))
               '("" . http-404))]
         ;; So that response functions only deal with relative paths…
         [path-sans-response-root
           (pathname-components
            (str:replace-first (car response-function) "" path))]}
    (or (funcall (cdr response-function) env path-sans-response-root params)
        (funcall 'http-404 env path-sans-response-root params))))

(defun start-server ()
  "Start the server."
  (clack:clackup (lambda (env)
                   (if *debug*
                       (server env)
                       (handle-server-errors (server env))))
                 :server 'woo
                 :address "0.0.0.0"
                 :port (getf *config* :port)))



;;; Utils.
;;; ————————————————————————————————————————
(defun body-contents (env)
  "Given a Clack HTTP request property-list ENV, return the request’s body
contents as a string. They are read from :RAW-BODY in this plist.

If the contents are a stream, the stream’s contents will be read into a
string and the stream’s object in ENV will be replaced with the string."
  (let {[body (getf env :raw-body)]}
    (if (stringp body)
        body
        (setf (getf env :raw-body)
              (babel:octets-to-string
               (alexandria:read-stream-content-into-byte-vector body))))))

(defun logs-push (item)
  "Prepends ITEM to *LOGS*, if we are in *DEBUG* mode."
  (when *debug*
    (setq *logs* (append (list item) *logs*))))

(defun assoc-by-path (alist path-items &optional (depth 0))
  "Given an associative list and a path decomposed into a list of
its components, return the item with the closest according
pathname as key. If the exact path isn't a valid key, it will
try all parent directories.
E.g., “/bear/apple/momma/” could match either “/bear/apple/momma”
or “/bear/apple/” or “/bear/”, but not “/bear” (not a directory)."
  (let {[path (str:join #\/ path-items)]}
    (if (eq path-items nil)
        (assoc "" alist :test 'string=)
        (or (and (eq depth 0)
                 (assoc path alist :test 'string=))
            (assoc (str:concat path "/")
                   alist :test 'string=)
          (assoc-by-path
           alist (reverse
                  (cdr (reverse path-items)))
           (+ depth 1))))))

(defun pathname-sans-parameters (path)
  "Removes parameters from a URI pathname, returning the bare path.
“/path/a/b?a=1&b=3” → “/path/a/b”"
  (car (str:split #\? path)))

(defun pathname-parameters (path)
  "Convert the parameters of a URI pathname into an associative list.
“/path/a/b?a=1&b=2&c=3” → ((“a” . “1”) (“b” . “2”) (“c” . “3”))"
  (mapcar
   (lambda (pair)
     (let {[pair-items (str:split #\= pair)]}
       (cons (car pair-items)
             (cadr pair-items))))
   (str:split #\&  (cadr (str:split #\? path)))))

(defun pathname-components (pathname)
  "Split a pathname into a list of its components.
“/u/bear/apple.txt” → '(“u” “bear” “apple.txt”)"
  (str:split #\/ pathname :omit-nulls 't))

(defun equal* (&rest items)
  "Whether or not all ITEMS are EQUAL to one another."
  (loop for item in items
        always (equal item (car items))))
