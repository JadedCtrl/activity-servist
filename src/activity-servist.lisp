;;;; activity-servist: An ActivityPub server framework.

;; Copyright © 2023-2024 Jaidyn Levesque <jadedctrl@posteo.at>
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
  (:use #:cl #:activity-servist/signatures)
  (:nicknames "AS" "ACTIVITYPUB")
  (:export
   ;; Functions
   :server :start-server
   ;; Globals
   *config*))

(in-package #:activity-servist)


;;; Globals
;;; ————————————————————————————————————————
(defvar *config* '(:address "localhost" :port 8080 :protocol "https")
  "Configuration for the server, a property-list.
There are three optional properties:
• :PROTOCOL, either “https” or “http” (the latter for testing, only!).
• :ADDRESS, the server’s domain-name/address.
• :PORT, the server’s port.

There is one required property:
• :FETCH, a function used as a callback by activity-servist.

:FETCH should be a function of (FETCH URI)
This function should simply return an object from your storage, queried by a URI.
The URI parameter is going to be either an @ID or an account-URI of the form “acct:username@hostname”.")

(defun directories ()
  "Alist of the server's paths and their response functions."
  '((".well-known/webfinger" . http-webfinger)
    (".well-known/host-meta" . http-host-meta)
    (""                      . http-object)))  ; By default, assume object.

(defvar *privkey*
  (alexandria:read-file-into-string
   (asdf:system-relative-pathname :activity-servist #p"enc/privkey.pem")))
(defvar *pubkey*
  (alexandria:read-file-into-string
   (asdf:system-relative-pathname :activity-servist #p"enc/pubkey.pem")))



;;; Callbacks
;;; ————————————————————————————————————————
(defun fetch (uri)
  "Runs the user-defined callback FETCH, as stored in *CONFIG*.
Returns the ActivityPub object associated with the given URI."
  (let ((func (getf *config* :fetch)))
    (if func
        (funcall func uri)
        (error "No FETCH function found in ACTIVITY-SERVIST:*CONFIG*."))))



;;; Host-info response
;;; ————————————————————————————————————————
(defun http-host-meta (&optional env path-items params)
  `(200 (:content-type "application/xrd+xml; charset=utf-8")
    (,(str:concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<XRD xmlns=\"http://docs.oasis-open.org/ns/xri/xrd-1.0\">
<link rel=\"lrdd\" type=\"application/xrd+xml\" template=\""
                  (getf *config* :protocol)
                  "://"
                  (getf *config* :address)
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
  (let ((obj (fetch resource)))
    (and obj (webfinger-info resource obj))))

(defgeneric webfinger-info (resource obj)
  (:documentation "Returns a property-list of Webfinger data on the given object.
Override this to provide custom Webfinger data; do what you please, but make sure RESOURCE is set as the :SUBJECT value.

For information on the property-list’s format, see the dosctring of WEBTENTACLE:SERVER."))

;; A default implementation, which provides (likely) all of the information
;; necessary for most use-cases.
(defmethod webfinger-info (resource (obj json-ld:object))
  (let ((obj-uri (json-ld:@id obj)))
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
can be found). Uses the callback :FETCH, defined in *CONFIG*."
  (let* ((uri (reduce (lambda (a b) (format nil "~A/~A" a b))
                     (append (list (getf *config* :host)) path-items)))
         (obj (fetch uri)))
    (if obj
        (list 200 '(:content-type "application/json")
              (list (yason:with-output-to-string* () (yason:encode-object obj))))
        `(400 (:content-type "text/plain")
          ("Such an object doesn’t exist!")))))



;;; Sending a note
;;; ————————————————————————————————————————
(defun note-json (from to text)
  "The JSON of a user's actor."
  (let* ((user-root from)
         (yason:*symbol-encoder* 'yason:encode-symbol-as-lowercase))
    (yason:with-output-to-string* ()
      (yason:encode-alist
       `(("@context" . ("https://www.w3.org/ns/activitystreams"
                        "https://litepub.social/litepub/context.jsonld"))
         ("id" . ,(format nil "~A" (random 900000)))
         ("actor" . ,user-root)
         ("type" . "Create")
         ("object"
          . ,(alexandria:plist-hash-table
              (list
               "id" (format nil "~A" (random 900000))
               "type" "Note"
               "attributedTo" user-root
               "content" text
               "to" (if (listp to) to (list to))))))))))

(defvar +date-header-datetime-format+
  '(:short-weekday ", " (:day 2) " " :short-month " " (:year 4) " "
    (:hour 2) #\: (:min 2) #\: (:sec 2) " " :timezone))

(defun note-headers (inbox from to json)
  (let* ((inbox-uri (quri:uri inbox))
         (digest-header (str:concat "SHA-256=" (string-sha256sum json)))
         (date-header
           (let ((local-time:*default-timezone* local-time:+gmt-zone+))
             (local-time:format-timestring
              nil (local-time:now)
              :format +date-header-datetime-format+)))
         (signed-headers
           (concatenate
            'string
            (format nil "(request-target): post ~A~%" (quri:uri-path inbox-uri))
            (format nil "host: ~A~%" (quri:uri-host inbox-uri))
            (format nil "date: ~A~%" date-header)
            (format nil "digest: ~A" digest-header)))
         (signature (sign-string *privkey* signed-headers))
         (signature-header (str:concat "keyId=\"" from "#main-key\","
                                       "algorithm=\"rsa-sha256\","
                                       "headers=\"(request-target) host date digest\","
                                       "signature=\"" signature "\"")))
    `(("Date" . ,date-header)
      ("Digest" . ,digest-header)
      ("Signature" . ,signature-header)
      ("Host" . ,(quri:uri-host inbox-uri))
      ("Content-Length" . ,(length json))
      ("Accept" . "application/activity+json")
      ("Content-Type" . "application/activity+json"))))

(defun send-note (inbox from to text)
  (let* ((json (note-json from to text))
         (headers (note-headers inbox from to json)))
    (dexador:post inbox :content json
                        :headers headers)))



;;; Misc. responses
;;; ————————————————————————————————————————
(defun http-404 (env path-items params)
  "The default 404 response."
  '(404 (:content-type "text/plain")
    ("404, you goddamn fool!")))

(defvar *logs* '())



;;; Invocation
;;; ————————————————————————————————————————
(defun server (env)
  "Returns the response data for Clack, given the request property-list ENV."
  (nconc *logs* (list env (babel:octets-to-string (alexandria:read-stream-content-into-byte-vector (getf env :raw-body)))))
  (let* ((path   (pathname-sans-parameters (getf env :request-uri)))
         (params (pathname-parameters      (getf env :request-uri)))
         (response-function
           (or (assoc-by-path (directories) (pathname-components path))
               '("" . http-404)))
         ;; So that response functions only deal with relative paths…
         (path-sans-response-root
           (pathname-components
            (str:replace-first (car response-function) "" path))))
    (format nil "Path: ~s" path)
    (or (funcall (cdr response-function) env path-sans-response-root params)
        (funcall 'http-404 env path-sans-response-root params))))

(defun start-server ()
  "Start the server."
  (clack:clackup (lambda (env)
                   (server env))
                 :server 'woo
                 :address "0.0.0.0"
                 :port (getf *config* :port)))



;;; Utils.
;;; ————————————————————————————————————————
(defun assoc-by-path (alist path-items &optional (depth 0))
  "Given an associative list and a path decomposed into a list of
its components, return the item with the closest according
pathname as key. If the exact path isn't a valid key, it will
try all parent directories.
E.g., “/bear/apple/momma/” could match either “/bear/apple/momma”
or “/bear/apple/” or “/bear/”, but not “/bear” (not a directory)."
  (let ((path (str:join #\/ path-items)))
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
     (let ((pair-items (str:split #\= pair)))
       (cons (car pair-items)
             (cadr pair-items))))
   (str:split #\&  (cadr (str:split #\? path)))))

(defun pathname-components (pathname)
  "Split a pathname into a list of its components.
“/u/bear/apple.txt” → '(“u” “bear” “apple.txt”)"
  (str:split #\/ pathname :omit-nulls 't))

(defun sequence-hexadecimal-string (sequence)
  (reduce #'str:concat
          (loop for number across
                sequence
                collect (format nil "~X" number))))
