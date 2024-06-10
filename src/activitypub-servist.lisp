;;;; activitypub-servist: An ActivityPub server framework.

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

(defpackage #:activitypub-servist
  (:use #:cl #:activitypub-servist/signatures)
  (:export :server :start-server))

(in-package #:activitypub-servist)


(defun users ()
  "List of the server's usernames."
  '("servistchjo"))

(defun userhosts ()
  "List of the server's usernames + hostname."
  (mapcar (lambda (username) (str:concat username "@" "etc.xwx.moe"))
          (users)))

(defun directories ()
  "Alist of the server's paths and their response functions."
  '(("u/" . http-user-dir)
    (".well-known/webfinger" . http-webfinger)
    (".well-known/host-meta" . http-host-meta)))


(defvar *privkey* (alexandria:read-file-into-string #p"../enc/privkey.pem"))
(defvar *pubkey* (alexandria:read-file-into-string #p"../enc/pubkey.pem"))



;;; Host-info response
;;; ————————————————————————————————————————
(defun http-host-meta (&optional env path-items params)
  `(200 (:content-type "application/xrd+xml; charset=utf-8")
    (,(str:concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<XRD xmlns=\"http://docs.oasis-open.org/ns/xri/xrd-1.0\">
<link rel=\"lrdd\" type=\"application/xrd+xml\" template=\"https://"
                  (getf env :domain)
                  "/.well-known/webfinger?resource={uri}\"/>
</XRD>
"))))



;;; Webfinger response
;;; ————————————————————————————————————————
(defun http-webfinger (env path-items params)
  (webtentacle:server env (lambda (resource) (resource-info-func resource)) nil))

(defun resource-userhost (resource)
  "Given an account URI in webfinger-friendly format, return the corresponding)))
username and host in a list. Whether or not these are valid… that’s your
business!
Ex:  acct:mom@bird.com → '(“mom” “bird.com”)"
  (cond
    ;; A @bird@mom-style resource
    ((str:containsp "@" resource)
     (let* ((sans-acct (if (str:starts-with-p "acct:" resource)
                           (subseq resource 5)
                           resource))
            (sans-@ (if (str:starts-with-p "@" sans-acct)
                        (subseq sans-acct 1)
                        sans-acct)))
       (destructuring-bind (user host)
           (str:split "@" sans-@)
         (cons user host))))
    ;; A URL-style resource
    ((str:containsp "/u/" resource)
     (cons
      (pathname-name resource)
      (purl:url-host resource)))))

(defun resource-valid-p (resource)
  "Given a webfinger-style “resource”"
  (let* ((userhost (resource-userhost resource))
         (userhost-str (funcall #'str:concat (car userhost) "@" (cdr userhost))))
   (member userhost-str (userhosts)
           :test (lambda (a b)
                   (string-equal (string-downcase a)
                                 (string-downcase b))))))

(defun resource-info-func (resource)
  "Given a webfinger RESOURCE, return a property-list of data on the given user…
if they exist, that is.
This is used by the WEBTENTACLE webfinger server; you can see information on
the plist in the docstring for its WEBTENTACLE:SERVER function."
  (let* ((userhost (resource-userhost resource))
         (profile (str:concat "https://" (cdr userhost) "/u/" (car userhost))))
    (when (resource-valid-p resource)
      (list
       :subject (str:concat "acct:" (car userhost) "@" (cdr userhost))
       :aliases `(,profile)
       :links
       `((href ,profile
               rel  "self"
               type "application/activity+json")
         (href ,profile
               rel "self"
               type "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
         (template ,(str:concat "https://" (cdr userhost) "/ostatus_subscribe?acct={uri}")
                   rel "http://ostatus.org/schema/1.0/subscribe"))))))



;;; User info response(s)
;;; ————————————————————————————————————————
(defun http-user-dir (env path-items params)
  "Respond to requests within the /u/* directory."
  (let ((user (car path-items)))
    ;; In case of request for the user's actor.
    (if (member user (users) :test 'equal)
        `(200 (:content-type "application/activity+json")
              (,(user-actor env user))))))

(defun user-actor (config username)
  "The JSON of a user's actor."
  (let* ((user-root (str:concat "https://" (getf config :domain) "/u/" username))
         (yason:*symbol-encoder* 'yason:encode-symbol-as-lowercase))
    (yason:with-output-to-string* ()
      (yason:encode-alist
       `(("@context" . ("https://www.w3.org/ns/activitystreams"
                        "https://w3id.org/security/v1"
                        "https://litepub.social/litepub/context.jsonld"))
         ("endpoints" . ,(alexandria:plist-hash-table (list "sharedInbox" "https://etc.xwx.moe/inbox")))
         ("url" . ,user-root)
         ("id" . ,user-root)
         ("type" . "Person")
         ("preferredUsername" . ,username)
         ("name" . "Servistiĉo")
         ("inbox" . ,(str:concat user-root "/inbox"))
         ("outbox" . ,(str:concat user-root  "/outbox"))
         ("discoverable" . t)
         ("summary" . "Mi estas simpla roboto, kiu montras ke iomete ekfunkcias activitypub-servist.
… ĉu mi rajtas demeti la servistinan kostumon, nun?
Mi ne estas knabino!!")
         ("icon"
          . ,(alexandria:plist-hash-table
              (list
               "type" "Image"
               "url" "https://xwx.moe/etc/servisticho-profilbildo.jpg")))
         ("image"
          . ,(alexandria:plist-hash-table
              (list
               "type" "Image"
               "url" "https://xwx.moe/etc/servisticho-standardo.png")))
         ("publicKey"
          . ,(alexandria:plist-hash-table
              (list
               "id" (str:concat user-root "#main-key")
               "owner" user-root
               "publicKeyPem" *pubkey*))))))))



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
;;         (signature (base64:usb8-array-to-base64-string
;;                     (ironclad:sign-message (openssl-shell-import-key-pair *privkey*)
;;                                            (string-to-ub8-vector
;;                                             (string-sha256sum signed-headers))))
         (signature (openssl-shell-sign-string *privkey* signed-headers))
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
  "Returns the response data for Clack, given the request data `env`."
  (setq *logs* (append *logs* (list env (babel:octets-to-string (alexandria:read-stream-content-into-byte-vector (getf env :raw-body))))))
  (let* ((path (pathname-sans-parameters (getf env :request-uri)))
         (params (pathname-parameters (getf env :request-uri)))
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

(defun start-server (&optional (config '(:domain "localhost" :port 8080)))
  "Start the server."
  (clack:clackup (lambda (env)
                   (server (append env config)))
                 :server 'woo
                 :address "0.0.0.0"
                 :port (getf config :port)))



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
