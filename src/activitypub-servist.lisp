;;; Copyright © 2023-2024 Jaidyn Levesque <jadedctrl@posteo.at>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(defpackage #:activitypub-servist
  (:use #:cl)
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


(defvar *privkey* (alexandria:read-file-into-string #p"enc/privkey.pem"))
(defvar *pubkey* (alexandria:read-file-into-string #p"enc/pubkey.pem"))



;; ————————————————————————————————————————
;; Host-info response
;; ————————————————————————————————————————
(defun http-host-meta (&optional env path-items params)
  `(200 (:content-type "application/xrd+xml; charset=utf-8")
    (,(str:concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<XRD xmlns=\"http://docs.oasis-open.org/ns/xri/xrd-1.0\">
<link rel=\"lrdd\" type=\"application/xrd+xml\" template=\"https://"
                  (getf env :domain)
                  "/.well-known/webfinger?resource={uri}\"/>
</XRD>
"))))



;; ————————————————————————————————————————
;; Webfinger response
;; ————————————————————————————————————————
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
               type "application/activity+json")
         (template ,(str:concat "https://" (cdr userhost) "/ostatus_subscribe?acct={uri}")
                   rel "http://ostatus.org/schema/1.0/subscribe"))))))




;; ————————————————————————————————————————
;; User info response(s)
;; ————————————————————————————————————————
;; Respond to requests within the /u/* directory.
(defun http-user-dir (env path-items params)
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



;; ————————————————————————————————————————
;; Sending a note
;; ————————————————————————————————————————
(defun note-json (from to text)
  "The JSON of a user's actor."
  (let* ((user-root (str:concat "https://etc.xwx.moe/u/" from))
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
         (signature (base64:usb8-array-to-base64-string
                     (ironclad:sign-message (openssl-shell-import-key-pair *privkey*)
                                            (string-to-ub8-vector
                                             (string-sha256sum signed-headers)))))
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




;; ————————————————————————————————————————
;; Misc. responses
;; ————————————————————————————————————————
(defun http-404 (env path-items params)
  "The default 404 response."
  '(404 (:content-type "text/plain")
    ("404, you goddamn fool!")))

(defvar *logs* '())

;; ————————————————————————————————————————
;; Invocation
;; ————————————————————————————————————————
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



;; ————————————————————————————————————————
;; Utils.
;; ————————————————————————————————————————
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


(defun string-to-ub8-vector (string)
  "Convert the given STRING into an unsigned 8-bit vector."
  (coerce
   (loop for char across string
         collect (char-code char))
   '(vector (unsigned-byte 8))))


(defun digest-string (digest-spec string)
  "Compute the digest of a STRING, given an Ironclad DIGEST-SPEC."
  (ironclad:digest-sequence digest-spec (string-to-ub8-vector string)))


(defun string-sha256sum (string)
  "Compute the sha256 checksum of a STRING, in hexadecimal string-format."
  (base64:usb8-array-to-base64-string
   (digest-string (ironclad:make-digest :sha256) string)))


(defun sequence-hexadecimal-string (sequence)
  (reduce #'str:concat
          (loop for number across
                sequence
                collect (format nil "~X" number))))



;; ————————————————————————————————————————
;; RSA keys
;; ————————————————————————————————————————
;; At the moment, I’ve yet to use figure out how to create PEM representations of
;; a public keypair properly in Lisp.
;; So at the moment, keys are generated into PEM files by the openssl binary on
;; the host’s system; and the output of the openssl command is used to parse into
;; Ironclad keys.
;; Yes, I know, this is absolutely horrific. Actually disgusting.
;; But at the moment,I want to focus on other core parts of ActivityPub; I’ve
;; tired of messing with ASN1 & co. That’s for another day! ^^
(defun openssl-shell-generate-key-pair ()
  "Generate a 2048-bit RSA key-pair in PEM-format using one’s `openssl` binary.
It returns two values: The private key, then the public key."
  (let* ((private-pem-key (inferior-shell:run/s "openssl genrsa 2048"))
         (public-pem-key
           (inferior-shell:run/s
            `(inferior-shell:pipe (echo ,private-pem-key)
                                  (openssl rsa -outform PEM -pubout)))))
    (values private-pem-key
            public-pem-key)))


(defun openssl-shell-destructure-private-key (pem-string &optional results)
  "When passed the output of the shell command `openssl rsa -text -noout`, will
parse the output into a plist containing relavent numbers:
  :n (modulus), :e (public exponent), :d (private exponent), :p (1st prime),
  :q (2nd prime), :e1 (1st exponent), :e2 (2nd exponent), and :c (coefficient)."
  (let* ((lines (if (stringp pem-string)
                    (inferior-shell:run/lines
                     `(inferior-shell:pipe
                       (echo ,pem-string)
                       (openssl rsa -text -noout)))
                    pem-string))
         (line (str:trim (car lines))))
    (cond
      ((not lines)
       (mapcar
        (lambda (result-item)
          (if (stringp result-item)
              (parse-integer (str:replace-all ":" "" result-item) :radix 16)
              result-item))
        results))
      ((str:starts-with-p "Private" line)
       (openssl-shell-destructure-private-key
        (cdr lines) results))
      ((str:starts-with-p "modulus:" line)
       (openssl-shell-destructure-private-key
        (cdr lines) (nconc results '(:n))))
      ((str:starts-with-p "prime1" line)
       (openssl-shell-destructure-private-key
        (cdr lines) (nconc results '(:p))))
      ((str:starts-with-p "prime2" line)
       (openssl-shell-destructure-private-key
        (cdr lines) (nconc results '(:q))))
      ((str:starts-with-p "exponent1" line)
       (openssl-shell-destructure-private-key
        (cdr lines) (nconc results '(:e1))))
      ((str:starts-with-p "exponent2" line)
       (openssl-shell-destructure-private-key
        (cdr lines) (nconc results '(:e2))))
      ((str:starts-with-p "coefficient" line)
       (openssl-shell-destructure-private-key
        (cdr lines) (nconc results '(:c))))
      ((str:starts-with-p "privateExponent" line)
       (openssl-shell-destructure-private-key
        (cdr lines) (nconc results '(:d))))
      ((str:starts-with-p "publicExponent" line)
       (openssl-shell-destructure-private-key
        (cdr lines)
        (nconc
         results
         (list
          :e
          (parse-integer
           (car (str:split #\space
                           (str:replace-first
                            "publicExponent: "
                            ""
                            line))))))))
      ('t
       (let* ((last-element (car (last results)))
              (total-string (if (stringp last-element)
                                (str:concat last-element line)
                                line)))
         (openssl-shell-destructure-private-key
          (cdr lines)
          (if (stringp last-element)
              (nconc (reverse (cdr (reverse results)))
                     (list total-string))
              (nconc results
                     (list total-string)))))))))


(defun openssl-shell-import-key-pair (private-pem-string)
  "Given the string value of a private RSA PEM file, this will parse it into two
returned values: An Ironclad private key, and an Ironclad public key."
  (let ((key-values
          (openssl-shell-destructure-private-key private-pem-string)))
    (values (ironclad:make-private-key
             :rsa
             :n (getf key-values :n)
             :e (getf key-values :e)
             :d (getf key-values :d)
             :p (getf key-values :p)
             :q (getf key-values :q))
            (ironclad:make-public-key
             :rsa
             :n (getf key-values :n)
             :e (getf key-values :e)))))


(defun openssl-shell-sign-string (private-pem-string string)
  "Use the OpenSSL binary on the host system to RSS-SHA256 sign a STRING with a
private key."
  (alexandria:write-string-into-file private-pem-string #p"private.pem" :if-exists :overwrite)
  (apply #'str:concat
         (inferior-shell:run/lines
          `(inferior-shell:pipe
            (printf ,string)
            (openssl dgst -sha256 -sign private.pem -)
            (base64)))))
 
