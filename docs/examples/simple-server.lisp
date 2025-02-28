;;;; simple-server: A bare-bones example ActivityPub instance.

;; Written in 2024–2025 by Jaidyn Levesque <jadedctrl@posteo.at>
;;
;; To the extent possible under law, the author(s) have dedicated
;; all copyright and related and neighboring rights to this
;; software to the public domain worldwide. This software is
;; distributed without any warranty.
;;
;; You should have received a copy of the CC0 Public Domain
;; Dedication along with this software. If not, see
;; <http://creativecommons.org/publicdomain/zero/1.0/>.

(defpackage #:activitypub-example
  (:use #:cl)
  (:export :start-server :seed)
  ;; Note that we use a nickname for the ActivityStreams vocab!
  ;; I recommend doing likewise; save yourself some typing!
  (:local-nicknames (:ass :activity-servist/vocab/activity)
                    (:lp :activity-servist/vocab/litepub)))

(in-package #:activitypub-example)


;;; Globals
;;; ————————————————————————————————————————
(defvar *store* (make-hash-table :test #'equal)
  "Our “object-store” — stores all ActivityPub objects, mapped by their IRI @ID.")

(defvar *inbox* nil
  "Our inbox, a simple list containing all received objects.")

(defvar *config*
  '(:host "http://localhost:8080" :address "0.0.0.0" :port 8080 :retrieve retrieve))

(defvar *user-id-format* "~A/users/~A"
  "The format we use for user’s @IDs/URIs.
The first parameter is the protocol+host, and the second is the username.
For example: “https://localhost:8080/users/lena”.")



;;; Invocation
;;; ————————————————————————————————————————
(defun start-server ()
  "Start the server; simple and to-the-point."
  (clack:clackup
   (lambda (env)
     (let {[activity-servist:*config* *config*]}
       (activity-servist:server env)))
   :server  'woo
   :address (getf *config* :address)
   :port    (getf *config* :port)))

(defun seed ()
  "Seed our server with some random users, for testing purposes."
  (as:store (make-user "maria5"  "Maria ^_^"))
  (as:store (make-user "melanie" "Melanie >:o"))
  (as:store (make-user "jorge"   "Jorge 🦆")))



;;; Activity-Servist callbacks
;;; ————————————————————————————————————————
(defun retrieve (uri)
  "activity-servist callback: Returns the JSON-LD OBJECT of the given @ID or URI
from our object-store.
This example server simply stores objects in a hash-table mapping IDs to objects."
  (let {[id (or (uri->id uri) uri)]}
    (gethash id *store*)))


(defmethod as:receive ((obj json-ld:object))
  "activity-servist callback: Recieve a JSON-LD OBJECT (posted to the server's
inbox, and decide what to do with it!"
  (setq *inbox* (append *inbox* (list obj))))


(defmethod as:store ((obj json-ld:object))
  "activity-servist callback: Store a foreign JSON-LD OBJECT (fetched during
operation of the server) in our object-store."
  (setf (gethash (json-ld:@id obj) *store*) obj))



;;; ID-management
;;; ————————————————————————————————————————
(defun uri->id (uri-str)
  "Returns an object’s @ID corresponding to the URI string.
For most cases, returning URI-STR directly is what you want. But there are two exceptions:
  • Account URIs of format “acct:username@host.tld”, as used by WebFinger.
  • Aliases, if an object can be accessed through several URIs.

This example only handles the first exception, acct: URIs."
  (let* {[uri    (quri:uri uri-str)]
         [scheme (quri:uri-scheme uri)]}
    (if (or (not scheme)
            (equal scheme "acct"))
        (acct-uri->id uri)
        uri-str)))

(defun acct-uri->id (uri)
  "Helper-function for URI->ID. Returns the @ID of an acct:-format URI.
That is, an “acct:username@host.tld” URI."
  (let* {[path             (quri:uri-path uri)]
         [sans-preceding-@ (if (str:starts-with-p "@" path)
                               (subseq path 1)
                               path)]}
    (destructuring-bind (user host)
        (str:split "@" sans-preceding-@)
      (format nil *user-id-format*
              (host-w-scheme host) user))))

(defun host-w-scheme (hostname)
  "Helper-function for ACCT-URI->ID. From a hostname, returns “scheme://hostname”.
If it matches our configured :HOST (in *CONFIG*), simply returns :HOST’s value.
Otherwise, assume “https”."
  (let {[our-host (getf *config* :host)]}
    (if (equal (quri:uri-host (quri:uri our-host)) hostname)
        our-host
        (format nil "https://~A" hostname))))



;;; Users
;;; ————————————————————————————————————————
(defclass user (lp:person)
  ((our-inbox
    :accessor user-inbox
    :initform nil
    :documentation "A list of objects in the user’s inbox.")
   (our-outbox
    :accessor user-outbox
    :initform nil
    :documentation "A list of objects in the user’s outbox.")
   (our-following
     :accessor user-following
     :initform nil
     :documentation "A list of Actor objects that the user is following.")
   (our-followers
    :accessor user-followers
    :initform nil
    :documentation "A list of Actor objects that follow the user.")))

(defun make-user (username nickname)
  "Create a USER of the given USERNAME and NICKNAME.
The ID and ENDPOINTS are derived using the parameter USERNAME and the global *USER-ID-FORMAT*."
  (let {[uri (format nil *user-id-format*
                     (getf *config* :host) username)]}
    (flet {[sub-user-uri (path)
             (format nil "~A~A" uri path)]
           [sub-host-uri (path)
             (format nil "~A~A" (getf *config* :host) path)]}
      (make-instance 'user :@id  uri
                           :name nickname
                           :preferred-username username
                           :inbox      (sub-host-uri "/inbox")
                           :outbox     (sub-host-uri "/outbox")
                           :following  (sub-user-uri "/following")
                           :followers  (sub-user-uri "/followers")
                           :public-key (alexandria:alist-hash-table
                                        `(("id" . ,(sub-user-uri "#main-key"))
                                          ("owner" . ,uri)
                                          ("publicKeyPem" . ,*public-key*)))))))
