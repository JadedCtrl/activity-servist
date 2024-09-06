;;;; simple-server: A bare-bones example ActivityPub instance.

;; Written in 2024 by Jaidyn Levesque <jadedctrl@posteo.at>
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
  (:local-nicknames (:as :activity-servist/vocab/activity)))

(in-package #:activitypub-example)


;;; Globals
;;; ————————————————————————————————————————
(defvar *store* (make-hash-table :test #'equal)
  "Our “object-store” — stores all ActivityPub objects, mapped by their IRI @ID.")

(defvar *config* '(:address "localhost" :protocol "http" :port 8080 :fetch fetch))

(defvar *user-id-format* "~A://~A/users/~A"
  "The format we use for user’s @IDs/URIs.
The first parameter is the protocol, the second the host, and the third the username.
For example: “https://localhost:8080/users/lena”.")



;;; Invocation
;;; ————————————————————————————————————————
(defun start-server ()
  "Start the server; simple and to-the-point."
  (clack:clackup
   (lambda (env)
     (let ((activity-servist:*config* *config*))
       (activity-servist:server env)))
   :server  'woo
   :address (getf *config* :address)
   :port    (getf *config* :port)))

(defun seed ()
  "Seed our server with some random users, for testing purposes."
  (save (make-user "maria" "Maria ♥"))
  (save (make-user "melanie" "Melanie >:o"))
  (save (make-user "jorge" "Jorge 🦆")))



;;; Activity-Servist callbacks
;;; ————————————————————————————————————————
(defun fetch (uri)
  "activity-servist callback); returns the JSON-LD-OBJECT of the given @ID or URI.
This example server simply stores objects in a hash-table mapping IDs to objects."
  (let ((id (or (uri->id uri) uri)))
    (gethash id *store*)))



;;; ID-management
;;; ————————————————————————————————————————
(defun uri->id (uri-str)
  "Returns an object’s @ID corresponding to the URI string.
For most cases, returning URI-STR directly is what you want. But there are two exceptions:
  • Account URIs of format “acct:username@host.tld”, as used by WebFinger.
  • Aliases, if an object can be accessed through several URIs.

This example only handles the first exception, acct: URIs."
  (let* ((uri    (quri:uri uri-str))
         (scheme (quri:uri-scheme uri)))
    (if (or (not scheme)
            (equal scheme "acct"))
        (acct-uri->id uri)
        uri-str)))

(defun acct-uri->id (uri)
  "Helper-function for URI->ID. Returns the @ID of an acct:-format URI.
That is, an “acct:username@host.tld” URI."
  (let* ((path             (quri:uri-path uri))
         (sans-preceding-@ (if (str:starts-with-p "@" path)
                               (subseq path 1)
                               path)))
    (destructuring-bind (user host)
        (str:split "@" sans-preceding-@)
      (format nil *user-id-format*
              (host-scheme host) host user))))

(defun host-scheme (hostname)
  "Helper-function for ACCT-URI->ID. Returns the expected protocol of a hostname.
If it’s our configured :ADDRESS (in *CONFIG*), then return *CONFIG*’s :PROTOCOL.
Otherwise, assume “https”."
  (if (equal (getf *config* :address) hostname)
      (getf *config* :protocol)
      "https"))



;;; Data-management
;;; ————————————————————————————————————————
(defun save (obj)
  "Save an object to our object-store, *STORE*."
  (setf (gethash (json-ld:@id obj) *store*) obj))



;;; Users
;;; ————————————————————————————————————————
(defclass user (as:person)
  ((inbox
    :accessor user-inbox
    :initform nil
    :documentation "A list of objects in the user’s inbox.")
   (outbox
    :accessor user-outbox
    :initform nil
    :documentation "A list of objects in the user’s outbox.")
   (following
     :accessor user-following
     :initform nil
     :documentation "A list of Actor objects that the user is following.")
   (followers
    :accessor user-followers
    :initform nil
    :documentation "A list of Actor objects that follow the user.")))

(defun make-user (username nickname)
  "Create a USER of the given USERNAME and NICKNAME.
The ID and ENDPOINTS are derived using the parameter USERNAME and the global *USER-ID-FORMAT*."
  (let ((obj (make-instance 'user))
        (uri (format nil *user-id-format*
                     (getf *config* :protocol)
                     (getf *config* :address)
                     username)))
    (flet ((sub-uri (path)
             (format nil "~A/~A" uri path)))
      (setf (as:name obj)      username)
      (setf (json-ld:@id obj)  uri)
      (setf (as:inbox obj)     (sub-uri "inbox"))
      (setf (as:outbox obj)    (sub-uri "outbox"))
      (setf (as:following obj) (sub-uri "following"))
      (setf (as:followers obj) (sub-uri "followers")))
   obj))
