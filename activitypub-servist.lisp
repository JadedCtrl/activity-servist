;;; Copyright © 2023 Jaidyn Levesque <jadedctrl@posteo.at>
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

;;(ql:quickload '(alexandria asn1 clack cl-base64 ironclad purl str trivia trivial-utf-8 webtentacle yason))


(defun users ()
  "List of the server's usernames."
  '("rod@localhost" "mum@localhost"))


(defun directories ()
  "Alist of the server's paths and their response functions."
  '(("u/" . http-dir) (".well-known/webfinger" . http-webfinger)))



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
   (member userhost-str (users)
           :test (lambda (a b)
                   (string-equal (string-downcase a)
                                 (string-downcase b))))))


(defun resource-info-func (resource)
  "Given a webfinger RESOURCE, return a property-list of data on the given user…
if they exist, that is.
This is used by the WEBTENTACLE webfinger server; you can see information on
the plist in the docstring for its WEBTENTACLE:SERVER function."
  (let* ((userhost (resource-userhost resource))
         (profile (str:concat "https://" (cdr userhost) "/users/" resource)))
    (when (resource-valid-p resource)
      (list
       :subject resource
       :links
       `((href ,profile
          rel  "http://webfinger.net/rel/profile-page"
          type "text/html"
               properties (:apple 3 :bear 4))
         (href ,profile
          rel  "self"
          type "application/activity+json"))))))



;; ————————————————————————————————————————
;; User info response(s)
;; ————————————————————————————————————————
;; Respond to requests within the /u/* directory.
(defun http-user-dir (env path-items params)
  (let ((user (car path-items)))
    ;; In case of request for the user's actor.
    (if (member user (users) :test 'string=)
        `(200 (:content-type "application/ld+json")
              (,(user-actor env user))))))


(defun user-actor (config username)
  "The JSON of a user's actor."
  (let* ((user-root (str:concat "https://" (getf config :domain) "/u/" username)))
    (yason:with-output-to-string* ()
      (yason:encode-alist
       `(("@context" . ("https://www.w3.org/ns/activitystreams"
                        "https://w3id.org/security/v1"))
         ("id" . ,user-root)
         ("type" . "Person")
         ("preferredUsername" . ,username)
         ("inbox" . ,(str:concat user-root "/inbox.json"))
         ("outbox" . ,(str:concat user-root  "/outbox.json")))))))



;; ————————————————————————————————————————
;; Misc. responses
;; ————————————————————————————————————————
(defun http-404 (env path-items params)
  "The default 404 response."
  '(404 (:content-type "text/plain")
    ("404, you goddamn fool!")))



;; ————————————————————————————————————————
;; Invocation
;; ————————————————————————————————————————
(defun server (env)
  "Returns the response data for Clack, given the request data `env`."
  (let* ((path (pathname-sans-parameters (getf env :request-uri)))
         (params (pathname-parameters (getf env :request-uri)))
         (response-function
           (or (assoc-by-path (directories) (pathname-components path))
               '("" . http-404)))
         ;; So that response functions only deal with relative paths…
         (path-sans-response-root
           (pathname-components
            (str:replace-first (car response-function) "" path))))
    (or (funcall (cdr response-function) env path-sans-response-root params)
        (funcall 'http-404 env path-sans-response-root params))))


(defun start-server (&optional (config '(:domain "localhost")))
  "Start the server."
  (clack:clackup (lambda (env)
                   (server (append env config)))
                 :server 'woo
                 :address "0.0.0.0"
                 :port 8080))



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
