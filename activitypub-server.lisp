;;
;; Copyright 2023, Jaidyn Levesque <jadedctrl@posteo.at>
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
;;

(ql:quickload '(clack str yason))


;; List of the server's usernames.
(defun users () '("rod" "mum"))

;; Alist of the server's paths and their response functions.
(defun directories () '(("u/" . http-user-dir)))


;; The default 404 response.
(defun http-404 (env path-items params)
  '(404 (:content-type "text/plain")
    ("404, you goddamn fool!")))


;; Respond to requests within the /u/* directory.
(defun http-user-dir (env path-items params)
  (let ((user (car path-items)))
    ;; In case of request for the user's actor.
    (if (member user (users) :test 'string=)
        `(200 (:content-type "application/ld+json")
              (,(user-actor user))))))


;; Returns the response data for Clack, given the request data `env`.
(defun server (env)
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


;; Start the server.
(defparameter *handler*
  (clack:clackup (lambda (env)
                   (funcall 'server env))
                 :server 'woo))


;; The JSON of a user's actor.
(defun user-actor (username)
  (let* ((host "http://localhost")
         (user-root (str:concat host "/u/" username)))
    (yason:with-output-to-string* ()
      (yason:encode-alist
       `(("@context" . ("https://www.w3.org/ns/activitystreams"
                        "https://w3id.org/security/v1"))
         ("id" . ,user-root)
         ("type" . "Person")
         ("preferredUsername" . ,username)
         ("inbox" . ,(str:concat user-root "/inbox.json"))
         ("outbox" . ,(str:concat user-root  "/outbox.json")))))))


;; Given an associative list and a path decomposed into a list of
;; its components, return the item with the closest according
;; pathname as key. If the exact path isn't a valid key, it will
;; try all parent directories.
;; E.g., "/bear/apple/momma/" could match either "/bear/apple/momma"
;; or "/bear/apple/" or "/bear/", but not "/bear" (not a directory).
(defun assoc-by-path (alist path-items &optional (depth 0))
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


;; Removes parameters from a URI pathname, returning the bare path.
;; "/path/a/b?a=1&b=3" → "/path/a/b"
(defun pathname-sans-parameters (path)
  (car (str:split #\? path)))


;; Convert the parameters of a URI pathname into an associative list.
;; "/path/a/b?a=1&b=2&c=3" → (("a" . "1") ("b" . "2") ("c" . "3"))
(defun pathname-parameters (path)
  (mapcar
   (lambda (pair)
     (let ((pair-items (str:split #\= pair)))
       (cons (car pair-items)
             (cadr pair-items))))
   (str:split #\&  (cadr (str:split #\? path)))))


;; Split a pathname into a list of its components.
;; "/u/bear/apple.txt" → '("u" "bear" "apple.txt")
(defun pathname-components (pathname)
  (str:split #\/ pathname :omit-nulls 't))

