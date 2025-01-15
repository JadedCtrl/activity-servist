;;;; util: General utilities used in several parts of activity-servist.

;; Copyright © 2025 Jaidyn Ann <jadedctrl@posteo.at>
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

(defpackage #:activity-servist/util
  (:use #:cl)
  (:nicknames "AS/U")
  (:export
   ;; Functions
   #:http-get
   ;; Symbols
   #:http-request-error #:http-get-error
   ;; Slots/Accessors
   :body :status))

(in-package #:activity-servist/util)

(defun http-get (uri &key (accept "application/activity+json,application/ld+json"))
  "Perform a GET request, returning two values: The response-body’s string, and the status code.
:ACCEPT, defaulting to ActivityPub-related types, corresponds to the “Accept” header.

If a non-2XX status-code is returned, the HTTP-GET-ERROR condition will be signaled."
  (multiple-value-bind (body status)
      (drakma:http-request uri :accept accept :force-binary t)
    (let ((body-str (ignore-errors (when body (flexi-streams:octets-to-string body)))))
      (when (or (< status 200) (>= status 300))
        (signal 'http-get-error :status status :body body-str))
      (values body-str status))))

(define-condition http-request-error (condition)
  ((status :initarg :status)
   (body   :initarg :body))
  (:documentation "Signaled when a HTTP request returns a non-2XX status code."))

(define-condition http-get-error (http-request-error)
  ()
  (:report (lambda (condition stream) (declare (ignore condition))
             (format stream "Received non-2XX status (~A) in response to GET request. Response body:~%~A~&"
                     (slot-value condition 'status)
                     (slot-value condition 'body))))
  (:documentation "Signaled when an HTTP GET request returns a non-2XX status code."))

