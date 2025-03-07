;;;; activity-servist/signatures: Handle AP-compatible HTTP signatures.

;; Copyright © 2023-2025 Jaidyn Ann <jadedctrl@posteo.at>
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

(defpackage #:activity-servist/signatures
  (:use #:cl)
  (:nicknames "AS/S")
  (:export :generate-key-pair
           :sign-string :signature-valid-p
           :string-sha256sum))

(in-package #:activity-servist/signatures)


;;; Macros
;;; ————————————————————————————————————————
(defmacro with-temporary-file (varspec &body body)
  "A wrapper around UIOP:WITH-TEMPORARY-FILE that ensures the file is only readable
by the current process’es user, and writes a default content-string to it.

VARSPEC should be of the form: (PATH-SYM PREFIX-STRING CONTENT-STRING-OR-USB8-ARRAY)"
  (destructuring-bind (pathname prefix contents)
      varspec
    `(uiop:with-temporary-file (:pathname ,pathname :prefix ,prefix)
       (setf (osicat:file-permissions ,pathname) '(:user-read :user-write))
       (typecase ,contents
         (string
          (alexandria:write-string-into-file ,contents ,pathname :if-exists :overwrite))
         ((simple-array (unsigned-byte 8))
          (alexandria:write-byte-vector-into-file ,contents ,pathname :if-exists :overwrite)))
       ,@body)))



;;; Key-generation
;;; ————————————————————————————————————————
;; At the moment, I’ve yet to use figure out how to create PEM representations of
;; a public keypair properly in Lisp.
;; So at the moment, keys are generated into PEM files by the openssl binary on
;; the host’s system; and the output of the openssl command is used to parse into
;; Ironclad keys.
;; Yes, I know, this is absolutely horrific. Actually disgusting.
;; But at the moment,I want to focus on other core parts of ActivityPub; I’ve
;; tired of messing with ASN1 & co. That’s for another day! ^^
(defun generate-key-pair ()
  "Generate a 2048-bit RSA key-pair in PEM-format using the host system’s `openssl` binary.
It returns two values: The private key, then the public key."
  (let* ((private-pem-key (inferior-shell:run/s "openssl genrsa 2048"))
         (public-pem-key
           (with-temporary-file (private-key-path "activityservist-" private-pem-key)
             (inferior-shell:run/s
              `(openssl rsa -outform pem -pubout -in ,private-key-path)))))
    (values private-pem-key
            public-pem-key)))



;;; Signing & verification
;;; ————————————————————————————————————————
(defun sign-string (private-pem-string string)
  "RSA-SHA256 signs a STRING with a private key, returning a base64 string.
Uses the host-system’s `base64`, `openssl`, & `printf` binaries."
  (with-temporary-file (key-pathname "activityservist-" private-pem-string)
    (apply #'str:concat
           (str:lines
            (inferior-shell:run/s
             `(inferior-shell:pipe
               (printf ,string)
               (openssl dgst -sha256 -sign ,key-pathname -)
               (base64)))))))

(defun signature-valid-p (public-pem-string string signature)
  "Check the validity of a RSA-SHA256 SIGNATURE of a STRING.
SIGNATURE should be a base64 string.
Uses the host-system’s `openssl` & `printf` binaries."
  (handler-case
      (let ((signature-usb8-array (base64:base64-string-to-usb8-array signature)))
        (with-temporary-file   (key-pathname "activityservist-" public-pem-string)
          (with-temporary-file (sig-pathname "activityservist-" signature-usb8-array)
            ;; (inferior-shell:run/nil `(cp ,key-pathname /tmp/key.pem))
            ;; (inferior-shell:run/nil `(cp ,sig-pathname /tmp/sig)) ; Useful for debugging
            (inferior-shell:run/lines
             `(inferior-shell:pipe
               (printf ,string)
               (openssl dgst -sha256 -verify ,key-pathname -signature ,sig-pathname -))))))
    ;; An exit-code of 1 means signature-verification failed. Anything else is
    ;; probably an actual error.
    (uiop/run-program:subprocess-error (err)
      (if (eq (slot-value err 'uiop/run-program::code) 1)
          nil
          (error err)))))



;;; Misc.
;;; ————————————————————————————————————————
(defun string-sha256sum (string)
  "Compute the sha256 checksum of a STRING, in hexadecimal string-format."
  (base64:usb8-array-to-base64-string
   (digest-string (ironclad:make-digest :sha256) string)))

(defun digest-string (digest-spec string)
  "Compute the digest of a STRING, given an Ironclad DIGEST-SPEC."
  (ironclad:digest-sequence
   digest-spec
   (flexi-streams:string-to-octets string :external-format 'utf-8)))



