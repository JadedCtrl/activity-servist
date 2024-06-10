;;;; activitypub-servist/signatures: Handle AP-compatible HTTP signatures.

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

(defpackage #:activitypub-servist/signatures
  (:use #:cl)
  (:export :openssl-shell-generate-key-pair
           :openssl-shell-sign-string :openssl-shell-import-key-pair
           :digest-string :string-sha256sum))

(in-package #:activitypub-servist/signatures)


;;; Key creation/parsing
;;; ————————————————————————————————————————
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



;;; Signing
;;; ————————————————————————————————————————
(defun openssl-shell-sign-string (private-pem-string string)
  "Use the OpenSSL binary on the host system to RSS-SHA256 sign a STRING with a
private key."
  (alexandria:write-string-into-file private-pem-string #p"/tmp/private.pem" :if-does-not-exist :create :if-exists :overwrite)
  (apply #'str:concat
         (inferior-shell:run/lines
          `(inferior-shell:pipe
            (printf ,string)
            (openssl dgst -sha256 -sign /tmp/private.pem -)
            (base64)))))



;;; Misc.
;;; ————————————————————————————————————————
(defun digest-string (digest-spec string)
  "Compute the digest of a STRING, given an Ironclad DIGEST-SPEC."
  (ironclad:digest-sequence digest-spec (string-to-ub8-vector string)))

(defun string-sha256sum (string)
  "Compute the sha256 checksum of a STRING, in hexadecimal string-format."
  (base64:usb8-array-to-base64-string
   (digest-string (ironclad:make-digest :sha256) string)))

(defun string-to-ub8-vector (string)
  "Convert the given STRING into an unsigned 8-bit vector."
  (coerce
   (loop for char across string
         collect (char-code char))
   '(vector (unsigned-byte 8))))
