(require "asdf")


(asdf:defsystem "activity-servist"
  :version "0.0"
  :license "AGPLv3"
  :description "ActitivyPub federated server framework."
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/activity-servist"

  :in-order-to ((test-op (test-op "activitypub/tests")))
  :depends-on ("activity-servist/signatures"
               "alexandria" "clack" "dexador"
               "local-time"  "purl" "str" "webtentacle" "yason")
  :components ((:file "src/activity-servist")))


(asdf:defsystem "activity-servist/activity-vocabulary"
  :version "0.0"
  :license "AGPLv3"
  :description "A-S subpackage containing ActivityVocabulary class-definitions."
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/activity-servist"

  :depends-on ("activity-servist/json-ld")
  :components ((:file "src/activity-vocabulary")))


(asdf:defsystem "activity-servist/json-ld"
  :version "0.0"
  :license "AGPLv3"
  :description "A fragile and meek JSON-LD parser and encoder."
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/activity-servist"

  :depends-on ("alexandria" "dexador" "str" "yason")
  :components ((:file "src/json-ld")))


(asdf:defsystem "activity-servist/signatures"
  :version "0.0"
  :license "AGPLv3"
  :description "A-S subpackage for handling HTTP signatures."
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/activity-servist"

  :depends-on ("cl-base64" "flexi-streams" "inferior-shell" "ironclad" "str")
  :components ((:file "src/signatures")))



;;; Tests
;;; —————————————————————————————————————
(asdf:defsystem "activity-servist/tests/json-ld"
  :version "0.0"
  :license "AGPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :description "Tests for the the activity-servist/json-ld package."

  :depends-on (:activity-servist/json-ld :alexandria :lisp-unit2)
  :components ((:file "t/json-ld")))


(asdf:defsystem "activity-servist/tests/activity-vocabulary"
  :version "0.0"
  :license "AGPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :description "Tests for the the activity-servist/activity-vocabulary package."

  :depends-on (:activity-servist/activity-vocabulary :alexandria :lisp-unit2)
  :components ((:file "t/activity-vocabulary")))


(asdf:defsystem "activity-servist/tests/signatures"
  :version "0.0"
  :license "AGPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :description "Tests for the the activity-servist/signatures package."

  :depends-on (:activity-servist/signatures :lisp-unit2)
  :components ((:file "t/signatures")))


(asdf:defsystem "activity-servist/tests"
  :version "0.0"
  :license "AGPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :description "Tests for all activity-servist subpacakges."

  :depends-on (:activity-servist/tests/activity-vocabulary
               :activity-servist/tests/json-ld
               :activity-servist/tests/signatures
               :alexandria :lisp-unit2)
  :components ((:file "t/t")))

;; Following method tweaked from lisp-unit2’s documentation:
;; https://github.com/AccelerationNet/lisp-unit2/blob/master/README.md#asdf
(defmacro define-asdf-testing (package)
  `(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system ',package))))
     (eval (read-from-string (format nil "(~A:run-with-summary)" ',package)))))

(define-asdf-testing activity-servist/tests/activity-vocabulary)
(define-asdf-testing activity-servist/tests/json-ld)
(define-asdf-testing activity-servist/tests/signatures)
(define-asdf-testing activity-servist/tests)
