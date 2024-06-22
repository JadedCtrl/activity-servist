(require "asdf")


(asdf:defsystem "activitypub-servist"
  :version "0.0"
  :license "AGPLv3"
  :description "ActitivyPub federated server framework."
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/activitypub-servist"

  :depends-on ("activitypub-servist/signatures"
               "alexandria" "clack" "dexador"
               "local-time"  "purl" "str" "webtentacle" "yason")
  :components ((:file "src/activitypub-servist")))


(asdf:defsystem "activitypub-servist/activity-vocabulary"
  :version "0.0"
  :license "AGPLv3"
  :description "AP-S subpackage for handling ActivityVocabulary parsing/encoding."
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/activitypub-servist"

  :depends-on ("alexandria" "closer-mop" "str" "yason")
  :components ((:file "src/activity-vocabulary")))


(asdf:defsystem "activitypub-servist/signatures"
  :version "0.0"
  :license "AGPLv3"
  :description "AP-S subpackage for handling HTTP signatures."
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/activitypub-servist"

  :depends-on ("cl-base64" "flexi-streams" "inferior-shell" "ironclad" "str")
  :components ((:file "src/signatures")))



;;; Tests
;;; —————————————————————————————————————
(asdf:defsystem "activitypub-servist/tests/activity-vocabulary"
  :version "0.0"
  :license "AGPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :description "Tests for the the activitypub-servist/signatures package."

  :depends-on (:activitypub-servist/activity-vocabulary :alexandria :lisp-unit2)
  :components ((:file "t/activity-vocabulary")))


(asdf:defsystem "activitypub-servist/tests/signatures"
  :version "0.0"
  :license "AGPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :description "Tests for the the activitypub-servist/signatures package."

  :depends-on (:activitypub-servist/signatures :lisp-unit2)
  :components ((:file "t/signatures")))


;; Following method tweaked from lisp-unit2’s documentation:
;; https://github.com/AccelerationNet/lisp-unit2/blob/master/README.md#asdf
(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :activitypub-servist/tests/activity-vocabulary))))
  (eval (read-from-string "(activtiypub-servist/tests/activity-vocabulary:run)")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :activitypub-servist/tests/signatures))))
  (eval (read-from-string "(activtiypub-servist/tests/signatures:run)")))
