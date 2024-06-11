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


(asdf:defsystem "activitypub-servist/signatures"
  :version "0.0"
  :license "AGPLv3"
  :description "AP-S subpackage for handling HTTP signatures."
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/activitypub-servist"

  :depends-on ("cl-base64" "inferior-shell" "ironclad" "str")
  :components ((:file "src/signatures")))
