(defsystem "activitypub-servist"
  :depends-on ("alexandria" "clack" "dexador" "inferior-shell" "ironclad" "local-time"  "purl" "str" "webtentacle" "yason")
  :components ((:file "src/activitypub-servist")))

;; (ql:quickload '(alexandria clack dexador inferior-shell ironclad local-time  purl str webtentacle yason))
