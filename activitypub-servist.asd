(defsystem "activitypub-servist"
  :depends-on ("alexandria" "clack" "dexador" "inferior-shell" "ironclad" "local-time"  "purl" "str" "webtentacle" "yason")
  :components ((:file "activitypub-servist")))

;; (ql:quickload '(alexandria clack dexador inferior-shell ironclad local-time  purl str webtentacle yason))
