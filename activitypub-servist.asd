(defsystem "activitypub-servist"
    :depends-on ("activitypub-servist/signatures"
                 "alexandria" "clack" "dexador"
                 "local-time"  "purl" "str" "webtentacle" "yason")
    :components ((:file "src/activitypub-servist")))

(defsystem "activitypub-servist/signatures"
    :depends-on ("cl-base64" "inferior-shell" "ironclad" "str")
    :components ((:file "src/signatures")))
