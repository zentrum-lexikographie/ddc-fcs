;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((clojure-mode . ((cider-preferred-build-tool . clojure-cli)
                  (cider-clojure-cli-aliases . ":dev:test")
                  (cider-ns-refresh-before-fn . "user/halt")
                  (cider-ns-refresh-after-fn . "user/go"))))
