(ns ddc.env
  (:import
   (io.github.cdimascio.dotenv Dotenv)))

(def ^Dotenv dot-env
  (.. Dotenv (configure) (ignoreIfMissing) (load)))

(defn get-env
  ([k]
   (get-env k nil))
  ([^String k df]
   (let [k (str "DDC_FCS_" k)]
     (or (System/getenv k) (.get dot-env k) df))))

(def debug?
  (some? (not-empty (get-env "DEBUG"))))

(def dstar-endpoint-index
  (get-env "DSTAR_INDEX" "https://ddc.dwds.de/dstar/"))

(def dstar-endpoint-user
  (get-env "DSTAR_USER"))

(def dstar-endpoint-password
  (get-env "DSTAR_PASSWORD"))

(def dstar-endpoint-credentials
  (when (and dstar-endpoint-user dstar-endpoint-password)
    {:basic-auth {:user dstar-endpoint-user
                  :pass dstar-endpoint-password}}))

(def dstar-endpoint-request
  (cond-> {:method       :get
           :url          dstar-endpoint-index
           :query-params {"f" "json"}}
    dstar-endpoint-credentials (-> (update :url str "intern.perl")
                                   (merge dstar-endpoint-credentials))))

(def http-protocol
  (get-env "HTTP_PROTOCOL" "http"))

(def http-host
  (get-env "HTTP_HOST" "localhost"))

(def http-port
  (get-env "HTTP_PORT" "8080"))

(def http-context-path
  (get-env "HTTP_CONTEXT_PATH" ""))

(def http-server-port
  (parse-long (get-env "HTTP_SERVER_PORT" http-port)))
