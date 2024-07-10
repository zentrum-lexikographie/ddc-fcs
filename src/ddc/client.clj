(ns ddc.client
  (:require
   [charred.api :as charred]
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   [ddc.env :as env]
   [hato.client :as hc]
   [taoensso.timbre :as log]
   [virtuoso.core :as v]
   [clojure.java.io :as io])
  (:import
   (java.io DataInputStream InputStream OutputStream)
   (java.net Socket)
   (java.nio ByteBuffer ByteOrder)
   (java.nio.charset Charset)))

(def ^Charset charset
  (Charset/forName "UTF-8"))

(defn write-str
  [^OutputStream out ^String s]
  (let [payload (. s (getBytes charset))
        len     (count payload)]
    (. out (write (.. (ByteBuffer/allocate 4)
                      (order ByteOrder/LITTLE_ENDIAN)
                      (putInt (unchecked-int len))
                      (array))))
    (. out (write payload))
    (. out (flush))))

(defn read-str
  [^InputStream in]
  (let [in  (DataInputStream. in)
        len (byte-array 4)]
    (.readFully in len)
    (let [len (.. (ByteBuffer/wrap len)
                  (order ByteOrder/LITTLE_ENDIAN)
                  (getInt))
          s   (byte-array len)]
      (.readFully in s)
      (String. s charset))))

(defn request
  [[host port :as endpoint] cmd]
  (try
    (log/tracef "? [%15s %5d/tcp] '%s'" host port cmd)
    (with-open [socket (Socket. ^String host (int port))
                output (.getOutputStream socket)
                input  (.getInputStream socket)]
      (write-str output cmd)
      (let [result (read-str input)]
        (log/tracef ". [%15s %5d/tcp -> %,15d chars] '%s'"
                    host port (count result) cmd)
        (charred/read-json result)))
    (catch Throwable t
      (throw (ex-info "DDC request error" {:endpoint endpoint :cmd cmd} t)))))

(defn query
  [endpoint q & {:keys [offset page-size timeout]
                 :or   {offset 0 page-size 1000 timeout 30}}]
  (assert (not-empty q) "No query (q) given")
  (let [cmd      (->> (str/join " " [offset page-size timeout])
                      (vector "run_query Distributed" q "json")
                      (str/join \))
        response (request endpoint cmd)
        total    (get response "nhits_" 0)
        results  (get response "hits_")]
    (when (seq results)
      (lazy-cat
       (let [meta {:endpoint endpoint :total total}]
         (map-indexed #(with-meta %2 (assoc meta :offset (+ offset %1))) results))
       (let [offset (+ offset (count results))]
         (when (< offset total)
           (query endpoint q
                  :offset offset
                  :page-size page-size
                  :timeout timeout)))))))

(comment
  (request ["data.dwds.de" 52170] "info")
  (take 1 (query ["tuvok.bbaw.de" 60260] "Pudel" :page-size 1))
  (take 2 (query ["data.dwds.de" 52170] "Hochkaräter #CNTXT 1" :page-size 2)))

(def metadata-fields
  [:pid :url :title-de :title-en :author-de :author-en :desc-de :desc-en])

(def metadata
  (with-open [r (io/reader (io/resource "corpus-metadata.csv"))]
    (->> (csv/read-csv r)
         (drop 1)
         (map (fn [[corpus & metadata]] [corpus (zipmap metadata-fields metadata)]))
         (into (sorted-map)))))

(def endpoints
  (as-> (hc/request env/dstar-endpoint-request) $
    (charred/read-json (get $ :body) :key-fn keyword)
    (into
     (sorted-map)
     (mapcat (fn [{:keys [host port corpus]}]
               (when (metadata corpus)
                 (list [corpus [host (parse-long port)]]))))
     $)))

(log/infof "Retrieved %d endpoint(s) from %s"
           (count endpoints)
           (env/dstar-endpoint-request :url))

(def corpora
  (into (sorted-map)
        (mapv vector
              (keys endpoints)
              (v/pmap! #(request % "info")
                       (vals endpoints)))))

(def indices
  (into (sorted-map)
        (mapv vector
              (keys corpora)
              (for [info (vals corpora)]
                (->> (tree-seq #(% "corpora") #(% "corpora") info)
                     (mapcat #(map (fn [{k "shortname"}] k) (% "indices")))
                     (remove nil?)
                     (into (sorted-set)))))))

(defn corpus-tokens
  [corpus-info]
  (->>
   corpus-info
   (tree-seq #(get % "corpora") #(get % "corpora"))
   (filter #(get % "indexed"))
   (map #(get % "ntokens"))
   (reduce +)))

(doseq [[corpus corpus-info] corpora :let [[host port] (endpoints corpus)]]
  (log/infof "DDC endpoint %s @ %s:%d – %,d token(s)"
             corpus host port (corpus-tokens corpus-info)))

(comment
  (indices "dta"))
