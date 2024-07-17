(ns ddc.fcs
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [ddc.client :as client]
   [ddc.env :as env]
   [ddc.query :as query]
   [gremid.xml :as gx]
   [lambdaisland.uri :as uri]
   [taoensso.timbre :as log])
  (:import
   (eu.clarin.sru.server SRUConstants SRUException SRUQueryParserRegistry$Builder SRUResultCountPrecision SRUSearchResultSet SRUServer SRUServerConfig)
   (eu.clarin.sru.server.fcs DataView DataView$DeliveryPolicy Layer Layer$ContentEncoding ResourceInfo SimpleEndpointSearchEngineBase XMLStreamWriterHelper)
   (eu.clarin.sru.server.fcs.utils SimpleEndpointDescription)
   (java.net URI)
   (javax.servlet.http HttpServlet)
   (org.eclipse.jetty.ee8.servlet ServletContextHandler ServletHolder)
   (org.eclipse.jetty.server Server ServerConnector)))

(log/handle-uncaught-jvm-exceptions!)
(log/merge-config!
 {:min-level [["eu.clarin.sru.server.SRUServer" :warn]
              ["ddc.fcs" (if env/debug? :debug :info)]
              ["*" :info]]
  :appenders {:println (log/println-appender {:stream :std-err})}})

(require '[ddc.client :as client])

(def record-schema-identifier
  "http://clarin.eu/fcs/resource")

(defn corpus->endpoint-config
  [corpus]
  (let [metadata (client/metadata corpus)]
    [:endpoint-config
     {:xmlns "http://www.clarin.eu/sru-server/1.0/"}
     [:databaseInfo
      [:title {:xml:lang "de"} (metadata :title-de)]
      [:title {:xml:lang "en" :primary "true"} (metadata :title-en)]
      [:description {:xml:lang "de"} (metadata :desc-de)]
      [:description {:xml:lang "en" :primary "true"} (metadata :desc-en)]
      [:author {:xml:lang "de"} (metadata :author-de)]
      [:author {:xml:lang "en" :primary "true"} (metadata :author-en)]]
     [:indexInfo
      [:set {:name "fcs" :identifier "http://clarin.eu/fcs/resource"}
       [:title {:xml:lang "en" :primary "true"} "CLARIN Content Search"]]
      [:index {:search "true" :scan "false" :sort "false"}
       [:title {:xml:lang "en" :primary "true"} "Words"]
       [:map {:primary "true"}
        [:name {:set "fcs"} "words"]]]]
     [:schemaInfo
      [:schema {:identifier record-schema-identifier
                :name       "fcs"
                :sort       "false"
                :retrieve   "true"}
       [:title {:xml:lang "en" :primary "true"} "CLARIN Content Search"]]]]))

(def capabilities
  [(URI. "http://clarin.eu/fcs/capability/basic-search")
   (URI. "http://clarin.eu/fcs/capability/advanced-search")])

(def all-data-views
  [(DataView. "hits" "application/x-clarin-fcs-hits+xml"
              DataView$DeliveryPolicy/SEND_BY_DEFAULT)
   (DataView. "adv" "application/x-clarin-fcs-adv+xml"
              DataView$DeliveryPolicy/SEND_BY_DEFAULT)])

(def data-layer->index
  {"text"    "w"
   "lemma"   "l"
   "pos"     "p"
   "norm"    "v"
   "surface" "u"})

(def data-layer->uri
  (into {}
        (map (juxt identity #(str "http://dwds.de/ns/fcs/layer/" %)))
        (keys data-layer->index)))

(defn data-layer
  [id]
  (Layer. id (URI. (data-layer->uri id)) id Layer$ContentEncoding/EMPTY
          nil nil nil))

(def all-data-layers
  (into [] (map data-layer) (keys data-layer->index)))

(defn corpus->resource-info
  [corpus]
  (let [metadata (client/metadata corpus)
        indices  (client/indices corpus)
        layers   (into []
                       (filter #(indices (data-layer->index (. % (getId)))))
                       all-data-layers)]
    (ResourceInfo.
     (metadata :pid)
     {"de" (metadata :title-de) "en" (metadata :title-en)}
     {"de" (metadata :desc-de) "en" (metadata :desc-en)}
     nil
     (metadata :url) ["deu"]
     all-data-views layers nil)))

(defn endpoint-desc
  [corpus]
  (SimpleEndpointDescription.
   2
   capabilities
   all-data-views
   all-data-layers
   [(corpus->resource-info corpus)]
   true))

(def default-num-records
  25)

(def maximum-records
  250)

(defn sru-server-config
  [corpus]
  (let [config   (corpus->endpoint-config corpus)
        config-f (fs/create-temp-file)
        path     (str env/http-context-path "/" corpus)]
    (try
      (with-open [output (io/output-stream (fs/file config-f))]
        (->> config gx/sexp->node gx/node->events (gx/write-events output)))
      (SRUServerConfig/parse
       {SRUServerConfig/SRU_SUPPORTED_VERSION_MAX          "2.0"
        SRUServerConfig/SRU_TRANSPORT                      env/http-protocol
        SRUServerConfig/SRU_HOST                           env/http-host
        SRUServerConfig/SRU_PORT                           (str env/http-port)
        SRUServerConfig/SRU_DATABASE                       path
        SRUServerConfig/SRU_MAXIMUM_RECORDS                (str maximum-records)
        SRUServerConfig/SRU_ALLOW_OVERRIDE_MAXIMUM_RECORDS (str false)
        SRUServerConfig/SRU_NUMBER_OF_RECORDS              (str default-num-records)}
       (.. config-f (toUri) (toURL)))
      (finally
        (fs/delete config-f)))))

(defn assoc-space-after
  [[t1 {ws "ws" :as _t2}]]
  (assoc t1 "ws" (or ws "0")))

(defn parse-tokens
  [indices tokens]
  (let [tokens (map #(into {} (map vector indices %)) tokens)]
    (into [] (map assoc-space-after) (partition-all 2 1 tokens))))

(def hits-ns
  "http://clarin.eu/fcs/dataview/hits")

(def hits-mime-type
  "application/x-clarin-fcs-hits+xml")

(def adv-ns
  "http://clarin.eu/fcs/dataview/advanced")

(def adv-mime-type
  "application/x-clarin-fcs-adv+xml")

(defn result->xml
  [query-type pid frag-ref result writer]
  (let [indices (vec (cons "hl" (get-in result ["meta_" "indices_"])))
        tokens  (parse-tokens indices (second (get result "ctx_")))]
    (doto writer
      (XMLStreamWriterHelper/writeStartResource pid nil)
      (XMLStreamWriterHelper/writeStartResourceFragment nil frag-ref)
      (XMLStreamWriterHelper/writeStartDataView hits-mime-type)
      (. (setPrefix "hits" hits-ns))
      (. (writeStartElement hits-ns "Result"))
      (. (writeNamespace "hits" hits-ns)))
    (doseq [{hl "hl" w "w" ws "ws" :as _token} tokens]
      (if (= hl 1)
        (doto writer
          (. (writeStartElement hits-ns "Hit"))
          (. (writeCharacters w))
          (. (writeEndElement)))
        (. writer (writeCharacters w)))
      (when (not= ws "0")
        (. writer (writeCharacters " "))))
    (doto writer
      (. (writeEndElement))
      (XMLStreamWriterHelper/writeEndDataView))
    (when (= query-type "fcs")
      (doto writer
        (XMLStreamWriterHelper/writeStartDataView adv-mime-type)
        (. (setPrefix "adv" adv-ns))
        (. (writeStartElement adv-ns "Advanced"))
        (. (writeNamespace "adv" adv-ns))
        (. (writeAttribute "unit" "item")))
      (. writer (writeStartElement adv-ns "Segments"))
      (loop [n 1 start 1 tokens tokens]
        (when-let [{w "w" ws "ws"} (first tokens)]
          (let [end (dec (+ start (count w)))]
            (doto writer
              (. (writeEmptyElement adv-ns "Segment"))
              (. (writeAttribute "id" (str "s" n)))
              (. (writeAttribute "start" (str start)))
              (. (writeAttribute "end" (str end))))
            (recur (inc n)
                   (cond-> (inc end) (not= ws "0") inc)
                   (rest tokens)))))
      (. writer (writeEndElement))
      (. writer (writeStartElement adv-ns "Layers"))
      (doseq [[layer index] data-layer->index
              :when         (-> tokens first (get index))]
        (. writer (writeStartElement adv-ns "Layer"))
        (. writer (writeAttribute "id" (str (data-layer->uri layer))))
        (doseq [[i {hl "hl" v index}] (map-indexed list tokens)]
          (. writer (writeStartElement adv-ns "Span"))
          (. writer (writeAttribute "ref" (str "s" (inc i))))
          (when (= hl 1)
            (. writer (writeAttribute "highlight" "h1")))
          (. writer (writeCharacters v))
          (. writer (writeEndElement)))
        (. writer (writeEndElement)))
      (. writer (writeEndElement))
      (doto writer
        (. (writeEndElement))
        (XMLStreamWriterHelper/writeEndDataView)))
    (doto writer
      (XMLStreamWriterHelper/writeEndResourceFragment)
      (XMLStreamWriterHelper/writeEndResource))
    nil))

(defn result-link
  [corpus query]
  (-> (uri/uri "https://www.dwds.de/r/")
      (uri/assoc-query :corpus corpus :q query)
      (str)))

(defn search
  [corpus request diagnostics]
  (let [endpoint     (client/endpoints corpus)
        metadata     (client/metadata corpus)
        pid          (metadata :pid)
        query-type   (. request (getQueryType))
        query        (query/->ddc (. request (getQuery)))
        frag-ref     (result-link corpus query)
        query        (str query " !#has[avail,OR0W] #separate")
        start-record (. request (getStartRecord))
        num-records  (min (. request (getMaximumRecords)) maximum-records)
        results      (->> (client/query endpoint query
                                        :offset (dec start-record)
                                        :page-size (min num-records 1000))
                          (into [] (take num-records)))
        total        (or (some-> results first meta :total) 0)
        num-results  (count results)
        result->xml  (partial result->xml query-type pid frag-ref)
        idx          (volatile! -1)]
    (proxy [SRUSearchResultSet] [diagnostics]
      (getTotalRecordCount [] (min maximum-records total))
      (getRecordCount [] (count results))
      (getResultCountPrecision [] SRUResultCountPrecision/EXACT)
      (getRecordSchemaIdentifier [] record-schema-identifier)
      (nextRecord [] (-> idx deref inc (< num-results)))
      (getRecordIdentifier [] nil)
      (writeRecord [writer] (result->xml (results (vswap! idx inc)) writer))
      (hasExtraRecordData [] false)
      (writeExtraRecordData [writer]))))

(defn throwable->diagnostic
  [t]
  (when-let [uri (some-> t ex-data :sru-diagnostic-uri)]
    (let [message (. t (getMessage))
          details (some-> t ex-data :sru-diagnostic-details)]
      (SRUException. uri details message t))))

(defn search-engine
  [corpus]
  (let [endpoint-desc (endpoint-desc corpus)]
    (proxy [SimpleEndpointSearchEngineBase] []
      (doInit [_context _config _query-parsers _params])
      (createEndpointDescription [_context _config _params] endpoint-desc)
      (search [_config request diagnostics]
        (try
          (search corpus request diagnostics)
          (catch Throwable t
            (throw (or (throwable->diagnostic t)
                       (SRUException.
                        SRUConstants/SRU_GENERAL_SYSTEM_ERROR
                        (. t (getMessage))
                        t)))))))))

(defn sru-servlet
  [corpus]
  (let [server-config (sru-server-config corpus)
        query-parsers (SRUQueryParserRegistry$Builder.)
        engine-params {}
        engine        (search-engine corpus)]
    (. engine (init nil server-config query-parsers engine-params))
    (let [server (SRUServer. server-config (.. query-parsers (build)) nil engine)]
      (proxy [HttpServlet] []
        (doGet  [req resp] (. server (handleRequest req resp)))
        (doPost [req resp] (. server (handleRequest req resp)))))))

(defn stop!
  [^Server server]
  (log/infof "Stopping HTTP service %s" server)
  (.stop server)
  (.join server))

(defn start!
  []
  (log/infof "Starting HTTP service @ %d/tcp" env/http-server-port)
  (let [server    (Server.)
        connector (ServerConnector. server)
        servlets  (ServletContextHandler.)]
    (. connector (setPort env/http-server-port))
    (. servlets  (setContextPath env/http-context-path))
    (doseq [corpus (keys client/corpora) :let [servlet (sru-servlet corpus)]]
      (. servlets (addServlet (ServletHolder. servlet) (str "/" corpus))))
    (. server (addConnector connector))
    (. server (setHandler servlets))
    (. server (start))
    (partial stop! server)))

(defn serve
  [& _]
  (let [stop! (start!)]
    (.. (Runtime/getRuntime) (addShutdownHook (Thread. stop!)))
    @(promise)))
