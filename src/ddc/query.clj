(ns ddc.query
  (:require
   [clojure.string :as str])
  (:import
   (eu.clarin.sru.server CQLQueryParser$CQLQuery SearchTermsQueryParser$SearchTermsQuery SRUConstants)
   (eu.clarin.sru.server.fcs Constants FCSQueryParser$FCSQuery)
   (eu.clarin.sru.server.fcs.parser Expression ExpressionAnd ExpressionGroup ExpressionNot ExpressionOr ExpressionWildcard Operator QueryDisjunction QueryGroup QueryNode QuerySegment QuerySequence QueryWithWithin RegexFlag SimpleWithin SimpleWithin$Scope)
   (org.z3950.zing.cql CQLAndNode CQLNode CQLNotNode CQLOrNode CQLProxNode CQLRelation CQLTermNode)))

(defn sru-diagnostic
  [uri message details]
  (ex-info message {:sru-diagnostic-uri uri
                    :sru-diagnostic-details details}))

(defn fcs-query-unsupported
  [details]
  (sru-diagnostic
   Constants/FCS_DIAGNOSTIC_GENERAL_QUERY_TOO_COMPLEX_CANNOT_PERFORM_QUERY
   "Query too complex/ cannot perform query"
   details))

(defn ddc-escape-term
  "DDC terms/phrase are single/double-quoted, so we need to escape quotes."
  [s]
  (-> s
      (str/replace #"\\" "\\\\\\\\")
      (str/replace #"['\"]" "\\\\$0")))

(letfn [(cql-modifier [type modifiers]
          (when-let [modifier (first (filter #(= type (. % (getType))) modifiers))]
            (list (. modifier (getComparison)) (. modifier (getValue)))))]
  (defn cql-modifiers->distance
    [modifiers]
    (let [[ucmp uval]     (cql-modifier "unit" modifiers)
          [dcmp dval]     (cql-modifier "distance" modifiers)
          unit-valid?     (or (and (nil? ucmp) (nil? uval))
                              (and (#{"=" "=="} ucmp)
                                   (#{"any" "word" "token"} uval)))
          distance-valid? (or
                           (and (nil? dcmp) (nil? dval))
                           (and (#{"=" "=="} dcmp) (= "0" dval))
                           (and (#{"<" "<="} dcmp) (re-seq #"[0-9]+" dval)))
          distance        (if (and distance-valid? dval) (parse-long dval) 0)]
      (when-not distance-valid?
        (throw (ex-info "prox/distance invalid" {:cmp dcmp :value dval})))
      (when-not unit-valid?
        (throw (ex-info "prox/unit invalid" {:cmp ucmp :value uval})))
      (if (and (< 0 distance) (= "<" dcmp)) (dec distance) distance))))

(defprotocol Translation
  (->ddc [this]))

(extend-protocol Translation

  SearchTermsQueryParser$SearchTermsQuery
  (->ddc [query]
    (let [terms (. query (getParsedQuery))]
      (str/join " && " (map (comp #(str "'" % "'") ddc-escape-term) terms))))

  CQLQueryParser$CQLQuery
  (->ddc [query] (->ddc (. query (getParsedQuery))))

  FCSQueryParser$FCSQuery
  (->ddc [query] (->ddc (. query (getParsedQuery))))

  CQLRelation
  (->ddc [rel]
    (condp = (. rel (getBase))
      "=="    "@"
      "exact" "@"
      "="     ""
      "scr"   ""
      (throw (sru-diagnostic SRUConstants/SRU_UNSUPPORTED_RELATION
                             "Unsupported relation"
                             (str rel)))))

  CQLTermNode
  (->ddc [node]
    (let [index (. node (getIndex))]
      (when-not (#{"cql.serverChoice" "fcs.words" "words"} index)
        (throw (sru-diagnostic
                SRUConstants/SRU_UNSUPPORTED_INDEX
                "Unsupported index"
                (str index)))))
    (let [relation (->ddc (. node (getRelation)))
          terms    (str/split (. node (getTerm)) #"\s+")
          terms    (map (comp #(str relation "'" % "'") ddc-escape-term) terms)
          phrase   (if (second terms)
                   (str "\"" (str/join " " terms) "\"")
                   (first terms))]
      phrase))

  CQLAndNode
  (->ddc [node]
    (format "( %s && %s)"
            (->ddc (. node (getLeftOperand)))
            (->ddc (. node (getRightOperand)))))

  CQLOrNode
  (->ddc [node]
    (format "( %s || %s)"
            (->ddc (. node (getLeftOperand)))
            (->ddc (. node (getRightOperand)))))

  CQLNotNode
  (->ddc [node]
    (format "( %s && !%s)"
            (->ddc (. node (getLeftOperand)))
            (->ddc (. node (getRightOperand)))))

  CQLProxNode
  (->ddc [node]
    (format "NEAR(%s, %s, %s)"
            (->ddc (. node (getLeftOperand)))
            (->ddc (. node (getRightOperand)))
            (cql-modifiers->distance (.. node (getModifiers)))))

  CQLNode
  (->ddc [node]
    (throw (sru-diagnostic SRUConstants/SRU_QUERY_FEATURE_UNSUPPORTED
                           "Query feature unsupported"
                           (str node))))

  Expression
  (->ddc [node]
    (let [qualifier     (or (. node (getLayerQualifier)) "")
          identifier    (. node (getLayerIdentifier))
          [field expan] (cond
                          (#{"word" "token"} identifier) ["$w"]
                          (#{"norm" "orth"} identifier)  ["$v"]
                          (#{"surface"} identifier)      ["$u"]
                          (#{"lemma"} identifier)        ["$l"]
                          ;; (un-)qualified text attribute, e.g. `text`, `word.text`
                          (#{"text"} identifier)
                          (cond
                            (#{"surface" "utf8"} qualifier)          ["$u"]
                            (#{"word" "latin1" ""} qualifier)        ["$w"]
                            (#{"norm" "orth" "canonical"} qualifier) ["$v"])
                          ;; (un-)qualified POS attribute, e.g. `pos`, `stts.pos`
                          (#{"pos"} identifier)
                          (cond
                            (#{"stts"} qualifier)  ["$p"]
                            (#{"ud" ""} qualifier) ["$p" "pos-ud"]))
          pattern       (. node (getRegexValue))
          regex-flags   (into #{} (. node (getRegexFlags)))
          literal?      (and (empty? regex-flags)
                           (re-matches #"^[^^$\\.?*+{}()\[\]|]*$" pattern))]
      (when (and expan (not literal?))
        (throw (fcs-query-unsupported "DDC-expanded regex")))
      (format
       (condp = (. node (getOperator))
         Operator/EQUALS "%s=%s%s"
         Operator/NOT_EQUALS "!%s=%s%s")
       field
       (if literal?
         (str "'" (ddc-escape-term pattern) "'")
         (let [flags (cond-> "g"
                       (regex-flags RegexFlag/IGNORE_DIACRITICS) (str "d")
                       (regex-flags RegexFlag/CASE_INSENSITIVE)  (str "i"))]
           (as-> pattern $
             (cond->> $
               (regex-flags RegexFlag/LITERAL_MATCHING) (format "\\Q%s\\E"))
             (str/replace $ #"/" "\\\\/")
             (str/replace $ #"\\u([0-9A-Fa-f]{1,4})" "\\\\x{$1}")
             (str/replace $ #"\\U([0-9A-Fa-f]{1,8})" "\\\\x{$1}")
             (str "/" $ "/" flags))))
       (cond-> "" expan (str "|" expan)))))

  ExpressionAnd
  (->ddc [node]
    (str "(" (str/join " &= " (map ->ddc (. node (getChildren)))) ")"))

  ExpressionOr
  (->ddc [node]
    (str "(" (str/join " |= " (map ->ddc (. node (getChildren)))) ")"))

  ExpressionNot
  (->ddc [_node]
    (throw (fcs-query-unsupported "Negation not supported")))

  ExpressionGroup
  (->ddc [_node]
    (throw (fcs-query-unsupported "Group queries not supported")))

  ExpressionWildcard
  (->ddc [_node] "*")

  QueryDisjunction
  (->ddc [node]
    (str/join " || " (map ->ddc (. node (getChildren)))))

  QueryGroup
  (->ddc [_node]
    (throw (fcs-query-unsupported "Grouped/quantified queries not supported")))

  QuerySegment
  (->ddc [node]
    (let [expression (. node (getExpression))
          min-occurs (min 32 (max 0 (. node (getMinOccurs))))
          max-occurs (min 32 (. node (getMaxOccurs)))
          max-occurs (if (= QueryNode/OCCURS_UNBOUNDED max-occurs) 32 max-occurs)]
      (cond
        ;; not quantified
        (and (= 1 min-occurs) (= 1 max-occurs))
        (->ddc expression)
        ;; quantified wildcard
        (instance? ExpressionWildcard expression)
        (->>
         (cond->> (repeat min-occurs "*")
           (< min-occurs max-occurs)
           (concat (list (str "#" (- max-occurs min-occurs)))))
         (str/join " " ))
        ;; unsupported
        :else
        (throw (fcs-query-unsupported
                "Quantifiers only supported for wildcard segments")))))

  QuerySequence
  (->ddc [node]
    (str "\"" (str/join " " (map ->ddc (. node (getChildren)))) "\""))

  QueryWithWithin
  (->ddc [node]
    (let [scope (. node (getWithin))]
      (when-not (instance? SimpleWithin scope)
        (throw (fcs-query-unsupported "Only simple query scopes supported")))
      (str (->ddc (. node (getQuery))) " WITHIN " (->ddc scope))))

  SimpleWithin
  (->ddc [node]
    (condp = (. node (getScope))
      SimpleWithin$Scope/PARAGRAPH "p"
      SimpleWithin$Scope/SENTENCE  "s"
      SimpleWithin$Scope/SESSION   "file"
      SimpleWithin$Scope/TEXT      "file"
      SimpleWithin$Scope/TURN      "file"
      SimpleWithin$Scope/UTTERANCE "u")))
