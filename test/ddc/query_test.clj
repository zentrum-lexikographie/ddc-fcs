(ns ddc.query-test
  (:require [ddc.query :as query]
            [clojure.test :refer [deftest is testing]])
  (:import (org.z3950.zing.cql CQLParser)
           (eu.clarin.sru.server.fcs.parser QueryParser)))

(defn parse-cql
  [s]
  (let [parser (CQLParser. CQLParser/V1POINT2)]
    (. parser (parse s))))

(defn cql-parses
  [cql-q]
  (is (some? (parse-cql cql-q))))

(defn cql->ddc
  [cql-q]
  (-> cql-q parse-cql query/->ddc))

(defn cql-is-ddc?
  [cql-q ddc-q]
  (is (= ddc-q (cql->ddc cql-q))))

(defn cql->ddc-fails?
  [cql-q]
  (is (thrown? Exception (-> cql-q parse-cql query/->ddc))))

(deftest cql-parsing
  (testing "CQL parsing and DDC conversion"
    (testing "Grammar"
      (cql-parses "fish")
      (cql-parses "dc.title any test")
      (cql-parses "dc.title any fish or dc.creator any sanderson")
      (cql-parses "dc.title any fish sortBy dc.date/sort.ascending")
      (cql-parses "cql.serverChoice = fish")
      (cql-parses "dc.title cql.any fish")
      (cql-parses "dc.title any/rel.algorithm=cori fish")
      (cql-parses
       "> dc = \"info:srw/context-sets/1/dc-v1.1\" dc.title any fish")
      (cql-parses
       "dc.title any fish or (dc.creator any yi and dc.identifier = \"id:123\")")
      (cql-parses
       "dc.title any fish or/rel.combine=sum dc.creator any sanderson")
      (cql-parses
       "dc.title any fish prox/unit=word/distance>3 dc.title any squirrel"))
    (testing "Expressions"
      (cql-is-ddc? "testen" "'testen'")
      (cql-is-ddc? "\"er hat\"" "\"'er' 'hat'\"")
      (cql-is-ddc? "er prox/distance<3/unit=word hat" "NEAR('er', 'hat', 2)")
      (cql->ddc-fails? "text == testen"))))

(defn parse-fcs
  [fcs-q]
  (. (QueryParser.) (parse fcs-q)))

(defn fcs-parses
  [fcs-q]
  (is (some? (parse-fcs fcs-q))))

(defn fcs->ddc
  [fcs-q]
  (-> fcs-q parse-fcs query/->ddc))

(defn fcs-is-ddc?
  [fcs-q ddc-q]
  (is (= ddc-q (fcs->ddc fcs-q))))

(deftest fcs-ql-parsing
  (testing "FCS-QL parsing and DDC conversion"
    (testing "Grammar"
      (fcs-parses "\"walking\"")
      (fcs-parses "[token = \"walking\"]")
      (fcs-parses "\"Dog\" /c")
      (fcs-parses "[pos = \"NOUN\"]")
      (fcs-parses "[pos != \"NOUN\"]")
      (fcs-parses "[lemma = \"walk\"]")
      (fcs-parses "\"blaue|grüne\" [pos = \"NOUN\"]")
      (fcs-parses "\"dogs\" []{3,} \"cats\" within s")
      (fcs-parses "[z:pos = \"ADJ\"]")
      (fcs-parses "[z:pos = \"ADJ\" & q:pos = \"ADJ\"]")
      (fcs-parses "[pos = 'ADJ']{1,2} [text = 'query'] | 'term' within s")
      (fcs-parses "[pos = 'ADJ']{1,2} [] 'testen' within s"))
    (testing "Expressions"
      (fcs-is-ddc? "[pos=\"NN\"]"
                   "$p='NN'|pos-ud")
      (fcs-is-ddc? "[lemma=\"test\"]"
                   "$l='test'")
      (fcs-is-ddc? "[word=\"lopen\"] [word =\"op\" & pos=\"ADP\"]"
                   "\"$w='lopen' ($w='op' &= $p='ADP'|pos-ud)\""))))
