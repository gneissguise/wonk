(ns wonk.core
  (:require [clj-kondo.impl.utils :as util]
            [clj-kondo.impl.rewrite-clj.node.seq :as node-seq]
            [clj-kondo.impl.analyzer :as analyze]
            [clj-kondo.impl.hooks :as hooks]
            [clara.rules :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))

(defrule jabroni
  "woop woop"
  [:domain/entity [{:keys [foo bar]}]
   (= foo ?foo)
   (= bar :baz)]
  =>
  (println "This happened with" ?foo))


(def my-node (util/parse-string "(defrule ^:meta ^:tag jabroni
  \"woop woop\"
  [:domain/entity [{:keys [foo bar]}]
   (= foo ?foo)
   (= bar :baz)]
  =>
  (println \"This happened with\" ?foo)
  (do :more))
"))

(:children my-node)

(defn node-type? [t node]
  (= (type node) t))
(def seq-node?
  (partial node-type? clj_kondo.impl.rewrite_clj.node.seq.SeqNode))
(def token-node?
  (partial node-type? clj_kondo.impl.rewrite_clj.node.token.TokenNode))
(def string-node?
  (partial node-type? clj_kondo.impl.rewrite_clj.node.string.StringNode))

(defn vector-node? [n]
  (and (seq-node? n) (= (:tag n) :vector)))

(defn list-node? [n]
  (and (seq-node? n) (= (:tag n) :list)))

(defn pred-conj [p coll n]
  (if p (conj coll n) coll))
(defn filter-seq-node [nodes]
  (loop [[n & more] nodes
         rtn []]
    (let [p (seq-node? n)
          rtn (pred-conj p rtn n)]
      (if more
        (recur more rtn)
        rtn))))

#_(clojure.pprint/pprint (filter-seq-node (:children my-node)))
(defn =>? [n]
  (and (token-node? n)
       (= (:string-value n) "=>")))
(defn validate-rule [node]
  (let [invalid-nodes #(or (token-node? %) (string-node? %))
        children (drop-while invalid-nodes (:children my-node))
        not=>? #(not (=>? %))
        lhs (take-while not=>? children)
        rhs (->> children
                 (drop-while not=>?)
                 (remove =>?))]
    (prn lhs)
    (if (and (> (count lhs) 0) (every? vector-node? lhs)) (print " Valid LHS!") (print " Invalid LHS!"))
    (println "\n=>\n")
    (prn rhs)
    (if (and (> (count rhs) 0) (every? list-node? rhs)) (print " Valid RHS!") (print " Invalid RHS!"))))

(validate-rule my-node)