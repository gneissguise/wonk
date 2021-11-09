(ns hooks.clara-macros
  (:require [clara.rules :refer :all]
            [clj-kondo.impl.analyzer :as analyze]
            [clj-kondo.impl.hooks :as hooks]
            [clj-kondo.impl.rewrite-clj.node.seq :as node-seq]
            [clj-kondo.impl.utils :as util]))

;; Node type / Helper fns
(defn node-type? [t node]
  (= (type node) t))

(def seq-node?
  (partial node-type? clj_kondo.impl.rewrite_clj.node.seq.SeqNode))

(def token-node?
  (partial node-type? clj_kondo.impl.rewrite_clj.node.token.TokenNode))

(def string-node?
  (partial node-type? clj_kondo.impl.rewrite_clj.node.string.StringNode))

(defn tag? [t node]
  (= (:tag node) t))

(def vector-tag?
  (partial tag? :vector))

(def list-tag?
  (partial tag? :list))

(defn vector-node? [n]
  ((every-pred seq-node? vector-tag?) n))

(defn list-node? [n]
  ((every-pred seq-node? list-tag?) n))

(defn string-value? [s node]
  (= :string-value node) s)

(defn =>? [n]
  ((every-pred token-node? (partial string-value? "=>")) n))

(def not=>? (complement =>?))

(defn predicated-conj [pred coll n]
  (if pred
    (conj coll n)
    coll))

(defn defrule [{:keys [:node]}]
  (let [nonessential (some-fn token-node? string-node?)
        children (drop-while nonessential (:children node))
        lhs-nodes (take-while not=>? children)
        rhs-nodes (->> children
                       (drop-while not=>?)
                       (remove =>?))
        validate #(when-not %1 (throw (ex-info (:message %2) %2)))]
    ;;; Conditions ===
    ;;;
    ;;; (defrule) macro validation
    (validate (pos? (count lhs-nodes))
              {:sexp :defrule
               :validator :lhs-missing-condition
               :message "defrule lhs needs at least one condition defined."})
    (validate (pos? (count rhs-nodes))
              {:sexp :defrule
               :validator :rhs-missing-action
               :message "defrule rhs needs at least one action defined."})
    (validate (every? vector-node? lhs-nodes)
              {:sexp :defrule
               :validator :lhs-invalid-condition-format
               :message "Invalid formatting of one or more conditions within the defrule's lhs."})
    (validate (every? list-node? rhs-nodes)
              {:sexp :defrule
               :validator :rhs-invalid-action
               :message "One or more actions are improperly defined within the defrule's rhs."})
    ;;; Return new node ===
    (let [new-node (util/list-node
                    (list*
                     (util/token-node 'let)
                     (util/vector-node )))])))