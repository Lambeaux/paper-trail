(ns lambeaux.paper-trail.impl.util
  (:require [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang Cons]))

;; ----------------------------------------------------------------------------
;; Predicates / Resolvers
;; ----------------------------------------------------------------------------

(def clojure-core-symbols->vars
  (ns-publics (the-ns 'clojure.core)))

(defn core-fn? [obj]
  (contains? clojure-core-symbols->vars obj))

(defn to-namespace
  "Attempts to convert an object to a namespace,
   returning the namespace if successful, or nil
   otherwise. Will convert symbols to namespaces."
  [obj]
  (try
    (the-ns obj)
    (catch Exception _e nil)))

(defn try-resolve
  "Attempts to resolve the provided object to a var.
   Supports full qualifications using the full name
   or an alias."
  [{::pt/keys [current-ns] :as _ctx} obj]
  (cond
    (qualified-symbol? obj) (let [obj-ns (-> obj
                                             namespace
                                             symbol)]
                              (if (to-namespace obj-ns)
                                (resolve obj)
                                (ns-resolve (-> current-ns
                                                ns-aliases
                                                (get obj-ns)
                                                (to-namespace))
                                            (symbol (name obj)))))
    (core-fn? obj) (resolve obj)
    :else (ns-resolve current-ns obj)))

(defn accessible-macro? [ctx obj]
  (try
    (when-let [resolved (try-resolve ctx obj)]
      (boolean (:macro (meta resolved))))
    (catch Exception _e false)))

(defn accessible-fn? [ctx obj]
  (or (keyword? obj)
      (try
        (when-let [resolved (try-resolve ctx obj)]
          (fn? @resolved))
        (catch Exception _e false))))

(defn ->impl [ctx sym]
  (if (keyword? sym)
    sym
    @(try-resolve ctx sym)))

(defn map-impl-fn [ctx]
  #(if (accessible-fn? ctx %) (->impl ctx %) %))

(defn terminal? [obj]
  (or (keyword? obj)
      (number? obj)
      (boolean? obj)
      (nil? obj)
      (char? obj)
      (string? obj)))

;; ----------------------------------------------------------------------------

(def temp-ctx {::pt/current-ns *ns*})

;; - Use symbols to represent literals in a dispatch map ('let, 'try, etc)
;; - Use keywords to represent types in a dispatch map (:list, :vector, :map, etc)
;; - Use multi-methods to arrive at the key when preds are needed??
;; - Use a map to arrive at dispatch keywords??

(defn classify [x]
  (cond (string? x)            :type/string
        (number? x)            :type/number
        (boolean? x)           :type/boolean
        (char? x)              :type/char
        (class? x)             :type/class
        (fn? x)                :type/fn
        (nil? x)               :type/nil
        (simple-keyword? x)    :type/keyword-simple
        (simple-symbol? x)     :type/symbol-simple
        (qualified-keyword? x) :type/keyword-qual
        (qualified-symbol? x)  :type/symbol-qual
        (map? x)               :type/map
        (vector? x)            :type/vector
        (set? x)               :type/set
        (list? x)              (let [op (first x)]
                                 (cond (accessible-macro? temp-ctx op) :type/list-macro
                                       (accessible-fn? temp-ctx op) :type/list-fn
                                       :else :type/list-literal))
        (instance? Cons x)     (let [op (first x)]
                                 (cond (accessible-macro? temp-ctx op) :type/list-macro
                                       (accessible-fn? temp-ctx op) :type/list-fn
                                       :else :type/cons))
        (seq? x)               :type/seq
        :else (throw (ex-info (str "Type " (type x) " is unknown for value: " x)
                              {:value x :type (type x)}))))
