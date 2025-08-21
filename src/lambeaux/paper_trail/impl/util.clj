;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.util
  (:require [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang Cons]))

(defn counter-fn
  "Returns a no-arg fn that returns the next integer in a ever-increasing
   sequence of integers, starting at 0."
  []
  (let [state* (atom -1)]
    (fn []
      (swap! state* inc))))

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

;; ----------------------------------------------------------------------------

(def temp-ctx {::pt/current-ns *ns*})

(comment ;; Some ideas for the form hierarchy, but not necessary right now
  ;; Including all leaf nodes in the generator.clj dispatch map is sufficient

  (def type->childtypes
    {:type/form
     [:type/scalar :type/composite]
     :type/scalar
     [:type/string :type/number :type/boolean :type/char :type/nil :type/fn :type/class :type/ident]
     :type/ident
     [:type/symbol :type/keyword]
     :type/symbol
     [:type/symbol-simple :type/symbol-qual]
     :type/keyword
     [:type/keyword-simple :type/keyword-qual]
     :type/composite
     [:type/map :type/vector :type/set :type/invokable :type/seq]
     :type/invokable
     [:type/macro :type/list :type/cons]})

  (defn create-form-hierarchy
    []
    (reduce (fn [h* [parent child]]
              (derive h* child parent))
            (make-hierarchy)
            (->> (seq type->childtypes)
                 (map (fn [[k coll]] (map vector (repeat k) coll)))
                 (apply concat))))

  ;;;; Form type hierarchy ;;;;
  (defonce fth (create-form-hierarchy)))

;; ----------------------------------------------------------------------------
;; Form Classification
;; ----------------------------------------------------------------------------

(defn macro-type
  "Returns the macro type when invokable form x is a macro, else nil."
  [x]
  (let [op (first x)]
    (when (accessible-macro? temp-ctx op)
      :type/macro)))

;; - Use symbols to represent literals in a dispatch map ('let, 'try, etc)
;; - Use keywords to represent types in a dispatch map (:list, :vector, :map, etc)
;; - Use multi-methods to arrive at the key when preds are needed??
;; - Use a map to arrive at dispatch keywords??

(defn classify
  "Returns a type keyword used for generating commands for input forms."
  [x]
  (cond (string? x)            :type/string
        (number? x)            :type/number
        (boolean? x)           :type/boolean
        (char? x)              :type/char
        (nil? x)               :type/nil
        (fn? x)                :type/fn
        (class? x)             :type/class
        (simple-keyword? x)    :type/keyword-simple
        (simple-symbol? x)     :type/symbol-simple
        (qualified-keyword? x) :type/keyword-qual
        (qualified-symbol? x)  :type/symbol-qual
        (map? x)               :type/map
        (vector? x)            :type/vector
        (set? x)               :type/set
        (list? x)              (or (macro-type x) :type/list)
        (instance? Cons x)     (or (macro-type x) :type/cons)
        (seq? x)               :type/seq
        :else (throw (ex-info (str "Type " (type x) " is unknown for value: " x)
                              {:value x :type (type x)}))))
