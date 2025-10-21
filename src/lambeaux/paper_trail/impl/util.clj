;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.util
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang Cons Namespace]))

;; ------------------------------------------------------------------------------------------------
;; Lib
;; ------------------------------------------------------------------------------------------------

(def ^:dynamic *assert-gen* (boolean true))

(defmacro assert*
  "Like clojure.core/assert but throws RuntimeException instead of AssertionError.
   This is useful when you want your assertions to be handled by your default error
   handling / exception catching logic."
  ([x]
   (when *assert-gen*
     `(when-not ~x
        (throw (new RuntimeException (str "Assert failed: " (pr-str '~x)))))))
  ([x message]
   (when *assert-gen*
     `(when-not ~x
        (throw (new RuntimeException (str "Assert failed: " ~message "\n" (pr-str '~x))))))))

#_(defn assoc-meta
    "Like 'assoc' but operates on obj's metadata."
    ([obj k v]
     (vary-meta obj assoc k v))
    ([obj k v k* v*]
     (vary-meta obj assoc k v k* v*))
    ([obj k v k* v* & kvs]
     (let [f #(apply assoc (concat [% k v k* v*] kvs))]
       (vary-meta obj f))))

;; TODO: make this invokable to increment, and derefable to read current state without change
(defn counter-fn
  "Returns a no-arg fn that returns the next integer in a ever-increasing
   sequence of integers, starting at 0."
  []
  (let [state* (atom -1)]
    (fn []
      (swap! state* inc))))

(defn disallow-overlap!
  [& keysets]
  (let [overlap (apply set/intersection keysets)]
    (assert* (empty? overlap)
             (str "There must be no overlap between the keysets but found "
                  (pr-str overlap)))))

;; ------------------------------------------------------------------------------------------------
;; Predicates / Resolvers (NEW)
;; ------------------------------------------------------------------------------------------------

(defn sym-split
  "Returns a 2-tuple of the symbol split apart: [(f (namespace sym)) (f (name sym))]. Will only
   call f for truthy inputs. Default f is to coerce to symbol."
  ([sym]
   (sym-split symbol sym))
  ([f sym]
   (when sym
     (vector
      (when-let [sym-ns (namespace sym)]
        (f sym-ns))
      (when-let [sym-name (name sym)]
        (f sym-name))))))

(defn ns-exists?
  "If ns-in is a namespace, returns true. If ns-in is a symbol, returns true if the namespace
   named by that symbol exists."
  [ns-in]
  (try
    (boolean
     (or (instance? Namespace ns-in)
         (and (symbol? ns-in)
              (the-ns ns-in))))
    (catch Exception _e
      false)))

(defn ns-refers-nocore
  "Like ns-refers but maps to namespaces, not vars. Also removes all entries in clojure.core 
   since that case is handled explicitly elsewhere."
  [ns-in]
  (->> (ns-refers (the-ns ns-in))
       (map (fn [[k v]] (vector k (-> v meta :ns))))
       (remove (fn [[_ v]] (= 'clojure.core (ns-name v))))
       (into {})))

(defn ns-qualify-name
  "Attempts to qualify a symbol sym based on src-ns.
   * If there's nothing operate on, return nil.
   * If sym is not qualified, check if it's referred, else qualify with src-ns.
   * If sym is qualified and the ns exists, return sym unchanged.
   * If sym is qualified by a non-existent namespace, check if it's aliased, else return unchanged."
  [src-ns sym]
  (let [[sym-ns sym-name] (sym-split sym)]
    (cond
      (not sym-name)      nil
      (not sym-ns)        (if-let [refer-ns (get (ns-refers-nocore src-ns) sym-name)]
                            (symbol (name (ns-name refer-ns))
                                    (name sym-name))
                            (symbol (name src-ns)
                                    (name sym)))
      (ns-exists? sym-ns) sym
      :else               (if-let [alias-ns (get (ns-aliases src-ns) sym-ns)]
                            (symbol (name (ns-name alias-ns))
                                    (name sym-name))
                            sym))))

(defn ns-resolve*
  "Same as ns-resolve but returned vars are deref'd to get their values."
  ;; note: maybe grab the var metadata while it's available and forward it onto the value
  [ns-in sym]
  (let [result (ns-resolve (the-ns ns-in) sym)]
    (if-not (var? result)
      result
      (deref result))))

(defn ns-resolve-name
  ([sym]
   (assert (symbol? sym) "sym must be a symbol")
   (ns-resolve-name (namespace sym) (name sym)))
  ([ns-str name-str]
   (ns-resolve-name *ns* ns-str name-str))
  ([src-ns ns-str name-str]
   (ns-resolve-name (constantly nil) src-ns ns-str name-str))
  ([lookup-fn src-ns ns-str name-str]
   (assert (not (str/blank? name-str)) "name-str cannot be blank")
   (assert (or (nil? ns-str)
               (not (str/blank? ns-str))) "ns-str cannot be blank, if provided")
   (let [[sym-ns sym-name] (sym-split (symbol ns-str name-str))]
     (if sym-ns
       (ns-resolve* sym-ns sym-name)
       (or (lookup-fn sym-name)
           (ns-resolve* src-ns sym-name)
           (ns-resolve* 'clojure.core sym-name))))))

;; ------------------------------------------------------------------------------------------------
;; Predicates / Resolvers (OLD)
;; ------------------------------------------------------------------------------------------------

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

;; ------------------------------------------------------------------------------------------------

;; todo: replace this with the proper ns binding we do elsewhere
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

;; ------------------------------------------------------------------------------------------------
;; Form Classification
;; ------------------------------------------------------------------------------------------------

(defn macro-type
  "Returns the macro type when invokable form x is a macro, else nil."
  [x]
  (let [op (first x)]
    (when (or (accessible-macro? temp-ctx op)
              ;; note: might move this later, but for now, this will handle .call and call. interop
              (and (simple-symbol? op) 
                   (or (str/starts-with? (name op) ".")
                       (str/ends-with? (name op) "."))))
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
