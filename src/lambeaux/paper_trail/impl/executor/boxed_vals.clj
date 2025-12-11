;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.executor.boxed-vals
  "Containers for values that also support Clojure metadata. Borrows terminology from Java 
   boxing/unboxing of types and is conceptually similar but a completely separate mechanic.
   The interpreter may need a value boxed for one or more of the following reasons:
   * The value is on the call stack and needs to hold metadata that impacts how it is processed.
   * The value is on the context and printing the value would cause premature realization.
   * The value is absurdly large and, as a convenience, is hidden by default."
  (:require [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang IObj IPending]))

;; ------------------------------------------------------------------------------------------------
;; Boxed Values: API
;; ------------------------------------------------------------------------------------------------

(declare is-boxed?)

(defn box-type  [this] (when (is-boxed? this) (. this boxType)))
(defn box-value [this] (when (is-boxed? this) (. this boxValue)))
(defn box-meta  [this] (when (is-boxed? this) (. this boxMeta)))

(deftype MetaBox [boxType boxValue boxMeta]
  IObj
  (meta [this]
    (box-meta this))
  (withMeta [this m]
    (new MetaBox (box-type this) (box-value this) m)))

(defn new-box
  ([bv] (new-box nil bv))
  ([bt bv] (new MetaBox bt bv nil)))

(defn is-boxed?
  [obj]
  (instance? MetaBox obj))

(def special-box-type? #{:unrealized :unhandled :hidden})

;; (pr-str nil)
(defn box->str
  [this]
  (let [box-val* (box-value this)
        meta-str (pr-str (box-meta this))]
    (if (nil? box-val*)
      (str "#box.val[nil " meta-str "]")
      (let [class-str (.getName (class box-val*))
            box-type* (box-type this)]
        (if (special-box-type? box-type*)
          (str "#box." (name box-type*) "[\"" class-str "\" " meta-str "]")
          (str "#box.val[" box-val* " " meta-str "]"))))))

(defmethod print-method MetaBox
  [this writer]
  (.write writer (box->str this)))

(defn val->unrealized [v] (new-box :unrealized v))
(defn val->unhandled  [v] (new-box :unhandled v))
(defn val->hidden     [v] (new-box :hidden v))

(defn is-realized-val?
  [obj]
  (if (and (instance? MetaBox obj)
           (= :unrealized (box-type obj)))
    false
    (if-not (instance? IPending obj)
      true
      (realized? obj))))

(defn val-unwrap
  [obj]
  (if-not (is-boxed? obj)
    obj
    (box-value obj)))

;; todo: when it comes to val->unhandled, probably update from 'Exception' to 'Throwable'
;; todo: default `m` used to be `{::pt/is-evaled? true}`, ensure that's not breaking
(defn val-wrap
  ([obj]
   (val-wrap obj {}))
  ([obj m]
   ;; todo: should merge order be flipped so `m` overrides existing meta?
   (let [total-meta (merge m (meta obj))]
     (with-meta
       (cond
         (not (is-realized-val? obj)) (val->unrealized obj)
         (instance? Exception obj) (val->unhandled obj)
         :else (new-box obj))
       total-meta))))
