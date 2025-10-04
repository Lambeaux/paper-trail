;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.executor.call-stack
  (:require [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang IObj IPending]))

;; ------------------------------------------------------------------------------------------------
;; Call Stack: API
;; ------------------------------------------------------------------------------------------------

(defn box-type [this] (. this boxType))
(defn box-value [this] (. this boxValue))
(defn box-meta [this] (. this boxMeta))

(deftype StackBox [boxType boxValue boxMeta]
  IObj
  (meta [this]
    (box-meta this))
  (withMeta [this m]
    (new StackBox (box-type this) (box-value this) m)))

(defn new-box
  ([bv] (new-box nil bv))
  ([bt bv] (new StackBox bt bv nil)))

(defmethod print-method StackBox
  [this writer]
  (if-let [box-val* (box-value this)]
    (let [class-str (.getName (class box-val*))]
      (.write writer (case (box-type this)
                       :unrealized (str "#stack.unrealized[" class-str "]")
                       :unhandled  (str "#stack.unhandled[" class-str "]")
                       (str "#stack.val[" box-val* "]"))))
    (.write writer "#stack.val[nil]")))

(defn val->unrealized [v] (new-box :unrealized v))
(defn val->unhandled [v] (new-box :unhandled v))

(defn is-realized-val?
  [obj]
  (if (and (instance? StackBox obj)
           (= :unrealized (box-type obj)))
    false
    (if-not (instance? IPending obj)
      true
      (realized? obj))))

(defn is-stack-frame?
  [obj]
  (boolean (and (vector? obj) (::pt/stack-frame? (meta obj)))))

(defn frame-push
  [call-stack]
  (conj call-stack (with-meta [] {::pt/stack-frame? true})))

(defn frame-pop
  [call-stack]
  (let [frame? (is-stack-frame? (peek call-stack))]
    (if frame?
      (pop call-stack)
      (throw (IllegalStateException.
              (str "No valid stack frame to pop: " (pr-str call-stack)))))))

(defn val-unwrap
  [obj]
  (if-not (instance? StackBox obj)
    obj
    (box-value obj)))

;; TODO: when it comes to val->unhandled, probably update from 'Exception' to 'Throwable'
(defn val-wrap
  [obj]
  (let [total-meta (merge {::pt/is-evaled? true} (meta obj))]
    (with-meta
      (cond
        (not (is-realized-val? obj)) (val->unrealized obj)
        (instance? Exception obj) (val->unhandled obj)
        :else (new-box obj))
      total-meta)))

(defn peek-val
  [call-stack]
  (let [frame-or-val (peek call-stack)
        frame? (is-stack-frame? frame-or-val)]
    (if-not frame?
      (val-unwrap frame-or-val)
      (val-unwrap (peek frame-or-val)))))

(defn peek-frame
  [call-stack]
  (let [frame-or-val (peek call-stack)
        frame? (is-stack-frame? frame-or-val)]
    (if frame?
      (mapv val-unwrap frame-or-val)
      (throw (IllegalStateException.
              (str "No valid stack frame to peek: " (pr-str call-stack)))))))

(defn push-val
  [call-stack obj]
  (let [frame? (is-stack-frame? (peek call-stack))
        result (val-wrap obj)]
    (if-not frame?
      (conj call-stack result)
      (conj (pop call-stack)
            (conj (peek call-stack) result)))))

(defn push-resolved-val
  ([call-stack]
   (push-resolved-val call-stack (peek-val call-stack)))
  ([call-stack obj]
   (let [frame? (is-stack-frame? (peek call-stack))]
     (if frame?
       (push-val (pop call-stack) obj)
       (throw (IllegalStateException.
               (str "No valid stack frame to resolve: " (pr-str call-stack))))))))
