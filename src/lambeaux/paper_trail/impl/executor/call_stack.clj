;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.executor.call-stack
  "The call stack is just a standard Clojure `(list)`. This will hold true for all `call-stack`
   args for the functions in this namespace. It behaves, by convention, similarly to `this`."
  (:require [lambeaux.paper-trail.impl.executor.boxed-vals :as b]
            [lambeaux.paper-trail :as-alias pt]))

;; ------------------------------------------------------------------------------------------------
;; Call Stack: API
;; ------------------------------------------------------------------------------------------------

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

(defn peek-val
  [call-stack]
  (let [frame-or-val (peek call-stack)
        frame? (is-stack-frame? frame-or-val)]
    (if-not frame?
      (b/val-unwrap frame-or-val)
      (b/val-unwrap (peek frame-or-val)))))

(defn peek-frame
  [call-stack]
  (let [frame-or-val (peek call-stack)
        frame? (is-stack-frame? frame-or-val)]
    (if frame?
      (mapv b/val-unwrap frame-or-val)
      (throw (IllegalStateException.
              (str "No valid stack frame to peek: " (pr-str call-stack)))))))

(defn push-val
  [call-stack obj]
  (let [frame? (is-stack-frame? (peek call-stack))
        result (b/val-wrap obj)]
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
