;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.conf-core
  (:require [clojure.test :as t :refer [testing is]]
            [clojure.walk :as w]
            [lambeaux.paper-trail.impl.core :as impl]
            [lambeaux.paper-trail :as-alias pt])
  (:import  [clojure.lang ExceptionInfo Atom]
            [java.io IOException]))

;; runtime ex
(defn new-illegal-arg-ex
  ([msg] (IllegalArgumentException. msg))
  ([msg cause] (IllegalArgumentException. msg cause)))

;; runtime ex
(defn new-illegal-state-ex
  ([msg] (IllegalStateException. msg))
  ([msg cause] (IllegalStateException. msg cause)))

;; NOT runtime ex
(defn new-io-ex
  ([msg] (IOException. msg))
  ([msg cause] (IOException. msg cause)))

(defn throwable?
  [obj]
  (instance? Throwable obj))

(defn ex-info?
  [obj]
  (instance? ExceptionInfo obj))

(defn ex-comparable*
  [obj]
  (when obj
    {:type    ::pt/ex-comparable
     :clazz   (class obj)
     :message (ex-message obj)
     :data    (ex-data obj)
     ;; todo: can only recur from tail position
     ;; not a big deal but possibly revisit later
     :cause   (ex-comparable* (ex-cause obj))}))

(defn ex-comparable
  [obj]
  (w/postwalk
   (fn [obj]
     (if-not (instance? Atom obj)
       obj
       {:type ::pt/atom-comparable :data (deref obj)}))
   (if-not (throwable? obj)
     obj
     (ex-comparable* obj))))

(comment

  "Keeping this code around just temporarily, can probably remove it
   if I don't ever need to compare nested data that contain throwables."

  (defn ex=
    "Extend Clojure equality to include throwables. Not comprehensive, but
     sufficient enough for our testing purposes."
    ([_x] true)
    ([x y]
     (if-not (throwable? x)
       (= x y)
       (if-not (throwable? y)
         false
         (= (ex-comparable* x)
            (ex-comparable* y)))))
    ([x y & more]
     (if (ex= x y)
       (if (next more)
         (recur y (first more) (next more))
         (ex= y (first more)))
       false)))

  (deftype TestResult [result]
    Object
    (equals [this other]
      (ex= (.result this) (.result other)))
    (hashCode [this]
      (.hashCode (.result this)))))

(defmacro capture-result
  [& body]
  (let [ex-sym (gensym "ex-")]
    `(try
       {::pt/uncaught? false
        ::pt/result    (do ~@body)}
       (catch Exception ~ex-sym
         {::pt/uncaught? true
          ::pt/result    ~ex-sym}))))

(defn wrap*
  [pt-result]
  (update pt-result ::pt/result ex-comparable))

(defmacro forms->test
  [msg & forms]
  `(testing ~msg
     (do ~@(map (fn [form]
                  ;; Possibly move '~form to a let binding
                  ;; due to expansion/evaluation side effects.
                  ;; ---
                  ;; In this case it's probably fine because
                  ;; we're just using the form "as data". However,
                  ;; if any atoms or other state are not local to
                  ;; the form, the test results might be skewed.
                  `(testing (str '~form)
                     (is (= (wrap* (capture-result (eval '~form)))
                            (wrap* (capture-result (impl/evaluate '~form)))))))
                forms))))

(defn compare-eval*
  [& forms]
  (let [handlers [`eval `impl/evaluate]
        test-cases (for [handler-sym handlers form forms]
                     {:handler handler-sym :input form})]
    (mapv (fn [{:keys [handler input] :as tcase}]
            (let [h (deref (resolve handler))
                  outcome (capture-result (h input))]
              (assoc tcase :outcome (update outcome ::pt/result ex-comparable))))
          test-cases)))

(defn all-evals-match?
  [compare-reports]
  (apply = (map :outcome compare-reports)))

(comment
  "How do use compare-eval to investigate conformance test failures: "
  (compare-eval* '(try (into [] (map inc ["a" "b" "c"]))))
  (all-evals-match? *1))
