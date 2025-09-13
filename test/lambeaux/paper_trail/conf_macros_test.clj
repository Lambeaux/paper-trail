;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.conf-macros-test
  (:require [clojure.test :as t :refer [deftest testing]]
            [lambeaux.paper-trail.conf-core :as conf]))

(comment
  "List macros in clojure.core alphabetically"
  (->> (the-ns 'clojure.core)
       (ns-publics)
       (vals)
       (map meta)
       (filter :macro)
       (map :name)
       (sort)
       (take 30)))

(comment
  "Macros that still need tests"
  ..
  amap
  areduce
  assert
  bound-fn
  delay
  "Macros we likely won't test (for now)"
  declare
  definline
  definterface
  defmacro
  defmethod
  defmulti
  defn
  defn-
  defonce
  defprotocol
  defrecord
  defstruct
  deftype)

;; Used for testing (binding ...) in below tests
(def ^:dynamic *test-counter* 0)

;; Used for testing (with-redefs ...) in below tests
(def test-fn (fn [idx] idx))

(deftest ^:core test-common-macros
  (testing "Test common macros from clojure.core"
    ;; ---------------------------------------------------------------------------
    (testing "that wrap a body: "
      (conf/forms->test "binding"
        ;; TODO: add a way to specify if we expect eval to throw or not 
        ;; (protect against false positives where the result matched but we didn't cover the intended code path)
        (binding)
        (binding [])
        ;; TODO: match paper trail's exception to the clojure one when a symbol cannot be found
        ;;   Clojure's flavor: Unable to resolve symbol: *test-counter* in this context (RuntimeException)
        ;;   PT's current flavor: class clojure.lang.Symbol cannot be cast to class java.lang.Number (ClassCastException)
        #_(binding [] (inc *test-counter*))
        ;; TODO: fix lookup of fully qualified symbols (PT gets a NullPointerException)
        ;;   Call Stack: [#stack.val[lambeaux.paper-trail.conf-test/*test-counter*]]
        ;;   Next Command: calling (var ...) so need to add support for that
        #_(binding []
            (inc lambeaux.paper-trail.conf-test/*test-counter*))
        #_(binding [lambeaux.paper-trail.conf-test/*test-counter* 1]
            (inc lambeaux.paper-trail.conf-test/*test-counter*))
        #_(binding [lambeaux.paper-trail.conf-test/*test-counter* 2]
            (inc lambeaux.paper-trail.conf-test/*test-counter*))
        #_(binding [lambeaux.paper-trail.conf-test/*test-counter* 1]
            (binding [lambeaux.paper-trail.conf-test/*test-counter* 2]
              (inc lambeaux.paper-trail.conf-test/*test-counter*)))
        #_(binding [lambeaux.paper-trail.conf-test/*test-counter* 1]
            (binding [lambeaux.paper-trail.conf-test/*test-counter* 2]
              (binding [lambeaux.paper-trail.conf-test/*test-counter* 3]
                (inc lambeaux.paper-trail.conf-test/*test-counter*))))))
    ;; ---------------------------------------------------------------------------
    (testing "that involve threading forms: "
      (conf/forms->test "->"
        (->)
        (-> (hash-map))
        (-> (hash-map) (assoc :k "v"))
        (-> (hash-map) (assoc :k "v1") (assoc :k "v2"))
        (-> (hash-map) (assoc :k "v1") (assoc :k "v2") (assoc :k "v3")))
      (conf/forms->test "cond->"
        (cond->)
        (cond-> (hash-map))
        ;; TODO: add support to ex-comparable for munging out the specifics of CompilerException
        ;; for example: 'output.calva-repl:171:1' in the :message and :line/:column in the :data
        ;; (cond-> (hash-map) true)
        (cond-> (hash-map) true (assoc :k "v"))
        (cond-> (hash-map) true (assoc :k "v1") true (assoc :k "v2"))
        (cond-> (hash-map) true (assoc :k "v1") true (assoc :k "v2") true (assoc :k "v3"))
        (cond-> (hash-map) true (assoc :k "v1") true (assoc :k "v2") false (assoc :k "v3"))
        (cond-> (hash-map) true (assoc :k "v1") false (assoc :k "v2") false (assoc :k "v3"))
        (cond-> (hash-map) false (assoc :k "v1") false (assoc :k "v2") false (assoc :k "v3")))
      (conf/forms->test "->>"
        (->>)
        (->> (vector 1 2 3))
        (->> (vector 1 2 3) (mapv inc))
        (->> (vector 1 2 3) (mapv inc) (mapv inc))
        (->> (vector 1 2 3) (mapv inc) (mapv inc) (mapv inc)))
      (conf/forms->test "cond->>"
        (cond->>)
        (cond->> (vector 1 2 3))
        ;; TODO: add support to ex-comparable for munging out the specifics of CompilerException
        ;; for example: 'output.calva-repl:171:1' in the :message and :line/:column in the :data
        ;; (cond->> (vector 1 2 3) true)
        (cond->> (vector 1 2 3) true (mapv inc))
        (cond->> (vector 1 2 3) true (mapv inc) true (mapv inc))
        (cond->> (vector 1 2 3) true (mapv inc) true (mapv inc) true (mapv inc))
        (cond->> (vector 1 2 3) true (mapv inc) true (mapv inc) false (mapv inc))
        (cond->> (vector 1 2 3) true (mapv inc) false (mapv inc) false (mapv inc))
        (cond->> (vector 1 2 3) false (mapv inc) false (mapv inc) false (mapv inc)))
      (conf/forms->test "as->"
        (as->)
        (as-> (+ 1 1))
        (as-> (+ 1 1) _$)
        (as-> (hash-map) $ (assoc $ :k "v"))
        (as-> (hash-map) $ (assoc $ :k "v1") (assoc $ :k "v2"))
        (as-> (vector 1 2 3) $ (mapv inc $))
        (as-> (vector 1 2 3) $ (mapv inc $) (mapv inc $))
        (as-> (hash-map) $
          (assoc $ :k (vector 10))
          (update $ :k conj 11)
          (:k $)
          (mapv inc $))))
    ;; ---------------------------------------------------------------------------
    (testing "that fulfill misc goals: "
      (conf/forms->test "comment"
        (comment)
        (comment 1)
        (comment 1 2)
        (comment 1 2 3)))
    ;; ---------------------------------------------------------------------------
    (testing "that involve control flow: "
      (conf/forms->test "if-not"
        (if-not (= 1 1) :hi)
        (if-not (= 1 2) :hi)
        (if-not (= 1 1) :hi :bye)
        (if-not (= 1 2) :hi :bye))
      (conf/forms->test "when"
        (when (= 1 1) :result)
        (when (= 1 2) :result))
      (conf/forms->test "when-not"
        (when-not (= 1 1) :result)
        (when-not (= 1 2) :result))
      (conf/forms->test "and"
        (and)
        (and 1)
        (and 1 2)
        (and 1 2 3)
        (and nil 1)
        (and nil nil 1)
        (and nil nil nil 1)
        (and false 1)
        (and false false 1)
        (and false false false 1)
        (and nil)
        (and nil nil)
        (and nil nil nil)
        (and false)
        (and false false)
        (and false false false))
      (conf/forms->test "or"
        (or)
        (or 1)
        (or 1 2)
        (or 1 2 3)
        (or nil 1)
        (or nil nil 1)
        (or nil nil nil 1)
        (or false 1)
        (or false false 1)
        (or false false false 1)
        (or nil)
        (or nil nil)
        (or nil nil nil)
        (or false)
        (or false false)
        (or false false false))
      (conf/forms->test "case"
        (case)
        ;; TODO: cannot find fn 'case*' during evaluation
        #_(case 2)
        #_(case 2 0)
        #_(case 2 1 -1)
        #_(case 2 1 -1 0)
        #_(case 2 1 -1 2 -2)
        #_(case 2 1 -1 2 -2 0))
      (conf/forms->test "cond"
        (cond)
        (cond (= 1 1) :result)
        (cond (= 1 2) :result)
        (cond (= 1 1) :result
              :else 0)
        (cond (= 1 2) :result
              :else 0)
        (cond (= 1 1) 1
              (= 1 2) 2)
        (cond (= 1 2) 1
              (= 2 2) 2)
        (cond (= 1 1) 1
              (= 1 2) 2
              :else   3)
        (cond (= 1 2) 1
              (= 1 1) 2
              :else   3)
        (cond (= 1 2) 1
              (= 1 2) 2
              :else   3))
      (conf/forms->test "condp"
        (condp)
        ;; TODO: trying to invoke-fn a java.lang.IllegalArgumentException. (interop not done)
        #_(condp = 2)
        (condp = 2 0)
        ;; TODO: incorrect result, getting -1 instead of 0 or error
        #_(condp = 2 1 -1)
        #_(condp = 2 1 -1 0)
        #_(condp = 2 1 -1 2 -2)
        #_(condp = 2 1 -1 2 -2 0)))))
