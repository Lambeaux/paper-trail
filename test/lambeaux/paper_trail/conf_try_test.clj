;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.conf-try-test
  (:require [clojure.test :as t :refer [deftest testing]]
            [lambeaux.paper-trail.conf-core :as conf]))

;; todo: revisit once you finish implementing interop
;; '(try (throw (IllegalArgumentException. "Hi")))

(deftest ^:special test-try-standalone
  (testing "Test (try) standalone"
    (testing "when nothing gets thrown"
      (conf/forms->test "using basic values"
        (try (+ 1 1))
        (try (into [] (map inc [1 2 3]))))
      (conf/forms->test "using basic values in a nested try"
        (try (try (+ 1 1)))
        (try (into [] (try (map inc [1 2 3]))))))
    (testing "when exceptions are manually thrown"
      (conf/forms->test "using basic values"
        (try (throw (ex-info "Hi" {})))
        (try (into [] (map inc (throw (ex-info "Hi" {}))))))
      (conf/forms->test "using basic values in a nested try"
        (try (try (throw (ex-info "Hi" {}))))
        (try (into [] (try (map inc (throw (ex-info "Hi" {}))))))))
    (testing "when exceptions propagate from other fns"
      (conf/forms->test "using basic values"
        (try (into [] (map inc ["a" "b" "c"]))))
      (conf/forms->test "using basic values in a nested try"
        (try (into [] (try (map inc ["a" "b" "c"]))))))))

(deftest ^:special test-try-with-catch
  (testing "Test (try) with (catch)"
    ;; ---------------------------------------------------------------------------
    (testing "when nothing gets thrown"
      (conf/forms->test "using basic values"
        (try (+ 1 1)
             (catch Exception _e :error))
        (try (into [] (map inc [1 2 3]))
             (catch Exception _e :error)))
      (conf/forms->test "using basic values in a nested try"
        (try (try (+ 1 1)
                  (catch Exception _ey :inner-error))
             (catch Exception _ex :outer-error))
        (try (into [] (try (map inc [1 2 3])
                           (catch Exception _ey :inner-error)))
             (catch Exception _ex :outer-error))))
    ;; ---------------------------------------------------------------------------
    (testing "when thrown exceptions are not caught"
      (conf/forms->test "using basic values"
        (try (throw (ex-info "Hi" {}))
             (catch IllegalArgumentException _e :error))
        (try (into [] (map inc (throw (ex-info "Hi" {}))))
             (catch IllegalArgumentException _e :error)))
      (conf/forms->test "using basic values in a nested try"
        (try (try (throw (ex-info "Hi" {}))
                  (catch IllegalArgumentException _ey :inner-error))
             (catch IllegalArgumentException _ex :outer-error))
        (try (into [] (try (map inc (throw (ex-info "Hi" {})))
                           (catch IllegalArgumentException _ey :inner-error)))
             (catch IllegalArgumentException _ex :outer-error))))
    ;; ---------------------------------------------------------------------------
    (testing "when thrown exceptions trigger a catch"
      (conf/forms->test "using basic values"
        (try (throw (ex-info "Hi" {}))
             (catch Exception _e :error))
        (try (throw (ex-info "Hi" {}))
             (catch IllegalArgumentException _e :first-catch)
             (catch Exception _e :second-catch))
        (try (throw (ex-info "Hi" {}))
             (catch Exception _e :first-catch)
             (catch RuntimeException _e :second-catch))
        (try (into [] (map inc (throw (ex-info "Hi" {}))))
             (catch Exception _e :error))
        (try (into [] (map inc (throw (ex-info "Hi" {}))))
             (catch IllegalArgumentException _e :first-catch)
             (catch Exception _e :second-catch))
        (try (into [] (map inc (throw (ex-info "Hi" {}))))
             (catch Exception _e :first-catch)
             (catch RuntimeException _e :second-catch)))
      (conf/forms->test "using basic values in a nested try"
        (try (try (throw (ex-info "Hi" {}))
                  (catch Exception _ey :inner-error))
             (catch Exception _ex :outer-error))
        (try (try (throw (ex-info "Hi" {}))
                  (catch IllegalArgumentException _ey :inner-error))
             (catch Exception _ex :outer-error))
        (try (try (throw (ex-info "Hi" {}))
                  (catch Exception _ey :inner-error))
             (catch RuntimeException _ex :outer-error))
        (try (into [] (try (map inc (throw (ex-info "Hi" {})))
                           (catch Exception _ey :inner-error)))
             (catch Exception _ex :outer-error))
        (try (into [] (try (map inc (throw (ex-info "Hi" {})))
                           (catch IllegalArgumentException _ey :inner-error)))
             (catch Exception _ex :outer-error))
        (try (into [] (try (map inc (throw (ex-info "Hi" {})))
                           (catch Exception _ey :inner-error)))
             (catch RuntimeException _ex :outer-error))))
    ;; ---------------------------------------------------------------------------
    (testing "when propagated exceptions are not caught"
      (conf/forms->test "using basic values"
        (try (into [] (map inc ["a" "b" "c"]))
             (catch IllegalArgumentException _e :error)))
      (conf/forms->test "using basic values in a nested try"
        (try (into [] (try (map inc ["a" "b" "c"])
                           (catch IllegalArgumentException _ey :inner-error)))
             (catch IllegalArgumentException _ex :outer-error))))
    ;; ---------------------------------------------------------------------------
    (testing "when propagated exceptions trigger a catch"
      (conf/forms->test "using basic values"
        (try (into [] (map inc ["a" "b" "c"]))
             (catch Exception _e :error))
        (try (into [] (map inc ["a" "b" "c"]))
             (catch IllegalStateException _e :first-catch)
             (catch RuntimeException _e :second-catch)
             (catch Exception _e :third-catch))
        (try (into [] (map inc ["a" "b" "c"]))
             (catch IllegalStateException _e :first-catch)
             (catch IllegalArgumentException _e :second-catch)
             (catch RuntimeException _e :third-catch)
             (catch Exception _e :fourth-catch))
        (try (into [] (map inc ["a" "b" "c"]))
             (catch ClassCastException _e :first-catch)
             (catch RuntimeException _e :second-catch)
             (catch Exception _e :third-catch))
        (try (into [] (map inc ["a" "b" "c"]))
             (catch IllegalStateException _e :first-catch)
             (catch ClassCastException _e :second-catch)
             (catch RuntimeException _e :third-catch)
             (catch Exception _e :fourth-catch)))
      (conf/forms->test "using basic values in a nested try"
        (try (into [] (try (map inc ["a" "b" "c"])
                           (catch IllegalArgumentException _ey :inner-error)))
             (catch Exception _ex :outer-error))
        (try (into [] (try (map inc ["a" "b" "c"])
                           (catch Exception _ey :inner-error)))
             (catch IllegalArgumentException _ex :outer-error))
        (try (into [] (try (map inc ["a" "b" "c"])
                           (catch Exception _ey :inner-error)))
             (catch RuntimeException _ex :outer-error))))))

(deftest ^:special test-try-with-finally
  (testing "Test (try) with (finally)"
    (testing "when nothing gets thrown"
      (conf/forms->test "using basic values"
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (swap! x inc)
                      x
                      (finally (swap! x inc)))))
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (swap! x inc)
                      x
                      (finally (swap! x inc)
                               (swap! x inc))))))
      (conf/forms->test "using basic values in a nested try"
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (try (swap! x inc)
                           (swap! x inc)
                           x
                           (finally (swap! x inc)))
                      (finally (swap! x inc)))))
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (try (swap! x inc)
                           (swap! x inc)
                           x
                           (finally (swap! x inc)
                                    (swap! x inc)))
                      (finally (swap! x inc)
                               (swap! x inc))))))
      (conf/forms->test "using basic values in a nested finally"
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (finally
                        (try (swap! x inc)
                             (swap! x inc)
                             x
                             (finally (swap! x inc)))))))))
    (testing "when an exception is thrown"
      (conf/forms->test "using basic values"
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (try (swap! x inc)
                           (throw (ex-info "Hi" (hash-map :the-atom x)))
                           x
                           (finally (swap! x inc)))
                      (finally (swap! x inc))))))
      (conf/forms->test "using basic values in a nested try"
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (try (swap! x inc)
                           (throw (ex-info "Hi" (hash-map :the-atom x)))
                           x
                           (finally (swap! x inc)
                                    (swap! x inc)))
                      (finally (swap! x inc)
                               (swap! x inc))))))
      (conf/forms->test "using basic values in a nested finally"
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (finally
                        (try (throw (ex-info "Hi" (hash-map :the-atom x)))
                             (swap! x inc)
                             x
                             (finally (swap! x inc)))))))))))

(deftest ^:special test-try-catch-finally
  (testing "Test (try) with (catch) and (finally)"
    (conf/forms->test "when nothing is thrown"
      (let [int-atom (atom 1)
            vec-val (vector 1 2 3 4 5)
            result (try
                     (into [] (filter odd? (map inc vec-val)))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (conf/forms->test "when an exception is manually thrown"
      (let [int-atom (atom 1)
            result (try
                     (into [] (filter odd? (throw (ex-info "Hi" {}))))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (conf/forms->test "when an exception propogates up from the call stack"
      (let [int-atom (atom 1)
            vec-val (map str (vector 1 2 3 4 5))
            result (try
                     (into [] (filter odd? (map inc vec-val)))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (conf/forms->test "when an exception is caught by the inner catch"
      (let [int-atom (atom 1)
            vec-val (map str (vector 1 2 3 4 5))
            result (try
                     ;; todo: need to test when ex's happen on earlier arg
                     ;; the (into) destination '[]' instead of the source seq
                     (into [] (try
                                (filter odd? (mapv inc vec-val))
                                (catch ClassCastException _cce [:inner-error])
                                (finally (swap! int-atom inc))))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (conf/forms->test "when an exception is caught by the outer catch"
      (let [int-atom (atom 1)
            vec-val (map str (vector 1 2 3 4 5))
            result (try
                     (into [] (try
                                (filter odd? (map inc vec-val))
                                (catch IllegalArgumentException _iae [:inner-error])
                                (finally (swap! int-atom inc))))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (conf/forms->test "when an inner exception is not caught at all"
      (let [int-atom (atom 1)
            vec-val (map str (vector 1 2 3 4 5))
            result (try
                     (into [] (try
                                (filter odd? (mapv inc vec-val))
                                (catch IllegalArgumentException _iae [:inner-error])
                                (finally (swap! int-atom inc))))
                     (catch IllegalStateException _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (conf/forms->test "when an outer exception is not caught at all"
      (let [int-atom (atom 1)
            vec-val (map str (vector 1 2 3 4 5))
            result (try
                     (into [] (try
                                (filter odd? (map inc vec-val))
                                (catch IllegalArgumentException _iae [:inner-error])
                                (finally (swap! int-atom inc))))
                     (catch IllegalStateException _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (conf/forms->test "when an exception is thrown in a finally after catch resolution"
      (let [int-atom (atom 1)
            vec-val (map str (vector 1 2 3 4 5))
            result (try
                     (into [] (try
                                (filter odd? (mapv inc vec-val))
                                (catch ClassCastException _iae [:inner-error])
                                (finally (swap! int-atom inc)
                                         (throw (ex-info "Hi" {})))))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (conf/forms->test "when an exception is thrown in a finally without catch resolution"
      (let [int-atom (atom 1)
            vec-val (map str (vector 1 2 3 4 5))
            result (try
                     (into [] (try
                                (filter odd? (mapv inc vec-val))
                                (catch IllegalArgumentException _iae [:inner-error])
                                (finally (swap! int-atom inc)
                                         (throw (ex-info "Hi" {})))))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))))
