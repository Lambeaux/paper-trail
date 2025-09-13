;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.conf-test
  "Conformance tests for the interpreter."
  (:require [clojure.test :as t :refer [deftest]]
            [lambeaux.paper-trail.conf-core :as conf]))

(deftest ^:minimal test-core-fns
  (conf/forms->test "Test basic core functions"
    (+ 1 1)
    (list 1 2 3)
    (vector 1 2 3)
    (hash-map :a 1 :b 2)))

(deftest ^:minimal test-nested-core-fns
  (conf/forms->test "Test basic core functions when nested"
    (into (vector) (list 1 2 3))
    (into (vector) (list 1 0.0 "hi" :hi :ex/hi))
    (concat (vector 1 2 3) (list 4 5 6))
    (filter even? (map inc (range 20)))))

(deftest ^:minimal test-literals
  (conf/forms->test "Test data literals"
    (into [] (list 1 2 3))
    (get {:a 1 :b 2} :a)))

(deftest ^:minimal test-do-return-vals
  (conf/forms->test "Test (do) return behavior"
    (do)
    (do nil)
    (do nil nil)
    (do false)
    (do true)
    (do (+ 1 1))
    (do (+ 1 1) (+ 2 2))))

(deftest ^:special test-if-standalone
  (conf/forms->test "Test (if) on its own"
    (if true true)
    (if true false)
    (if false false)
    (if (= 1 1) (+ 2 3) (- 3 2))
    (if (= 1 2) (+ 2 3) (- 3 2))))

(deftest ^:special test-let-standalone
  (conf/forms->test "Test (let) on its own"
    (let [x 1] x)
    (let [x 1 y 2] (vector x y))
    (let [x (filter even? (map inc (range 20)))
          y (filter odd? (map inc (range 20)))]
      (into [] (map + x y)))
    (let [x (filter even? (map inc (range 20)))]
      (let [y (filter odd? (map inc (range 20)))]
        (into [] (map + x y))))
    (let [x (+ 1 2 3)
          y (let [x (inc x)]
              (inc x))
          z (inc x)]
      (vector x y z)))
  (conf/forms->test "Test (let) implicit (do)"
    (let [a (atom 1)]
      @a)
    (let [a (atom 1)]
      (swap! a inc)
      @a)
    (let [a (atom 1)]
      (swap! a inc)
      (swap! a inc)
      @a)
    (let [a (atom 1)]
      (swap! a inc)
      (swap! a inc)
      (swap! a inc)
      @a)))

(deftest ^:special test-loop-standalone
  (conf/forms->test "Test (loop) on its own"
    (loop [x 1]
      (if (> x 3)
        x
        (recur (inc x))))
    (loop [x 1 y 1]
      (if (> x 3)
        (vector x y)
        (recur (inc x) (* y 2))))
    (loop [x 1]
      (if (> x 3)
        (loop [y 1]
          (if (> y 3)
            (vector x y)
            (recur (inc y))))
        (recur (inc x))))))

(deftest ^:core test-do-side-effects
  (conf/forms->test "Test (do) side effect behavior"
    (let [a (atom 1)]
      (do @a))
    (let [a (atom 1)]
      (do (swap! a inc)
          @a))
    (let [a (atom 1)]
      (do (swap! a inc)
          (swap! a inc)
          @a))
    (let [a (atom 1)]
      (do (swap! a inc)
          (swap! a inc)
          (swap! a inc)
          @a))))

(deftest ^:core test-eval-left-to-right
  (conf/forms->test "Test forms evaluate left-to-right"
    (let [state (atom (list 3 5))
          peek-pop! (fn []
                      (let [the-val (peek (deref state))]
                        (swap! state pop)
                        the-val))]
      (- (* 2 (peek-pop!)) (peek-pop!)))))

(deftest ^:core test-anonymous-functions
  #_(conf/forms->test "Test invoke (fn) directly"
      ((fn [] 100))
      ((fn [x] (+ 100 x)) 1)
      ((fn [x y] (+ 100 x y)) 1 2)
      ((fn [x y z] (+ 100 x y z)) 1 2 3))
  (conf/forms->test "Test (fn) inside lazy sequences"
    (map (fn [x] (inc x))
         (filter (fn [x] (odd? x))
                 (into (vector) (range 1 6))))
    (map (fn [x] (range (* -1 x) x))
         (into (vector) (range 1 6))))
  (conf/forms->test "Test (fn) inside realized sequences"
    (mapv (fn [x] (inc x))
          (filterv (fn [x] (odd? x))
                   (into (vector) (range 1 6))))
    (mapv (fn [x] (into [] (range (* -1 x) x)))
          (into (vector) (range 1 6))))
  (conf/forms->test "Test (fn) inside (fn) inside lazy sequences"
    (map (fn [x] (map (fn [y] (inc y)) (get x :seq)))
         (mapv (fn [i] (hash-map :seq (range i)))
               (range 1 6))))
  (conf/forms->test "Test (fn) inside (fn) inside realized sequences"
    (mapv (fn [x] (mapv (fn [y] (inc y)) (get x :seq)))
          (mapv (fn [i] (hash-map :seq (into (vector) (range i))))
                (range 1 6)))))
