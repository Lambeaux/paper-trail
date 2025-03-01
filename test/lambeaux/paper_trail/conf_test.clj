(ns lambeaux.paper-trail.conf-test
  "Conformance tests for the interpreter."
  (:require [clojure.test :as t :refer [deftest testing is]]
            [lambeaux.paper-trail.decomp :as ptd]))

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
                     (is (= (eval '~form)
                            (ptd/run-eval '~form)))))
                forms))))

(deftest ^:minimal test-core-fns
  (forms->test "Test basic core functions"
    (+ 1 1)
    (list 1 2 3)
    (vector 1 2 3)
    (hash-map :a 1 :b 2)))

(deftest ^:minimal test-nested-core-fns
  (forms->test "Test basic core functions when nested"
    (into (vector) (list 1 2 3))
    (into (vector) (list 1 0.0 "hi" :hi :ex/hi))
    (concat (vector 1 2 3) (list 4 5 6))
    (filter even? (map inc (range 20)))))

(deftest ^:minimal test-literals
  (forms->test "Test data literals"
    (into [] (list 1 2 3))
    (get {:a 1 :b 2} :a)))

(deftest ^:minimal test-do-return-vals
  (forms->test "Test (do) return behavior"
    (do)
    (do nil)
    (do nil nil)
    (do false)
    (do true)
    (do (+ 1 1))
    (do (+ 1 1) (+ 2 2))))

(deftest ^:special test-if-standalone
  (forms->test "Test (if) on its own"
    (if true true)
    (if true false)
    (if false false)
    (if (= 1 1) (+ 2 3) (- 3 2))
    (if (= 1 2) (+ 2 3) (- 3 2))))

(deftest ^:special test-let-standalone
  (forms->test "Test (let) on its own"
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
  (forms->test "Test (let) implicit (do)"
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
  (forms->test "Test (loop) on its own"
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
  (forms->test "Test (do) side effect behavior"
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

(deftest ^:core test-common-macros
  (forms->test "Test common macros from clojure.core"
    (if-not (= 1 2) :hi)
    (if-not (= 1 1) :hi :bye)
    (and 1 2 3)
    (or nil nil 1)
    (when (= 1 1) :hi)
    (when (= 1 2) :hi)
    (cond (= 1 1) :hi
          (= 1 2) :bye
          :else   :sigh)
    (cond (= 1 2) :hi
          (= 1 1) :bye
          :else   :sigh)
    (cond (= 1 2) :hi
          (= 1 2) :bye
          :else   :sigh)))
