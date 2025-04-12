(ns lambeaux.paper-trail.conf-test
  "Conformance tests for the interpreter."
  (:require [clojure.test :as t :refer [deftest testing is]]
            [lambeaux.paper-trail.decomp :as ptd]
            [paper.trail :as-alias pt])
  (:import  [clojure.lang ExceptionInfo]
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
  (if-not (throwable? obj)
    obj
    (ex-comparable* obj)))

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
                            (wrap* (capture-result (ptd/run-eval '~form)))))))
                forms))))

(defn compare-eval*
  [& forms]
  (let [handlers [`eval `ptd/run-eval]
        test-cases (for [handler-sym handlers form forms]
                     {:handler handler-sym :input form})]
    (mapv (fn [{:keys [handler input] :as tcase}]
            (let [h (deref (resolve handler))
                  outcome (capture-result (h input))]
              (assoc tcase :outcome (update outcome ::pt/result ex-comparable))))
          test-cases)))

(comment
  "How do use compare-eval to investigate conformance test failures: "
  (compare-eval* '(try (into [] (map inc ["a" "b" "c"]))))
  (apply = (map :outcome *1)))

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

;; todo: revisit once you finish implementing interop
;; '(try (throw (IllegalArgumentException. "Hi")))
(deftest ^:special test-try-catch-finally
  ;; ---------------------------------------------------------------------------
  (testing "Test (try) standalone"
    ;; ---------------------------------------------------------------------------
    (testing "when nothing gets thrown"
      (forms->test "using basic values"
        (try (+ 1 1))
        (try (into [] (map inc [1 2 3]))))
      (forms->test "using basic values in a nested try"
        (try (try (+ 1 1)))
        (try (into [] (try (map inc [1 2 3]))))))
    ;; ---------------------------------------------------------------------------
    (testing "when exceptions are manually thrown"
      (forms->test "using basic values"
        (try (throw (ex-info "Hi" {})))
        (try (into [] (map inc (throw (ex-info "Hi" {}))))))
      (forms->test "using basic values in a nested try"
        (try (try (throw (ex-info "Hi" {}))))
        (try (into [] (try (map inc (throw (ex-info "Hi" {}))))))))
    ;; ---------------------------------------------------------------------------
    (testing "when exceptions propagate from other fns"
      (forms->test "using basic values"
        (try (into [] (map inc ["a" "b" "c"]))))
      (forms->test "using basic values in a nested try"
        (try (into [] (try (map inc ["a" "b" "c"])))))))
  ;; ---------------------------------------------------------------------------
  (testing "Test (try) with (catch)"
    ;; ---------------------------------------------------------------------------
    (testing "when nothing gets thrown"
      (forms->test "using basic values"
        (try (+ 1 1)
             (catch Exception _e :error))
        (try (into [] (map inc [1 2 3]))
             (catch Exception _e :error)))
      (forms->test "using basic values in a nested try"
        (try (try (+ 1 1)
                  (catch Exception _ey :inner-error))
             (catch Exception _ex :outer-error))
        (try (into [] (try (map inc [1 2 3])
                           (catch Exception _ey :inner-error)))
             (catch Exception _ex :outer-error))))
    ;; ---------------------------------------------------------------------------
    (testing "when thrown exceptions are not caught"
      (forms->test "using basic values"
        (try (throw (ex-info "Hi" {}))
             (catch IllegalArgumentException _e :error))
        (try (into [] (map inc (throw (ex-info "Hi" {}))))
             (catch IllegalArgumentException _e :error)))
      (forms->test "using basic values in a nested try"
        (try (try (throw (ex-info "Hi" {}))
                  (catch IllegalArgumentException _ey :inner-error))
             (catch IllegalArgumentException _ex :outer-error))
        (try (into [] (try (map inc (throw (ex-info "Hi" {})))
                           (catch IllegalArgumentException _ey :inner-error)))
             (catch IllegalArgumentException _ex :outer-error))))
    ;; ---------------------------------------------------------------------------
    (testing "when thrown exceptions trigger a catch"
      (forms->test "using basic values"
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
      (forms->test "using basic values in a nested try"
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
      (forms->test "using basic values"
        (try (into [] (map inc ["a" "b" "c"]))
             (catch IllegalArgumentException _e :error)))
      (forms->test "using basic values in a nested try"
        (try (into [] (try (map inc ["a" "b" "c"])
                           (catch IllegalArgumentException _ey :inner-error)))
             (catch IllegalArgumentException _ex :outer-error))))
    ;; ---------------------------------------------------------------------------
    (testing "when propagated exceptions trigger a catch"
      (forms->test "using basic values"
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
      (forms->test "using basic values in a nested try"
        (try (into [] (try (map inc ["a" "b" "c"])
                           (catch IllegalArgumentException _ey :inner-error)))
             (catch Exception _ex :outer-error))
        (try (into [] (try (map inc ["a" "b" "c"])
                           (catch Exception _ey :inner-error)))
             (catch IllegalArgumentException _ex :outer-error))
        (try (into [] (try (map inc ["a" "b" "c"])
                           (catch Exception _ey :inner-error)))
             (catch RuntimeException _ex :outer-error))))))

(comment
  (testing "Test (try) with (finally)"
    (testing "when nothing gets thrown"
      ())
    (testing "when an exception is thrown"
      ()))
  (testing "Test (try) with (catch) and (finally)"))

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