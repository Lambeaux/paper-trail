(ns lambeaux.paper-trail.conf-test
  "Conformance tests for the interpreter."
  (:require [clojure.test :as t :refer [deftest testing is]]
            [clojure.walk :as w]
            [lambeaux.paper-trail.decomp :as impl]
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
                            (wrap* (capture-result (impl/run-eval '~form)))))))
                forms))))

(defn compare-eval*
  [& forms]
  (let [handlers [`eval `impl/run-eval]
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
             (catch RuntimeException _ex :outer-error)))))
  ;; ---------------------------------------------------------------------------
  (testing "Test (try) with (finally)"
    (testing "when nothing gets thrown"
      (forms->test "using basic values"
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
      (forms->test "using basic values in a nested try"
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
      (forms->test "using basic values in a nested finally"
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (finally
                        (try (swap! x inc)
                             (swap! x inc)
                             x
                             (finally (swap! x inc)))))))))
    (testing "when an exception is thrown"
      (forms->test "using basic values"
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (try (swap! x inc)
                           (throw (ex-info "Hi" (hash-map :the-atom x)))
                           x
                           (finally (swap! x inc)))
                      (finally (swap! x inc))))))
      (forms->test "using basic values in a nested try"
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (try (swap! x inc)
                           (throw (ex-info "Hi" (hash-map :the-atom x)))
                           x
                           (finally (swap! x inc)
                                    (swap! x inc)))
                      (finally (swap! x inc)
                               (swap! x inc))))))
      (forms->test "using basic values in a nested finally"
        (deref (let [x (atom 1)]
                 (try (swap! x inc)
                      (finally
                        (try (throw (ex-info "Hi" (hash-map :the-atom x)))
                             (swap! x inc)
                             x
                             (finally (swap! x inc))))))))))
  ;; ---------------------------------------------------------------------------
  (testing "Test (try) with (catch) and (finally)"
    (forms->test "when nothing is thrown"
      (let [int-atom (atom 1)
            vec-val (vector 1 2 3 4 5)
            result (try
                     (into [] (filter odd? (map inc vec-val)))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (forms->test "when an exception is manually thrown"
      (let [int-atom (atom 1)
            result (try
                     (into [] (filter odd? (throw (ex-info "Hi" {}))))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (forms->test "when an exception propogates up from the call stack"
      (let [int-atom (atom 1)
            vec-val (map str (vector 1 2 3 4 5))
            result (try
                     (into [] (filter odd? (map inc vec-val)))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (forms->test "when an exception is caught by the inner catch"
      (let [int-atom (atom 1)
            vec-val (map str (vector 1 2 3 4 5))
            result (try
                     (into [] (try
                                (filter odd? (mapv inc vec-val))
                                (catch ClassCastException _cce [:inner-error])
                                (finally (swap! int-atom inc))))
                     (catch Exception _e :error)
                     (finally (swap! int-atom inc)))]
        (hash-map :int-atom (deref int-atom) :result result)))
    (forms->test "when an exception is caught by the outer catch"
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
    (forms->test "when an inner exception is not caught at all"
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
    (forms->test "when an outer exception is not caught at all"
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
    (forms->test "when an exception is thrown in a finally after catch resolution"
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
    (forms->test "when an exception is thrown in a finally without catch resolution"
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

(deftest ^:core test-anonymous-functions
  #_(forms->test "Test invoke (fn) directly"
      ((fn [] 100))
      ((fn [x] (+ 100 x)) 1)
      ((fn [x y] (+ 100 x y)) 1 2)
      ((fn [x y z] (+ 100 x y z)) 1 2 3))
  (forms->test "Test (fn) inside lazy sequences"
    (map (fn [x] (inc x))
         (filter (fn [x] (odd? x))
                 (into (vector) (range 1 6))))
    (map (fn [x] (range (* -1 x) x))
         (into (vector) (range 1 6))))
  (forms->test "Test (fn) inside realized sequences"
    (mapv (fn [x] (inc x))
          (filterv (fn [x] (odd? x))
                   (into (vector) (range 1 6))))
    (mapv (fn [x] (into [] (range (* -1 x) x)))
          (into (vector) (range 1 6))))
  (forms->test "Test (fn) inside (fn) inside lazy sequences"
    (map (fn [x] (map (fn [y] (inc y)) (get x :seq)))
         (mapv (fn [i] (hash-map :seq (range i)))
               (range 1 6))))
  (forms->test "Test (fn) inside (fn) inside realized sequences"
    (mapv (fn [x] (mapv (fn [y] (inc y)) (get x :seq)))
          (mapv (fn [i] (hash-map :seq (into (vector) (range i))))
                (range 1 6)))))

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