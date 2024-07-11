(ns lambeaux.paper-trail.base-test
  (:require [clojure.test :as t :refer [deftest testing is]]
            [lambeaux.paper-trail.seq :as ts]))

(deftest basic-trace-seq-tests
  
  (testing "Basic map and filter"
    (is (= (seq [{:form '(map inc [1 2 3 11 12 13 14]), :result '(2 3 4 12 13 14 15)}
                 {:form '(filter even? (2 3 4 12 13 14 15)), :result '(2 4 12 14)}])
           (ts/trace-seq '(filter even? (map inc num-seq))
                         {'num-seq [1 2 3 11 12 13 14]})))
    (is (= (seq [{:form '(map inc [1 2 3 11 12 13 14]), :result '(2 3 4 12 13 14 15)}
                 {:form '(filter odd? (2 3 4 12 13 14 15)), :result '(3 13 15)}])
           (ts/trace-seq '(filter odd? (map inc num-seq))
                         {'num-seq [1 2 3 11 12 13 14]}))))
  
  (testing "Thread last macro"
    (let [result (seq [{:form '(map inc [1 2 3 11 12 13 14]), :result '(2 3 4 12 13 14 15)}
                       {:form '(filter even? (2 3 4 12 13 14 15)), :result '(2 4 12 14)}
                       {:form '(remove (fn [x] (< x 10)) (2 4 12 14)), :result '(12 14)}
                       {:form '(vector), :result []}
                       {:form '(into [] (12 14)), :result [12 14]}])]
      (is (= result (ts/trace-seq '(->> num-seq
                                        (map inc)
                                        (filter even?)
                                        (remove (fn [x] (< x 10)))
                                        (into (vector)))
                                  {'num-seq [1 2 3 11 12 13 14]})))
      (is (= result (ts/trace-seq '(into (vector)
                                         (->> num-seq
                                              (map inc)
                                              (filter even?)
                                              (remove (fn [x] (< x 10)))))
                                  {'num-seq [1 2 3 11 12 13 14]})))
      (is (= result (ts/trace-seq '(into (vector)
                                         (remove (fn [x] (< x 10))
                                                 (->> num-seq
                                                      (map inc)
                                                      (filter even?))))
                                  {'num-seq [1 2 3 11 12 13 14]}))))))
