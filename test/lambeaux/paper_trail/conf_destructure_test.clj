(ns lambeaux.paper-trail.conf-destructure-test
  (:require [clojure.test :as t :refer [deftest]]
            [lambeaux.paper-trail.conf-core :as conf]))

#_(deftest test-simple-destructure-forms
    #_(conf/forms->test "error destructure forms"
        (let [[] (hash-map)] (vector nil))
        (let [[x] (hash-map)] (vector x))
        (let [[x y] (hash-map)] (vector x y))
        (let [[x y z] (hash-map)] (vector x y z)))

    (conf/forms->test "simple sequential destructuring forms"
      (let [[] (vector)] nil)
      (let [[x] (vector)] (vector x))
      (let [[x y] (vector)] (vector x y))
      (let [[x y z] (vector)] (vector x y z))

      (let [[x] (vector 1)] (vector x))
      (let [[x y] (vector 1)] (vector x y))
      (let [[x y z] (vector 1)] (vector x y z))

      (let [[x] (vector 1 2)] (vector x))
      (let [[x y] (vector 1 2)] (vector x y))
      (let [[x y z] (vector 1 2)] (vector x y z))

      (let [[x] (vector 1 2 3)] (vector x))
      (let [[x y] (vector 1 2 3)] (vector x y))
      (let [[x y z] (vector 1 2 3)] (vector x y z))

      (let [[& more] (vector)] (vector more))
      (let [[x & more] (vector)] (vector x more))
      (let [[x y & more] (vector)] (vector x y more))
      (let [[x y z & more] (vector)] (vector x y z more))

      (let [[& more] (vector 1)] (vector more))
      (let [[x & more] (vector 1)] (vector x more))
      (let [[x y & more] (vector 1)] (vector x y more))
      (let [[x y z & more] (vector 1)] (vector x y z more))

      (let [[& more] (vector 1 2)] (vector more))
      (let [[x & more] (vector 1 2)] (vector x more))
      (let [[x y & more] (vector 1 2)] (vector x y more))
      (let [[x y z & more] (vector 1 2)] (vector x y z more))

      (let [[& more] (vector 1 2 3)] (vector more))
      (let [[x & more] (vector 1 2 3)] (vector x more))
      (let [[x y & more] (vector 1 2 3)] (vector x y more))
      (let [[x y z & more] (vector 1 2 3)] (vector x y z more))

      (let [[& more] (vector 1 2 3 4)] (vector more))
      (let [[x & more] (vector 1 2 3 4)] (vector x more))
      (let [[x y & more] (vector 1 2 3 4)] (vector x y more))
      (let [[x y z & more] (vector 1 2 3 4)] (vector x y z more))

      (let [[& more] (vector 1 2 3 4 5)] (vector more))
      (let [[x & more] (vector 1 2 3 4 5)] (vector x more))
      (let [[x y & more] (vector 1 2 3 4 5)] (vector x y more))
      (let [[x y z & more] (vector 1 2 3 4 5)] (vector x y z more))

      (let [[:as coll] (vector)] (vector coll))
      (let [[:as coll] (vector 1 2 3)] (vector coll))
      (let [[x :as coll] (vector 1 2 3)] (vector x coll))
      (let [[x y :as coll] (vector 1 2 3)] (vector x y coll))
      (let [[x y z :as coll] (vector 1 2 3)] (vector x y z coll))
      (let [[x y z t :as coll] (vector 1 2 3)] (vector x y z t coll)))

    #_(conf/forms->test "simple associative destructuring forms"
        ()))
