(ns lambeaux.paper-trail.impl.classpath-test
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.string :as str]
            [lambeaux.paper-trail.conf-core :as conf]
            [lambeaux.paper-trail.impl.classpath :as ptc]))

;; todo: if we end up writing a lot of these types of tests, maybe replace 'bob' and 'greg'
;; with some form of a gensym
(def test-script-peer
  "(ns lambeaux.paper-trail.testns.greg)
   (defn peer-with-output
     [the-val]
     (println \"Here is some output from the peer\")
     the-val)")

(def test-script-local
  "(ns lambeaux.paper-trail.testns.bob
     (:require [lambeaux.paper-trail.testns.greg :as greg]))
   (defn val-with-output
     [the-val]
     (println \"Here is some output from me\")
     the-val)
   (defmacro delegate-to-local
     [message]
     `(val-with-output ~message))
   (defmacro delegate-to-peer
     [message]
     `(greg/peer-with-output ~message))
   (defmacro delegate-to-core
     [message]
     `(vector ~message))")

(def no-src-file "NO_SOURCE_FILE")

(defn is-not-empty
  [coll]
  (is (seq coll) "coll cannot be empty")
  coll)

(deftest test-form-loading-handles-syntax-quote
  (conf/load-ns-for-test!
   (str/join (System/lineSeparator)
             [test-script-peer test-script-local]))
  (let [form-map (ptc/read-forms no-src-file
                                 (conf/str->input-stream test-script-local))
        {:keys [file forms] ns-name* :ns-name} form-map]
    (is (= no-src-file file))
    (is (= 'lambeaux.paper-trail.testns.bob ns-name*))
    (is (= true (every? #(= "lambeaux.paper-trail.testns.bob" (namespace %))
                        (is-not-empty
                         (conf/gather (every-pred symbol? #(= "val-with-output" (name %)))
                                      (nth forms 1))))))
    (is (= true (every? #(= "lambeaux.paper-trail.testns.greg" (namespace %))
                        (is-not-empty
                         (conf/gather (every-pred symbol? #(= "peer-with-output" (name %)))
                                      (nth forms 2))))))
    (is (= true (every? #(= "clojure.core" (namespace %))
                        (is-not-empty
                         (conf/gather (every-pred symbol? #(= "vector" (name %)))
                                      (nth forms 3))))))))
