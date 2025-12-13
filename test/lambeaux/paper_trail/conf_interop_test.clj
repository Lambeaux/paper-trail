(ns lambeaux.paper-trail.conf-interop-test
  (:require [clojure.test :as t :refer [deftest]]
            [lambeaux.paper-trail.conf-core :as conf]))

(deftest test-simple-dot-forms
  (conf/forms->test "simple dot forms"
    (. "hello" contains "he")))
