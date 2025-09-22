;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns user
  (:require [clojure.tools.namespace.repl :as tnr]
            [lambeaux.paper-trail.repl :as r]))

(r/require-pt)

(defn refresh []
  (r/with-std-out
    (println "Reloading Clojure on classpath")
    (let [result (tnr/refresh)]
      (println "Reloading done")
      result)))

(defn refresh-all []
  (r/with-std-out
    (println "Reloading Clojure on classpath")
    (let [result (tnr/refresh-all)]
      (println "Reloading done")
      result)))

(comment
  (refresh)
  (refresh-all))

(comment "How to run the old interpreter versions for comparison purposes."

         (require '[lambeaux.paper-trail.misc.impl-v1-rdi :as ptv1]
                  '[lambeaux.paper-trail.misc.impl-v2-btsi :as ptv2]
                  '[lambeaux.paper-trail.misc.samples :as samples])

         (ptv1/trace* 'samples/simplest-even
                      [1 2 3 4 5 6 7])

         (ptv2/trace-seq '(mapv inc (filter odd? (range 10)))
                         {}))
