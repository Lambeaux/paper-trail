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

(comment (refresh))

(comment "How to run the old interpreter versions for comparison purposes."

         (require '[lambeaux.paper-trail.misc.impl-v1-rdi :as ptv1]
                  '[lambeaux.paper-trail.misc.impl-v2-btsi :as ptv2]
                  '[lambeaux.paper-trail.misc.samples :as samples])

         (ptv1/trace* 'samples/simplest-even
                      [1 2 3 4 5 6 7])

         (ptv2/trace-seq '(mapv inc (filter odd? (range 10)))
                         {}))
