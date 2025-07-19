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
