(ns lambeaux.paper-trail.alpha.api
  (:require [lambeaux.paper-trail.impl.core :as impl]))

(defn evaluate
  "Like clojure.core/eval but uses the paper-trail interpreter to produce
   the result."
  [form]
  (impl/evaluate form))
