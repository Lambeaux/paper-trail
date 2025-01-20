(ns lambeaux.paper-trail.repl
  (:require [lambeaux.paper-trail.decomp :as ptd]))

(def default-requires
  ['[lambeaux.paper-trail.repl :as r]
   '[lambeaux.paper-trail.classpath :as cp]
   '[lambeaux.paper-trail.decomp :as impl]])

(defn require-pt
  []
  (apply require default-requires))

(defmacro with-std-out
  [& body]
  `(binding [*out* (java.io.PrintWriter. System/out)]
     ~@body))

(comment

  (def my-try-form
    '(let [x (atom 1)]
       (try
         (swap! x inc)
         x
         (catch Exception e nil)
         (finally (swap! x inc)))))

  (ptd/run-eval my-try-form))

(comment
  (require '[portal.api :as p])
  (def p (p/open {:launcher :vs-code}))
  (add-tap #'p/submit))
