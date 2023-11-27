(ns lambeaux.paper-trail.repl)

(comment
  (require '[portal.api :as p])
  (def p (p/open {:launcher :vs-code}))
  (add-tap #'p/submit))