;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.repl
  (:require [lambeaux.paper-trail.impl.core :as impl]))

(def default-requires
  ['[lambeaux.paper-trail.repl :as r]
   '[lambeaux.paper-trail.impl.core :as impl]
   '[lambeaux.paper-trail.impl.generator :as ptg]
   '[lambeaux.paper-trail.impl.executor :as pte]])

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

  (impl/evaluate my-try-form))

(comment
  (require '[portal.api :as p])
  (def p (p/open {:launcher :vs-code}))
  (add-tap #'p/submit))
