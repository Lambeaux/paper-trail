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
   '[lambeaux.paper-trail.impl.executor :as pte]
   '[lambeaux.paper-trail.impl.util :as ptu]
   '[lambeaux.paper-trail.conf-test :as ptconf]])

(defn require-pt
  []
  (apply require default-requires))

(defmacro with-std-out
  [& body]
  `(binding [*out* (java.io.PrintWriter. System/out)]
     ~@body))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn decompose-form
  [form]
  (cond (vector? form)
        {:event :vector :children (map decompose-form form)}
        (sequential? form)
        (let [args (rest form)]
          {:event :invoke
           :op (first form)
           :form form
           :arg-count (count args)
           :children (map decompose-form args)})
        :else
        {:event :scalar :value form}))

(defn tiny-context
  [{:keys [fn-idx] :as ctx}]
  (let [fctx (get-in ctx [:fn-stack fn-idx])
        commands (:commands fctx)]
    (merge (select-keys ctx [:is-throwing? :is-finally?])
           (select-keys fctx [:source-scope :call-stack-primary :call-stack-finally])
           {:commands (take 15 commands)})))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn with-eval-to
  [idx form]
  (tiny-context (impl/evaluate-to idx form)))

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
