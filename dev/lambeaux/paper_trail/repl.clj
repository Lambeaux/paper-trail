;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.repl
  (:require [clojure.pprint :as pp]
            [lambeaux.paper-trail.impl.core :as impl]
            [lambeaux.paper-trail.impl.executor.middleware :as middleware]
            [lambeaux.paper-trail.impl.generator :as ptg]))

(def default-requires
  ['[lambeaux.paper-trail.repl :as r]
   '[lambeaux.paper-trail.impl.core :as impl]
   '[lambeaux.paper-trail.impl.generator :as ptg]
   '[lambeaux.paper-trail.impl.util :as ptu]
   '[lambeaux.paper-trail.impl.executor :as pte]
   '[lambeaux.paper-trail.impl.executor.call-stack :as stack]
   '[lambeaux.paper-trail.impl.executor.data-model :as model]
   '[lambeaux.paper-trail.impl.executor.middleware :as middleware]
   '[lambeaux.paper-trail.conf-core :as conf]])

(defn require-pt
  []
  (apply require default-requires))

(defmacro with-std-out
  [& body]
  `(binding [*out* (java.io.PrintWriter. System/out)
             *err* (java.io.PrintWriter. System/err)]
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
(defn with-executor-middleware
  [f & args]
  (let [f* (middleware/wrap-command-middleware f)]
    (apply f* args)))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn with-eval
  ([form]
   (impl/evaluate form))
  ([idx form]
   (tiny-context (impl/evaluate-to idx form))))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn spit-commands
  ([fname form]
   (spit-commands fname identity form))
  ([fname xform form]
   (let [xform* (comp #(into (sorted-map) (seq %)) xform)
         spit-seq (fn [fname* coll]
                    (doseq [item coll]
                      (let [content* (with-out-str (pp/pprint (xform* item)))]
                        (spit fname* content* :append true))))]
     (if-let [working-dir (System/getProperty "user.dir")]
       (spit-seq (str working-dir "/.tmp/" (name fname) ".edn")
                 (ptg/create-commands form))
       (throw (IllegalStateException. "No user.dir system property provided"))))))

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
