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
            [lambeaux.paper-trail.impl.executor.data-model :as model]
            [lambeaux.paper-trail.impl.executor.middleware :as middleware]
            [lambeaux.paper-trail.impl.generator :as ptg]
            [lambeaux.paper-trail.impl.executor :as pte]))

(def default-thirdparty-requires
  ['[clojure.java.classpath :as jc]
   '[clojure.string :as str]
   '[clojure.java.io :as io]
   '[clojure.walk :as w]
   '[edamame.core :as ed]])

(def default-project-requires
  ['[lambeaux.paper-trail.alpha.api :as pt]
   '[lambeaux.paper-trail.repl :as r]
   '[lambeaux.paper-trail.impl.core :as impl]
   '[lambeaux.paper-trail.impl.classpath :as ptc]
   '[lambeaux.paper-trail.impl.generator :as ptg]
   '[lambeaux.paper-trail.impl.util :as ptu]
   '[lambeaux.paper-trail.impl.executor :as pte]
   '[lambeaux.paper-trail.impl.executor.call-stack :as stack]
   '[lambeaux.paper-trail.impl.executor.data-model :as model]
   '[lambeaux.paper-trail.impl.executor.middleware :as middleware]
   '[lambeaux.paper-trail.conf-core :as conf]
   '[lambeaux.paper-trail.wip.edamame :as wed]
   '[lambeaux.paper-trail.wip.interop :as wiop]])

(defn require-pt
  []
  (apply require (concat default-project-requires
                         default-thirdparty-requires)))

(defmacro with-std-out
  [& body]
  `(binding [*out* (java.io.PrintWriter. System/out)
             *err* (java.io.PrintWriter. System/err)]
     ~@body))

(defn emit-msg*
  "Just a test fn for checking pt's invocation facilities."
  [msg]
  (with-std-out (println msg)
    msg))

(defn say-hello*
  "Just a test fn for checking pt's invocation facilities."
  [coll]
  (let [tar-most (butlast coll)
        tar-last (last coll)
        msg (apply str (concat (list "Hello ")
                               (interpose ", " tar-most)
                               (list ", and " tar-last "!")))]
    (emit-msg* msg)))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn say-hello
  "Just a test fn for checking pt's invocation facilities."
  ([]
   (emit-msg* "Hello!"))
  ([target]
   (emit-msg* (str "Hello " target "!")))
  ([tx ty]
   (say-hello* (list tx ty)))
  ([tx ty tz & more]
   (say-hello* (concat [tx ty tz] more))))

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
(defmacro with-ex-val
  "Return an exception as if it was a value."
  [& body]
  (let [ex-sym (gensym "e")]
    `(try
       ~@body
       (catch Exception ~ex-sym ~ex-sym))))

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
                 (ptg/generate form))
       (throw (IllegalStateException. "No user.dir system property provided"))))))

(comment ;; Example test hook

  "Example use case for test hook: "
  (let [f (fn [{:keys [fn-idx] :as ctx}]
            (-> ctx
                model/next-command
                (assoc-in [:fn-stack fn-idx :is-throwing?] true)))
        test-hook {:action :test-hook :hook-fn f}
        cmds (ptg/generate '(+ 1 1))
        cmds* (concat (butlast cmds)
                      [test-hook (last cmds)])]
    (pte/execute cmds*)))

(comment ;; Example try form

  (def my-try-form
    '(let [x (atom 1)]
       (try
         (swap! x inc)
         x
         (catch Exception e nil)
         (finally (swap! x inc)))))

  (impl/evaluate my-try-form))

(comment ;; Setting up Portal

  (require '[portal.api :as p])
  (def p (p/open {:launcher :vs-code}))
  (add-tap #'p/submit))
