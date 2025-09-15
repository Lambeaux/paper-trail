;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.alpha.api
  (:require [lambeaux.paper-trail.impl.core :as impl]))

(def ^:dynamic *trace-opts* {})

(defmacro trace-fn
  [f & args]
  (assert (symbol? f)
          "f must be a symbol that resolves to a fn var or value")
  (let [ns-local *ns*
        fn-symbol-str (pr-str f)]
    `(impl/trace-fn* ~ns-local ~fn-symbol-str *trace-opts* (list ~@args))))

(defn evaluate
  "Like clojure.core/eval but uses the paper-trail interpreter to produce
   the result."
  [form]
  (impl/evaluate form))
