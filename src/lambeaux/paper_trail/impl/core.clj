;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.core
  (:require [lambeaux.paper-trail.impl.generator :as ptg]
            [lambeaux.paper-trail.impl.executor :as pte]
            [lambeaux.paper-trail.impl.util :as ptu]
            [lambeaux.paper-trail.impl.classpath :as ptc]
            [lambeaux.paper-trail.impl.executor.data-model :as model])
  (:import [clojure.lang IObj Compiler$CompilerException]))

(def default-line-or-column 0)

(defn ex-unresolvable-namespace
  [fpath-str sym]
  (Compiler$CompilerException.
   fpath-str
   default-line-or-column
   default-line-or-column
   nil
   :compile-syntax-check
   (ClassNotFoundException. (name sym))))

(defn ex-unresolvable-symbol
  [fpath-str sym]
  (Compiler$CompilerException.
   fpath-str
   default-line-or-column
   default-line-or-column
   nil
   :compile-syntax-check
   (RuntimeException. (str "Unable to resolve symbol: " (name sym) " in this context"))))

(defn trace-original*
  ([src-ns symbol-str arg-seq]
   (trace-original* src-ns symbol-str {} arg-seq))
  ([src-ns symbol-str opts arg-seq]
   (if-let [fvar (resolve (symbol symbol-str))]
     (apply (deref fvar) arg-seq)
     (let [[sym-ns _] (ptu/sym-split (symbol symbol-str))]
       (throw
        (if (ptu/ns-exists? sym-ns)
          ;; This works fine, but ...
          ;; TODO: Not a fan of trying to match Clojure's errors this closely, will likely
          ;;   extract the core error reasons and make this much simpler (e.g. will make some
          ;;   improvements to conf/ex-comparable for testing).
          (ex-unresolvable-symbol *file* symbol-str)
          (ex-unresolvable-namespace *file* symbol-str)))))))

(defonce ns-idx
  (delay (atom (ptc/ns-index))))

(defn ns-load*
  [{:keys [ns-location] :as ns-cache}]
  (when ns-cache
    (let [{:keys [ns-id forms] :as location*} (ptc/ns-load ns-location)]
      (assoc ns-cache
             :ns-location location*
             :ns-vars (->> forms
                           (map (partial ptg/create-commands ns-id))
                           (mapcat pte/execute-for-defs)
                           (map (fn [[k v]]
                                  (vector (last (ptu/sym-split k)) v)))
                           (into {}))))))

;; todo: resolve ns aliases in symbol str
;; todo: only call ns-load* when the code has changed
;; todo: add nrepl middleware for detecting when vars are redef'd in the repl separate from source
(defn trace-fn*
  ([src-ns symbol-str arg-seq]
   (trace-fn* src-ns symbol-str {} arg-seq))
  ([src-ns symbol-str _opts arg-seq]
   (let [[sym-ns sym-name] (ptu/sym-split (symbol symbol-str))
         ns-cache (ns-load* (get @(force ns-idx) sym-ns))
         f (when ns-cache
             (get-in ns-cache [:ns-vars sym-name]))]
     (if-not (fn? f)
       (throw (IllegalArgumentException. "f is not a fn"))
       (as-> (model/new-exec-ctx) $
         (model/new-call-ctx $)
         (apply f $ arg-seq)
         (pte/execute-ctx $)
         (pte/fn-stack-pop $))))))

;; ----------------------------------------------------------------------------------------

(defn evaluate
  ([form]
   (let [cmds (ptg/create-commands form)]
     (pte/execute cmds))))

(defn evaluate-to
  ([idx form]
   (let [cmds (ptg/create-commands form)]
     (pte/execute cmds idx))))

(defn evaluate-debug
  ([form]
   (evaluate-debug form identity))
  ([form xform]
   (let [cmds (ptg/create-commands form)]
     (->> cmds
          (pte/ctx-seq)
          (map (fn [{:keys [fn-idx throwing-ex is-throwing? is-finally?] :as ctx}]
                 (-> (get-in ctx [:fn-stack fn-idx])
                     (assoc
                      :is-throwing? is-throwing?
                      :is-finally? is-finally?
                      :throwing-ex (boolean throwing-ex)))))
          (map (fn [{:keys [commands] :as ctx}]
                 (-> ctx
                     (assoc :next-command (first commands))
                     (dissoc :command-history :commands))))
          (map (fn [ctx]
                 (update-vals ctx #(if-not (instance? IObj %)
                                     %
                                     (with-meta % {:portal.viewer/default
                                                   :portal.viewer/pprint})))))
          (xform)
          (into (with-meta [] {:portal.viewer/default
                               :portal.viewer/table}))))))

;; ----------------------------------------------------------------------------------------

(comment
  (evaluate '(+ 1 1))
  (evaluate-to 0 '(+ 1 1))
  (evaluate-debug '(+ 1 1)))
