;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.executor.data-model
  (:require [lambeaux.paper-trail.impl.executor.boxed-vals :as b]
            [lambeaux.paper-trail.impl.util :as ptu :refer [assert*]]))

;; ------------------------------------------------------------------------------------------------
;; Data Model: Execution Context Summary
;; ------------------------------------------------------------------------------------------------

(def default-keys
  [:ns-sym
   #_:extracted-defs
   #_:reports
   :cmd-counter
   :fn-idx
   #_:fn-stack
   #_:try-handlers
   :is-throwing?
   :is-finally?
   #_:throwing-ex
   :throwing-form])

(def default-fn-keys
  [:ns-caller
   :fn-meta
   #_:commands
   #_:command-history
   :call-stack
   #_:form-depth
   #_:finally-depth
   :source-scope
   :impl-scope
   :state])

(defn tiny-context
  "Creates a condensed view of an execution context."
  ([ctx]
   (tiny-context [] ctx))
  ([more-keys {:keys [fn-idx] :as ctx}]
   (let [{:keys [commands command-history] :as fctx} (get-in ctx [:fn-stack fn-idx])]
     (merge (select-keys ctx (concat default-keys more-keys))
            (select-keys fctx (concat default-fn-keys more-keys))
            {:command-meta {:preview-next (take 5 commands)
                            :preview-last (take 5 command-history)}}))))

;; ------------------------------------------------------------------------------------------------
;; Data Model: Exceptions
;; ------------------------------------------------------------------------------------------------

(defn ex-chain
  [ex]
  (->> (iterate ex-cause ex)
       (take-while identity)
       (mapv (fn [ex*]
               {:ex-class   (class ex*)
                :ex-message (ex-message ex*)
                :ex-data    (ex-data ex*)}))))

(defn ex->data
  ([ctx ex]
   (ex->data ctx ex false))
  ([ctx ex expected?]
   (assert (map? ctx) "ctx cannot be nil and must be a map")
   (assert ex "ex cannot be nil")
   (assert (boolean? expected?) "expected? cannot be nil and must be a boolean")
   (let [{cause-expected? :expected? cause-context :context-full} (ex-data ex)
         actual-expected (first (keep identity [cause-expected? expected?]))
         actual-context  (first (keep identity [(b/box-value cause-context) ctx]))]
     {:expected?    actual-expected
      :context      (tiny-context actual-context)
      :context-full (b/val->hidden actual-context)
      :cause-chain  (ex-chain ex)})))

;; ------------------------------------------------------------------------------------------------
;; Data Model: Execution Context Builders
;; ------------------------------------------------------------------------------------------------

(defn new-fn-ctx
  [ns-caller commands]
  {:ns-caller ns-caller
   :fn-meta nil
   :commands commands
   :command-history (list)
   :call-stack (list)
   :form-depth -1
   :finally-depth 0
   :source-scope {}
   :impl-scope {}
   :state {}})

(defn new-exec-ctx
  ([]
   (new-exec-ctx nil))
  ([commands]
   (new-exec-ctx nil commands))
  ([ns-sym commands]
   (let [ns-caller (if ns-sym
                     ns-sym
                     (ns-name *ns*))
         idx (if-not commands
               -1
               0)
         stack (if-not commands
                 []
                 [(new-fn-ctx ns-caller commands)])]
     {:ns-sym ns-caller
      :extracted-defs []
      :reports []
      :cmd-counter (atom -1)
      :fn-idx idx
      :fn-stack stack
      :try-handlers (list)
      :is-throwing? false
      :is-finally? false
      :throwing-ex nil
      :throwing-form nil})))

(def allowed-fn-keys (into #{} (keys (new-fn-ctx nil nil))))
(def allowed-exec-keys (into #{} (keys (new-exec-ctx nil))))

(ptu/disallow-overlap! allowed-exec-keys allowed-fn-keys)

(defn new-call-ctx
  ([]
   (new-call-ctx :fn-in-execute nil))
  ([exec-ctx]
   (new-call-ctx :fn-on-stack exec-ctx))
  ([call-type exec-ctx]
   {:call-type call-type
    :fn-meta nil
    :exec-ctx (when (= :fn-on-stack call-type)
                (assert* (map? exec-ctx) "exec-ctx must be a map")
                exec-ctx)}))

;; ------------------------------------------------------------------------------------------------
;; Structs: Execution Context Updaters
;; ------------------------------------------------------------------------------------------------

(comment
  "TODO (Later): Come back and revisit some cases regarding 'apply'"
  ;; does not use varadic
  (apply update-in [{:k []}
                    [:k 0]
                    (fn [m & args] (apply assoc m [:x 1 :args args]))]))

(defn with-keyvals
  [& kvs]
  (fn [m] (apply assoc m kvs)))

(defn update-fn-ctx
  [{:keys [fn-idx] :as ctx} f & args]
  (apply update-in (concat [ctx [:fn-stack fn-idx] f] args)))

(defn next-command
  [{:keys [cmd-counter command-history]
    [cmd & cmds] :commands
    :as ctx}]
  (let [cmd* (assoc cmd :cmd-idx (swap! cmd-counter inc))]
    (update-fn-ctx ctx (with-keyvals
                         :commands cmds
                         :command-history (conj command-history cmd*)))))

;; todo: fix default-update's inability to handle an empty vector of kvs
(defn default-update
  ([ctx kvs]
   (default-update ctx true kvs))
  ([ctx consume-command? kvs]
   (cond-> ctx
     consume-command? next-command
     true (update-fn-ctx (apply with-keyvals kvs)))))

(defn process-no-op
  [ctx]
  (next-command ctx))
