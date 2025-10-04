(ns lambeaux.paper-trail.impl.executor.data-model
  (:require [lambeaux.paper-trail.impl.util :as ptu]
            [lambeaux.paper-trail.impl.lib :refer [assert*]]))

;; ------------------------------------------------------------------------------------------------
;; Data Model: Execution Context Builders
;; ------------------------------------------------------------------------------------------------

(defn new-call-ctx
  ([]
   (new-call-ctx :fn-in-execute nil))
  ([exec-ctx]
   (new-call-ctx :fn-on-stack exec-ctx))
  ([call-type exec-ctx]
   {:call-type call-type
    :exec-ctx (when (= :fn-on-stack call-type)
                (assert* (map? exec-ctx) "exec-ctx must be a map")
                exec-ctx)}))

(defn new-fn-ctx
  [commands]
  {:commands commands
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
   {:fn-idx 0
    :cmd-counter (atom -1)
    :fn-stack (if-not commands
                []
                [(new-fn-ctx commands)])
    :try-handlers (list)
    :is-throwing? false
    :is-finally? false
    :extracted-defs []
    :throwing-ex nil}))

(def allowed-fn-keys (into #{} (keys (new-fn-ctx nil))))
(def allowed-exec-keys (into #{} (keys (new-exec-ctx nil))))

(ptu/disallow-overlap! allowed-exec-keys allowed-fn-keys)

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
