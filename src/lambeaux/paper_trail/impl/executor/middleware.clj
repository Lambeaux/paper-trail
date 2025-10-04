(ns lambeaux.paper-trail.impl.executor.middleware
  (:require [lambeaux.paper-trail.impl.executor.data-model :as model]
            [lambeaux.paper-trail.impl.util :as ptu]))

;; ------------------------------------------------------------------------------------------------
;; Processors: Middleware
;; ------------------------------------------------------------------------------------------------

(def ^:dynamic *enable-verbose-logging* false)

(defn wrap-verbose-logging
  [handler]
  (fn [{[{:keys [action] :as cmd}] :commands :as ctx}]
    (when *enable-verbose-logging*
      (println "Handling " action " for command: " (pr-str cmd)))
    (handler ctx)))

(def allowed-op-when-throwing?
  #{:stack-push-frame
    :stack-pop-frame
    :unbind-name
    :cleanup-try
    :set-context
    :begin-finally
    :end-finally})

(defn wrap-throwing
  [handler]
  (fn [{:keys [is-throwing? is-finally?] [{:keys [action]}] :commands :as ctx}]
    (if (and is-throwing?
             (not is-finally?)
             (not (allowed-op-when-throwing? action)))
      (model/process-no-op ctx)
      (handler ctx))))

(defn wrap-check-not-infinite
  [handler]
  (fn [ctx]
    (let [ctx* (handler ctx)
          pre-idx (:fn-idx ctx)
          post-idx (:fn-idx ctx*)
          pre-handler-cmds (get-in ctx [:fn-stack pre-idx :commands])
          post-handler-cmds (get-in ctx* [:fn-stack post-idx :commands])]
      ;; TODO: what happens when the commands happen to be equal but the fn-idx changed?
      ;; likely will never happen even in a true recursive call because SOME of the pre-idx commands
      ;; HAD to be processed in order to invoke the interpreted fn at post-idx
      (if-not (= pre-handler-cmds post-handler-cmds)
        ctx*
        (throw (new RuntimeException
                    "Infinite processing error detected: handler did not update commands"))))))

(defn wrap-convenience-mappings
  [handler]
  (fn [{:keys [fn-idx] :as ctx}]
    (let [fctx (get-in ctx [:fn-stack fn-idx])]
      (ptu/disallow-overlap! (into #{} (keys ctx))
                             (into #{} (keys fctx)))
      (apply dissoc
             (handler (merge ctx fctx))
             (keys fctx)))))

(defn wrap-uncaught-ex
  [handler]
  (fn [ctx]
    (try
      (handler ctx)
      (catch Exception ex
        (throw (ex-info "Uncaught interpreter exception"
                        {:ctx ctx :cause-data (ex-data ex)}
                        ex))))))

(defn wrap-command-middleware
  ([handler]
   (wrap-command-middleware true handler))
  ([wrap-infinite? handler]
   (cond-> handler
     true           wrap-throwing
     wrap-infinite? wrap-check-not-infinite
     true           wrap-verbose-logging
     true           wrap-convenience-mappings
     true           wrap-uncaught-ex)))

;; (wrap-command-middleware false f)
(defn run-with
  [f ctx & args]
  (apply (wrap-convenience-mappings f)
         (select-keys ctx model/allowed-exec-keys)
         args))

(defn inject-handlers
  ([all-handlers]
   (inject-handlers {} all-handlers))
  ([{:as _opts} all-handlers]
   (reduce (fn [m k]
             (update m k wrap-command-middleware))
           all-handlers
           (keys all-handlers))))
