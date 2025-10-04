;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.executor
  (:require [lambeaux.paper-trail.impl.executor.call-stack :as stack]
            [lambeaux.paper-trail.impl.executor.data-model :as model]
            [lambeaux.paper-trail.impl.executor.middleware :as middleware]
            [lambeaux.paper-trail.impl.generator :as ptg]
            [lambeaux.paper-trail.impl.util :as ptu]
            [lambeaux.paper-trail.impl.lib :refer [assert*]]
            [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang LazySeq ArityException]))

;; ------------------------------------------------------------------------------------------------
;; Processors: Call Stack
;; ------------------------------------------------------------------------------------------------

(defn stack-find
  [{:keys [fn-idx] :as ctx}]
  (get-in ctx [:fn-stack fn-idx :call-stack]))

(defn process-stack-push-frame
  [{:keys [call-stack] :as ctx}]
  (model/default-update ctx [:call-stack (stack/frame-push call-stack)]))

(defn process-stack-pop-frame
  [{:keys [is-throwing? is-finally? call-stack] :as ctx}]
  (if (or is-finally? (not is-throwing?))
    (model/next-command ctx)
    (model/default-update ctx [:call-stack (stack/frame-pop call-stack)])))

;; ------------------------------------------------------------------------------------------------
;; Processors: Function Stack
;; ------------------------------------------------------------------------------------------------

;; Note: probably don't need the middleware for a push, only exec-ctx fields are relevant
(defn fn-stack-push
  [ctx commands]
  (-> ctx
      (update :fn-idx inc)
      (update :fn-stack #(conj % (model/new-fn-ctx commands)))))

(defn fn-stack-do-pop*
  [{:keys [fn-idx] :as ctx}]
  (assert* (> fn-idx 0)
           (str "Fn stack cannot be popped unless fn-idx > 0 but fn-idx is "
                fn-idx))
  (let [ctx* (-> ctx
                 (update :fn-idx dec)
                 (update :fn-stack pop))]
    (middleware/run-with model/next-command ctx*)))

(defn fn-stack-resolve*
  [ctx result]
  (let [convey-fn (fn [{:keys [call-stack] :as ctx*}]
                    (model/default-update
                     ctx* false [:call-stack (stack/push-resolved-val call-stack result)]))]
    (middleware/run-with convey-fn ctx)))

(defn fn-stack-pop
  [{:keys [fn-idx throwing-ex is-throwing?] :as ctx}]
  (let [call-stack (stack-find ctx)]
    (if (zero? fn-idx)
      (if is-throwing?
        (throw throwing-ex)
        (stack/peek-val call-stack))
      (if is-throwing?
        (fn-stack-do-pop* ctx)
        (let [return-val (stack/peek-val call-stack)]
          (-> ctx
              (fn-stack-do-pop*)
              (fn-stack-resolve* return-val)))))))

;; ------------------------------------------------------------------------------------------------
;; Processors: Function Objects
;; ------------------------------------------------------------------------------------------------

(declare execute)

(defn copy-scope-cmds
  ([keyvals]
   (copy-scope-cmds false keyvals))
  ([in-macro? keyvals]
   (map (fn [[k v]]
          {:action :bind-name
           :bind-id k
           :bind-from :command-value
           :value (peek v)
           :in-macro? in-macro?})
        (seq keyvals))))

(defn process-create-fn
  [{:keys [call-stack source-scope impl-scope]
    [{:keys [arities]} & _] :commands
    :as ctx}]
  (let [the-fn (let [scope-cmds (concat
                                 (copy-scope-cmds source-scope)
                                 (copy-scope-cmds true impl-scope))]
                 (fn [obj-name {:keys [call-type exec-ctx]} & args]
                   (let [arg-count (count args)
                         ks (map #(str "arg-" %) (range arg-count))
                         kvs (into {} (map vector ks args))]
                     (if-let [{:keys [commands] :as _metainf} (get arities arg-count)]
                       (let [all-cmds (concat [{:action :assoc-state :kvs kvs}] scope-cmds commands)]
                         (case call-type
                           :fn-in-execute (execute all-cmds)
                           :fn-on-stack   (fn-stack-push exec-ctx all-cmds)))
                       (throw (ArityException. arg-count obj-name))))))
        ;; todo: instead of (str the-fn) inject proper location metadata from source
        ;;   the technique will vary depending if the fn is a var or local binding
        the-fn* (with-meta (partial the-fn (str the-fn))
                  {::pt/fn-impl true})]
    (model/default-update ctx [:call-stack (stack/push-val call-stack the-fn*)])))

;; ------------------------------------------------------------------------------------------------
;; Processors: Function Handlers
;; ------------------------------------------------------------------------------------------------

(def temp-ctx {::pt/current-ns *ns*})

;; TODO clean up all this messy code that concerns
;; symbol and namespace resolution
(defn map-impl-fn [ctx]
  (fn [arg]
    (cond
      ;; needed this check because lazy ex's were getting
      ;; swallowed by accessible-fn? and ->impl
      (instance? LazySeq arg) arg
      (ptu/accessible-fn? ctx arg) (ptu/->impl ctx arg)
      ;; note: temporary limitation: for now, fns passed as arguments must be invoked 
      ;; as fn-in-execute and not as fn-on-stack, because return vals are expected, not
      ;; returned context
      (and (fn? arg) (::pt/fn-impl (meta arg))) (partial arg (model/new-call-ctx))
      :else arg)))

(defn invoke-opaque-fn
  [{:keys [throwing-ex is-throwing? is-finally? call-stack] :as ctx}
   fn-impl]
  (let [[ex? result] (try
                       (let [return (apply fn-impl
                                           (map (map-impl-fn temp-ctx)
                                                (stack/peek-frame call-stack)))]
                         ;; [false (if-not (seq? return) return (doall return))]
                         ;; Turns out, requiring doall here was a red herring. The real
                         ;; failure was occurring down below when trying to with-meta a
                         ;; lazy, unrealized seq. 
                         [false return])
                       ;; JVM Note: typical applications should not catch throwable
                       ;; if you copy this pattern, make sure you know what you're doing
                       ;; (catch Throwable t [true t])
                       ;; TODO: eventually support Throwable for more accurate results
                       (catch Exception e [true e]))
        #_#_final-form (map (fn [arg]
                              (or (and (instance? IObj arg)
                                       (::pt/raw-form (meta arg)))
                                  arg))
                            (stack-take arg-count call-stack))]
    (-> ctx
        (assoc
         :is-throwing? (boolean (or is-throwing? ex?))
         :is-finally? (if ex? false is-finally?)
         :throwing-ex (if ex? result throwing-ex))
        (model/default-update
         [:call-stack (if ex?
                        call-stack
                        (stack/push-resolved-val call-stack result))]))))

(defn invoke-interpreted-fn
  [{:keys [call-stack] :as ctx} fn-impl]
  (apply fn-impl
         (model/new-call-ctx ctx)
         (map (map-impl-fn temp-ctx)
              (stack/peek-frame call-stack))))

(defn process-invoke-fn
  [{:keys [source-scope impl-scope] [cmd & _] :commands :as ctx}]
  (let [fn-symbol (:op cmd)
        fn-impl   (or (peek (get source-scope fn-symbol))
                      (peek (get impl-scope fn-symbol))
                      (ptu/->impl temp-ctx fn-symbol))
        pt-fn?    (boolean (::pt/fn-impl (meta fn-impl)))]
    (if pt-fn?
      (invoke-interpreted-fn ctx fn-impl)
      (invoke-opaque-fn ctx fn-impl))))

(defn process-invoke-throw
  [{:keys [call-stack] :as ctx}]
  (-> ctx
      (assoc
       :is-throwing? true
       :is-finally? false
       :throwing-ex (stack/peek-val call-stack))
      ;; todo: fix model/default-update's inability to handle an empty vector of kvs
      (model/default-update [:call-stack call-stack])))

(defn process-invoke-do
  [{:keys [call-stack] [{:keys [convey-result?]}] :commands :as ctx}]
  (model/default-update ctx [:call-stack (if convey-result?
                                           (stack/push-resolved-val call-stack)
                                           (stack/frame-pop call-stack))]))

(defn process-invoke-def
  "Interpreted defs do not have side effects."
  [{:keys [call-stack] [{:keys [arg-count] :as _cmd} & _] :commands :as ctx}]
  (let [[name-sym :as args] (stack/peek-frame call-stack)
        def-result (find-var name-sym)]
    (cond-> ctx
      (> arg-count 1) (update :extracted-defs conj [name-sym (last args)])
      true (model/default-update [:call-stack (stack/push-resolved-val call-stack def-result)]))))

;; ------------------------------------------------------------------------------------------------
;; Processors: Terminal Value Handlers
;; ------------------------------------------------------------------------------------------------

(defn process-scalar
  [{:keys [call-stack source-scope impl-scope]
    [cmd & _] :commands
    :as ctx}]
  (let [scalar-form (:form cmd)
        scalar-value (if-not (:eval? cmd)
                       scalar-form
                       (or (peek (get source-scope scalar-form))
                           (peek (get impl-scope scalar-form))
                           scalar-form))
        scalar-value (case scalar-value
                       ::pt/nil nil
                       ::pt/false false
                       scalar-value)]
    (model/default-update ctx [:call-stack (stack/push-val call-stack scalar-value)])))

;; ------------------------------------------------------------------------------------------------
;; Processors: State & Scope Handlers
;; ------------------------------------------------------------------------------------------------

(defn process-capture-state
  [{:keys [call-stack state]
    [cmd & _] :commands
    :as ctx}]
  ;; todo: should we pop the call-stack in this scenario?
  ;; no, we should not (but awaiting test case verification)
  (model/default-update ctx [:state (assoc state (:state-id cmd) (stack/peek-val call-stack))]))

(defn process-bind-name
  [{:keys [state call-stack source-scope impl-scope]
    [{:keys [bind-id bind-from in-macro? value state-id]} & _] :commands
    :as ctx}]
  (let [val-to-bind  (case bind-from
                       :command-value value
                       :call-stack (stack/peek-val call-stack)
                       :state (get state state-id))
        val-to-bind  (if-not (stack/is-realized-val? val-to-bind)
                       val-to-bind
                       (case val-to-bind
                         nil ::pt/nil
                         false ::pt/false
                         val-to-bind))
        keyval-pairs (if in-macro?
                       [:impl-scope (update impl-scope bind-id #(conj % val-to-bind))]
                       [:source-scope (update source-scope bind-id #(conj % val-to-bind))])]
    (model/default-update ctx (if (not= :call-stack bind-from)
                                keyval-pairs
                                (conj keyval-pairs :call-stack (stack/frame-pop call-stack))))))

(defn process-unbind-name
  [{:keys [source-scope impl-scope]
    [{:keys [bind-id in-macro?]} & _] :commands
    :as ctx}]
  (let [keyvals (if in-macro?
                  [:impl-scope (update impl-scope bind-id pop)]
                  [:source-scope (update source-scope bind-id pop)])]
    (model/default-update ctx keyvals)))

;; ------------------------------------------------------------------------------------------------
;; Processors: Flow Control Handlers
;; ------------------------------------------------------------------------------------------------

(defn keyvals-for-exec
  [{:keys [command-history]
    [cmd & cmds] :commands
    :as _ctx}]
  [:commands cmds
   :command-history (conj command-history cmd)])

(defn keyvals-for-skip
  [{:keys [command-history]
    [cmd & cmds] :commands
    :as _ctx}]
  [:commands (drop (:count cmd) cmds)
   :command-history (into (conj command-history cmd)
                          (take (:count cmd) cmds))])

(defn process-exec-when
  [{:keys [state]
    [cmd & _] :commands
    :as ctx}]
  (let [keyvals (if (get state (:state-id cmd))
                  (keyvals-for-exec ctx)
                  (keyvals-for-skip ctx))]
    (model/default-update ctx false keyvals)))

(defn process-skip-when
  [{:keys [state]
    [cmd & _] :commands
    :as ctx}]
  (let [keyvals (if (get state (:state-id cmd))
                  (keyvals-for-skip ctx)
                  (keyvals-for-exec ctx))]
    (model/default-update ctx false keyvals)))

(defn process-replay-commands
  [{:keys [command-history]
    [{:keys [idx] :as cmd} & cmds] :commands
    :as ctx}]
  (let [cmds-to-replay (take-while (fn [old-cmd]
                                     (not (and (= :recur-target (:action old-cmd))
                                               (= idx (:idx old-cmd)))))
                                   command-history)
        kvs [:commands (concat (reverse cmds-to-replay) [cmd] cmds)
             :command-history (drop (count cmds-to-replay) command-history)]]
    (model/default-update ctx false kvs)))

;; ------------------------------------------------------------------------------------------------
;; Processors: Try/Catch/Finally Handlers
;; ------------------------------------------------------------------------------------------------

(defn find-catch
  [e catches]
  (->> catches
       (filter (fn [{:keys [ex-class]}] (instance? ex-class e)))
       (first)
       :commands))

(defn process-setup-try
  [{:keys [try-handlers]
    [cmd] :commands
    :as ctx}]
  (-> ctx
      model/next-command
      (assoc :try-handlers
             (conj try-handlers
                   (merge (select-keys ctx [:fn-idx])
                          (select-keys cmd [:id :catches :finally]))))))

(defn process-cleanup-try
  [{:keys [is-throwing? throwing-ex try-handlers state]
    [_ & cmds] :commands
    :as ctx}]
  (let [{:keys [catches] finally-commands :finally} (peek try-handlers)
        catch-commands (find-catch throwing-ex catches)
        still-throwing? (boolean (when is-throwing? (not catch-commands)))
        ex-caught? (boolean (and is-throwing? (not still-throwing?)))]
    (-> ctx
        (assoc :is-throwing? still-throwing?
               :try-handlers (pop try-handlers)
               :throwing-ex (when still-throwing? throwing-ex))
        (model/default-update [:commands (concat catch-commands finally-commands cmds)
                               :state    (if-not ex-caught?
                                           state
                                           ;; note: wanted to wrap the ex in a val->unhandled
                                           (assoc state :caught-ex throwing-ex))]))))

(defn process-finally
  [{:keys [fn-idx commands] :as ctx}]
  (case (:action (first commands))
    ;; todo: the fn-stack is still leaking into the context, clean it up
    :begin-finally (update-in (model/next-command ctx) [:fn-stack fn-idx :finally-depth] inc)
    :end-finally   (update-in (model/next-command ctx) [:fn-stack fn-idx :finally-depth] dec)))

;; ------------------------------------------------------------------------------------------------
;; Processors: Misc
;; ------------------------------------------------------------------------------------------------

(defn process-begin-form
  [{:keys [form-depth] :as ctx}]
  (model/default-update ctx [:form-depth (inc form-depth)]))

(defn process-end-form
  [{:keys [form-depth] :as ctx}]
  (model/default-update ctx [:form-depth (dec form-depth)]))

(defn process-assoc-state
  [{:keys [fn-idx] :as ctx}]
  (let [kvs (:kvs (first (:commands ctx)))]
    (-> ctx
        model/next-command
        (update-in [:fn-stack fn-idx :state]
                   ;; NOTE: not sure about merging state on top of the kvs
                   ;; seems counter-intuitive, but probably fine
                   (partial merge kvs)))))

(defn process-set-context
  [ctx]
  (let [props (:props (first (:commands ctx)))
        ctx*  (apply assoc ctx (apply concat (seq props)))]
    (model/next-command ctx*)))

(defn process-test-hook
  [ctx]
  (let [f (:hook-fn (first (:commands ctx)))]
    (f ctx)))

(comment
  "Example use case for test hook: "
  (let [f (fn [{:keys [fn-idx] :as ctx}]
            (-> ctx
                model/next-command
                (assoc-in [:fn-stack fn-idx :is-throwing?] true)))
        test-hook {:action :test-hook :hook-fn f}
        cmds (ptg/create-commands '(+ 1 1))
        cmds* (concat (butlast cmds)
                      [test-hook (last cmds)])]
    (execute cmds*)))

;; ------------------------------------------------------------------------------------------------
;; Processors: Handler Mappings
;; ------------------------------------------------------------------------------------------------

(defn create-command-handlers
  []
  (middleware/inject-handlers
   {:begin-form       process-begin-form
    :end-form         process-end-form
    :stack-push-frame process-stack-push-frame
    :stack-pop-frame  process-stack-pop-frame
    :create-fn        process-create-fn
    :invoke-fn        process-invoke-fn
    :invoke-def       process-invoke-def
    :invoke-throw     process-invoke-throw
    :invoke-do        process-invoke-do
    :scalar           process-scalar
    :capture-state    process-capture-state
    :bind-name        process-bind-name
    :unbind-name      process-unbind-name
    :exec-when        process-exec-when
    :skip-when        process-skip-when
    :replay-commands  process-replay-commands
    :setup-try        process-setup-try
    :cleanup-try      process-cleanup-try
    :begin-finally    process-finally
    :end-finally      process-finally
    ;; --------------------------------------------------------------------------
    :intern-var       model/process-no-op ;; TODO: fix?
    :recur-target     model/process-no-op ;; TODO: fix?
    ;; --------------------------------------------------------------------------
    :assoc-state      process-assoc-state
    :set-context      process-set-context
    :test-hook        process-test-hook
    :not-implemented  process-scalar}))

;; ------------------------------------------------------------------------------------------------
;; Processors: Command Execution
;; ------------------------------------------------------------------------------------------------

(def ^:dynamic *enable-verbose-logging* false)

(defn debug-log
  [msg]
  (when *enable-verbose-logging*
    (println msg)))

;; also consider wrapping exceptions we know should be thrown inside
;; ex-info's with attached metadata, then wrap the entire interpreter
;; process with something like wrap-uncaught-ex but forward any wrapped
;; exception accordingly since they don't count as 'interpreter errors'
;; (maybe make a deftype for internal interpreter errors/exceptions)
(defn execute-ctx
  ([context]
   (execute-ctx context nil))
  ([context stop-idx]
   (let [command-handlers (create-command-handlers)
         stop-early? (if (and (int? stop-idx)
                              (or (zero? stop-idx) (pos-int? stop-idx)))
                       (partial <= stop-idx)
                       (constantly false))]
     (loop [{:keys [cmd-counter fn-idx] :as ctx} context]
       (let [{:keys [commands] :as _fctx} (get-in ctx [:fn-stack fn-idx])
             ;; _ (println (pr-str (first command-history)))
             idx (inc @cmd-counter)
             action (:action (first commands))
             h (get command-handlers action)]
         ;; (println "Command Loop [" idx "] " action " " (pr-str (stack-find ctx)))
         (cond
           ;; todo: when you support drilling down into function calls, fix
           ;; this to pop fns off the fn-stack and only throw when none remain
           ;; update: this should be done but leaving comment here until more tests
           ;; are added
           (stop-early? idx) (do (debug-log "Execute Pivot = Stop Early")
                                 (assoc ctx :stop-early? true))
           h                 (do (debug-log "Execute Pivot = Handler")
                                 (recur (h ctx)))
           (> fn-idx 0)      (do (debug-log "Execute Pivot = Pop Stack")
                                 (recur (fn-stack-pop ctx)))
           :else             (do (debug-log "Execute Pivot = Context")
                                 ctx)))))))

(defn execute-for-defs
  ([commands]
   (execute-for-defs commands nil))
  ([commands stop-idx]
   (let [{:keys [stop-early?] :as ctx} (execute-ctx (model/new-exec-ctx commands) stop-idx)]
     (if stop-early?
       ctx
       (:extracted-defs ctx)))))

(defn execute
  ([commands]
   (execute commands nil))
  ([commands stop-idx]
   (let [{:keys [stop-early?] :as ctx} (execute-ctx (model/new-exec-ctx commands) stop-idx)]
     (if stop-early?
       ctx
       (fn-stack-pop ctx)))))

#_(defn get-fctx
    [context]
    (get-in context [:fn-stack (:fn-idx context)]))

#_(defn execute-seq
    [handlers context]
    (cons context
          (lazy-seq
           (let [fctx (get-fctx context)
                 h (get handlers (:action (first (:commands fctx))))]
             (when h
               (execute-seq handlers (h context)))))))

#_(defn execute-lazy
    [commands]
    (let [command-handlers (create-command-handlers)
          initial-ctx (new-exec-ctx commands)
          seq* (execute-seq command-handlers initial-ctx)
          final-ctx (last seq*)
          {:keys [enable-try-catch-support? throwing-ex is-throwing?]} final-ctx
          call-stack (:call-stack (get-fctx final-ctx))]
      (if (and enable-try-catch-support? is-throwing?)
        ;; SEE ABOVE TODO ITEM
        (throw throwing-ex)
        (stack-peek call-stack))
      seq*))

(defn ctx-seq
  [input]
  (let [command-handlers (create-command-handlers)]
    (cond
      (seq? input)
      (ctx-seq (model/new-exec-ctx input))
      (map? input)
      (cons input (lazy-seq
                   (let [fctx (get-in input [:fn-stack (:fn-idx input)])
                         h (get command-handlers
                                (:action (first (:commands fctx))))]
                     (when h
                       (ctx-seq (h input))))))
      :else
      (throw (IllegalArgumentException. "Input must be seq or map")))))
