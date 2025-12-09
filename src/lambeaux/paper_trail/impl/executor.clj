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
            [lambeaux.paper-trail.impl.util :as ptu :refer [assert*]]
            [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang IObj LazySeq ArityException]))

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
  [{:keys [ns-sym fn-idx] :as ctx} fn-meta commands]
  (let [{:keys [ns-caller]} (when (>= fn-idx 0)
                              (get-in ctx [:fn-stack fn-idx]))
        ns-caller* (or ns-caller ns-sym)]
    (-> ctx
        (update :fn-idx inc)
        (update :fn-stack #(conj % (-> (model/new-fn-ctx ns-caller* commands)
                                       (assoc :fn-meta fn-meta)))))))

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
                 (fn [obj-name {:keys [call-type exec-ctx fn-meta]} & args]
                   (let [arg-count (count args)
                         ks (map #(str "arg-" %) (range arg-count))
                         kvs (into {} (map vector ks args))]
                     (if-let [{:keys [commands] :as _metainf} (get arities arg-count)]
                       (let [all-cmds (concat [{:action :assoc-state :kvs kvs}] scope-cmds commands)]
                         (case call-type
                           :fn-in-execute (execute all-cmds)
                           :fn-on-stack   (fn-stack-push exec-ctx fn-meta all-cmds)))
                       (throw (ArityException. arg-count obj-name))))))
        ;; todo: instead of (str the-fn) inject proper location metadata from source
        ;;   the technique will vary depending if the fn is a var or local binding
        the-fn* (with-meta (partial the-fn (pr-str the-fn))
                  {::pt/fn-impl true})]
    (model/default-update ctx [:call-stack (stack/push-val call-stack the-fn*)])))

;; ------------------------------------------------------------------------------------------------
;; Processors: Function Handlers
;; ------------------------------------------------------------------------------------------------

(defn raw-form
  [form]
  (or (::pt/raw-form (meta form)) form))

(defn form->str
  "Convert form data into a safe-to-print string without accidentally realizing any lazy seqs
   or triggering other side effects."
  [form]
  (let [wrapped-vals (map #(if-not (and (instance? LazySeq %)
                                        (not (realized? %)))
                             %
                             (stack/val->unrealized %))
                          form)]
    (pr-str (apply list wrapped-vals))))

(defn invoke-form
  "Given an execution `context`, an input `form`, and a `form-fn`, execute the form and update
   the interpreter's execution state, handling errors appropriately. Execution typically means, 
   but is not limited to, function calls or form evaluation. The `form-fn` is a zero argument fn
   that will be called; its return value will resolve the invocation. The `form` arg is the raw 
   form as data (not text), and in most cases it should hold true that `(= (eval form) (form-fn))`.
   
   Important note regarding the JVM. Typical applications should not `catch Throwable`. Doing so
   means your code may try to recover from inherently unrecoverable errors. If you copy this pattern,
   make sure you know what you're doing."
  [context form form-fn]
  ;; note: the form->str call is ESSENTIAL, otherwise printing triggers early realization of lazy
  ;;   seqs, which does not match Clojure's behavior.
  (println "Invoke form: " (form->str form))
  (let [[ex? result] (try
                       (vector false (form-fn))
                       ;; todo: eventually support Throwable for more accurate results
                       (catch Exception e (vector true e)))
        {:keys [throwing-ex throwing-form is-throwing? is-finally? call-stack]} context]
    (-> context
        (assoc :is-throwing?  (boolean (or is-throwing? ex?))
               :is-finally?   (if ex? false  is-finally?)
               :throwing-ex   (if ex? result throwing-ex)
               :throwing-form (if ex? form   throwing-form))
        (model/default-update [:call-stack (if ex?
                                             call-stack
                                             (stack/push-resolved-val call-stack result))]))))

;; note: do not do any prep on lazy seqs, setting their meta realizes them
;; note: temporary limitation: for now, fns passed as arguments must be invoked as fn-in-execute 
;;   and not as fn-on-stack, because return vals are expected, not returned context
(defn preprocess-arg
  [arg]
  (cond
    ;; note: can simplify this, we only care about fn args for now, nothing else
    (instance? LazySeq arg) arg
    (and (fn? arg) (::pt/fn-impl (meta arg))) (partial arg (model/new-call-ctx))
    :else arg))

(defn invoke-opaque-fn
  "Handle invoking a fn that we will NOT recursively interpret, just call for a return val."
  [context [f & args :as _fn-frame]]
  (let [raw-form* (apply list (raw-form f) (map raw-form args))
        form-fn   (fn []
                    (apply f (map preprocess-arg args)))]
    (invoke-form context raw-form* form-fn)))

(defn invoke-interpreted-fn
  "Handle invoking a fn that we will recursively interpret."
  [context [f & args :as _fn-frame]]
  (apply f
         (-> (model/new-call-ctx context)
             (assoc :fn-meta (meta f)))
         (map preprocess-arg args)))

(defn process-invoke-fn
  "General handling for any fn."
  [{:keys [call-stack] :as ctx}]
  (let [fn-frame (stack/peek-frame call-stack)
        pt-fn? (boolean (::pt/fn-impl (meta (first fn-frame))))]
    (if pt-fn?
      (invoke-interpreted-fn ctx fn-frame)
      (invoke-opaque-fn ctx fn-frame))))

;; todo: this is a stopgap to get a release out the door; as we iterate on the interpreter, this
;;   should go away entirely
(defn process-invoke-eval
  [{:keys [ns-sym ns-caller call-stack] [{:keys [op] :as _cmd} & _] :commands :as ctx}]
  (let [raw-form* (apply list op (stack/peek-frame call-stack))
        form-fn   (fn []
                    (binding [*ns* (the-ns (or ns-caller ns-sym))]
                      (eval raw-form*)))]
    (invoke-form ctx raw-form* form-fn)))

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

(defn wrapped-delegate
  "Refer to the following snippet for why this exists:
   ```
   clj꞉user꞉> (vary-meta list assoc :original-symbol 'list)
   ; Execution error (UnsupportedOperationException) at user/eval53338 (REPL:1135).
   ; null
   ```
   Seems like an edge case concerning `list` specifically, but not an issue for the other colls
   (e.g. `vector`, `hash-map`, `hash-set`, etc).
   ```
   clj꞉user꞉> (class list)
   clojure.lang.PersistentList$Primordial
   ```"
  [f]
  (with-meta (fn [& args]
               (apply f args))
    (meta f)))

(defn preserve-sym
  [resolved form-sym]
  (if-not (instance? IObj resolved)
    resolved
    (let [resolved* (if-not (fn? resolved)
                      resolved
                      (wrapped-delegate resolved))]
      (vary-meta resolved* assoc ::pt/raw-form form-sym))))

;; todo: update symbol resolution to pull from pt's ns-index if deep trace is desired
(defn resolve-if-symbol
  [{:keys [ns-sym source-scope impl-scope]
    [{:keys [form eval?] :as _cmd} & _] :commands
    :as _ctx}]
  (if-not (and (symbol? form) eval?)
    form
    (let [local-lookup #(or (peek (get source-scope %))
                            (peek (get impl-scope %)))]
      (-> local-lookup
          (ptu/ns-resolve-name ns-sym (namespace form) (name form))
          (preserve-sym form)))))

(defn process-scalar
  [{:keys [call-stack] :as ctx}]
  (let [scalar-value (resolve-if-symbol ctx)
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
    :invoke-eval      process-invoke-eval
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
