;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.executor
  (:require [clojure.set :as set]
            [lambeaux.paper-trail.impl.generator :as ptg]
            [lambeaux.paper-trail.impl.util :as ptu]
            [lambeaux.paper-trail.impl.lib :refer [assert*]]
            [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang IObj IPending LazySeq ArityException]))

;; ------------------------------------------------------------------------------------------------
;; Processors: Context Builders
;; ------------------------------------------------------------------------------------------------

(def ^:dynamic *enable-try-catch-support* true)

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
   ;; :call-stack (list) ;; (added with middleware)
   :call-stack-primary (list)
   :call-stack-finally (list)
   :form-depth -1
   :finally-depth 0
   :source-scope {}
   :impl-scope {}
   :state {}})

(defn new-exec-ctx
  [commands]
  {:fn-idx 0
   :cmd-counter (atom -1)
   :fn-stack [(new-fn-ctx commands)]
   :try-handlers (list)
   :is-throwing? false
   :is-finally? false
   :throwing-ex nil
   :enable-try-catch-support? *enable-try-catch-support*})

;; ------------------------------------------------------------------------------------------------
;; Processors: Context Updaters
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

;; ------------------------------------------------------------------------------------------------
;; Processors: Call Stack
;; ------------------------------------------------------------------------------------------------

(defn box-type [this] (. this boxType))
(defn box-value [this] (. this boxValue))
(defn box-meta [this] (. this boxMeta))

(deftype StackBox [boxType boxValue boxMeta]
  IObj
  (meta [this]
    (box-meta this))
  (withMeta [this m]
    (new StackBox (box-type this) (box-value this) m)))

(defn new-box
  ([bv] (new-box nil bv))
  ([bt bv] (new StackBox bt bv nil)))

(defmethod print-method StackBox
  [this writer]
  (let [class-str (.getName (class (box-value this)))]
    (.write writer (case (box-type this)
                     :unrealized (str "#stack.unrealized[" class-str "]")
                     :unhandled  (str "#stack.unhandled[" class-str "]")
                     (str "#stack.val[" (box-value this) "]")))))

(defn val->unrealized [v] (new-box :unrealized v))
(defn val->unhandled [v] (new-box :unhandled v))

(defn is-realized-val?
  [obj]
  (if (and (instance? StackBox obj)
           (= :unrealized (box-type obj)))
    false
    (if-not (instance? IPending obj)
      true
      (realized? obj))))

(defn is-stack-frame?
  [obj]
  (boolean (and (vector? obj) (::pt/stack-frame? (meta obj)))))

(defn stack-frame-push
  [call-stack]
  (conj call-stack (with-meta [] {::pt/stack-frame? true})))

(defn stack-frame-pop
  [call-stack]
  (let [frame? (is-stack-frame? (peek call-stack))]
    (if frame?
      (pop call-stack)
      (throw (IllegalStateException.
              (str "No valid stack frame to pop: " (pr-str call-stack)))))))

(defn stack-val-unwrap
  [obj]
  (if-not (instance? StackBox obj)
    obj
    (box-value obj)))

;; TODO: when it comes to val->unhandled, probably update from 'Exception' to 'Throwable'
(defn stack-val-wrap
  [obj]
  (let [total-meta (merge {::pt/is-evaled? true} (meta obj))]
    (with-meta
      (cond
        (not (is-realized-val? obj)) (val->unrealized obj)
        (instance? Exception obj) (val->unhandled obj)
        :else (new-box obj))
      total-meta)))

(defn stack-peek
  [call-stack]
  (let [frame-or-val (peek call-stack)
        frame? (is-stack-frame? frame-or-val)]
    (if-not frame?
      (stack-val-unwrap frame-or-val)
      (stack-val-unwrap (peek frame-or-val)))))

(defn stack-peek-frame
  [call-stack]
  (let [frame-or-val (peek call-stack)
        frame? (is-stack-frame? frame-or-val)]
    (if frame?
      (mapv stack-val-unwrap frame-or-val)
      (throw (IllegalStateException.
              (str "No valid stack frame to peek: " (pr-str call-stack)))))))

(defn stack-push
  [call-stack obj]
  (let [frame? (is-stack-frame? (peek call-stack))
        result (stack-val-wrap obj)]
    (if-not frame?
      (conj call-stack result)
      (conj (pop call-stack)
            (conj (peek call-stack) result)))))

(defn stack-push-resolve
  ([call-stack]
   (stack-push-resolve call-stack (stack-peek call-stack)))
  ([call-stack obj]
   (let [frame? (is-stack-frame? (peek call-stack))]
     (if frame?
       (stack-push (pop call-stack) obj)
       (throw (IllegalStateException.
               (str "No valid stack frame to resolve: " (pr-str call-stack))))))))

(defn process-stack-push-frame
  [{:keys [call-stack] :as ctx}]
  (default-update ctx [:call-stack (stack-frame-push call-stack)]))

(defn process-stack-pop-frame
  [{:keys [is-throwing? is-finally? call-stack] :as ctx}]
  (if (or is-finally? (not is-throwing?))
    (next-command ctx)
    (default-update ctx [:call-stack (stack-frame-pop call-stack)])))

;; ------------------------------------------------------------------------------------------------
;; Processors: Function Stack
;; ------------------------------------------------------------------------------------------------

(defn push-fn-stack
  [ctx commands]
  (-> ctx
      (update :fn-idx inc)
      (update :fn-stack #(conj % (new-fn-ctx commands)))))

(defn pop-fn-stack*
  [{:keys [fn-idx] :as ctx}]
  (assert* (> fn-idx 0)
           (str "Fn stack cannot be popped unless fn-idx > 0 but fn-idx is "
                fn-idx))
  (-> ctx
      (update :fn-idx dec)
      (update :fn-stack pop)
      (next-command)))

;; todo: create a proper stack abstraction so this code copied from the middleware is no longer
;; necessary; the issue here is once we pop-fn-stack, we're now on a fn-idx that hasn't been augmented
;; with convenience bindings + other middleware housekeeping, so we have to handle things the verbose way
(defn convey-resolved-fn-result
  [{:keys [fn-idx] :as ctx} result]
  (let [{:keys [call-stack-primary call-stack-finally finally-depth]} (get-in ctx [:fn-stack fn-idx])
        [stack-name stack-to-use] (if (zero? finally-depth)
                                    [:call-stack-primary call-stack-primary]
                                    [:call-stack-finally call-stack-finally])
        new-stack (stack-push-resolve stack-to-use result)]
    (default-update
     ctx false [:call-stack new-stack stack-name new-stack])))

;; todo: this call occurs outside command handlers, so we also have no middleware benefits for this fn
(defn pop-fn-stack
  [{:keys [fn-idx throwing-ex is-throwing?] :as ctx}]
  (let [{:keys [call-stack-primary call-stack-finally finally-depth]} (get-in ctx [:fn-stack fn-idx])
        [_ stack-to-use] (if (zero? finally-depth)
                           [:call-stack-primary call-stack-primary]
                           [:call-stack-finally call-stack-finally])]
    (if (zero? fn-idx)
      (if is-throwing?
        (throw throwing-ex)
        (stack-peek stack-to-use))
      (if is-throwing?
        (pop-fn-stack* ctx)
        (let [return (stack-peek stack-to-use)]
          (-> ctx
              (pop-fn-stack*)
              (convey-resolved-fn-result return)))))))

;; ------------------------------------------------------------------------------------------------
;; Processors: Function Objects
;; ------------------------------------------------------------------------------------------------

(declare execute)

(defn copy-scope-cmds
  ([keyvals]
   (copy-scope-cmds false keyvals))
  ([impl? keyvals]
   (map (fn [[k v]]
          {:cmd :bind-name
           :bind-id k
           :bind-from :command-value
           :value (peek v)
           :impl? impl?})
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
                       (let [all-cmds (concat [{:cmd :assoc-state :kvs kvs}] scope-cmds commands)]
                         (case call-type
                           :fn-in-execute (execute all-cmds)
                           :fn-on-stack   (push-fn-stack exec-ctx all-cmds)))
                       (throw (ArityException. arg-count obj-name))))))
        the-fn* (with-meta (partial the-fn (str the-fn))
                  {::pt/fn-impl true})]
    (-> ctx
        (default-update [:call-stack (stack-push call-stack the-fn*)]))))

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
      (and (fn? arg) (::pt/fn-impl (meta arg))) (partial arg (new-call-ctx))
      :else arg)))

(defn invoke-opaque-fn
  [{:keys [throwing-ex is-throwing? is-finally? call-stack] :as ctx}
   fn-impl]
  (let [[ex? result] (try
                       (let [return (apply fn-impl
                                           (map (map-impl-fn temp-ctx)
                                                (stack-peek-frame call-stack)))]
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
        (default-update
         [:call-stack (if ex?
                        call-stack
                        (stack-push-resolve call-stack result))]))))

(defn invoke-interpreted-fn
  [{:keys [call-stack] :as ctx} fn-impl]
  (apply fn-impl
         (new-call-ctx ctx)
         (map (map-impl-fn temp-ctx)
              (stack-peek-frame call-stack))))

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
       :throwing-ex (stack-peek call-stack))
      ;; todo: fix default-update's inability to handle an empty vector of kvs
      (default-update [:call-stack call-stack])))

(defn process-invoke-do
  [{:keys [call-stack] :as ctx}]
  (default-update ctx [:call-stack (stack-push-resolve call-stack)]))

;; ------------------------------------------------------------------------------------------------
;; Processors: Terminal Value Handlers
;; ------------------------------------------------------------------------------------------------

(defn process-scalar
  [{:keys [call-stack source-scope impl-scope]
    [cmd & _] :commands
    :as ctx}]
  (let [scalar-form (:form cmd)
        scalar-value (or (peek (get source-scope scalar-form))
                         (peek (get impl-scope scalar-form))
                         scalar-form)
        scalar-value (case scalar-value
                       ::pt/nil nil
                       ::pt/false false
                       scalar-value)]
    (default-update ctx [:call-stack (stack-push call-stack scalar-value)])))

;; ------------------------------------------------------------------------------------------------
;; Processors: State & Scope Handlers
;; ------------------------------------------------------------------------------------------------

(defn process-capture-state
  [{:keys [call-stack state]
    [cmd & _] :commands
    :as ctx}]
  ;; todo: should we pop the call-stack in this scenario?
  ;; no, we should not (but awaiting test case verification)
  (default-update ctx [:state (assoc state (:state-id cmd) (stack-peek call-stack))]))

(defn process-bind-name
  [{:keys [state call-stack source-scope impl-scope]
    [{:keys [bind-id bind-from impl? value state-id]} & _] :commands
    :as ctx}]
  (let [val-to-bind  (case bind-from
                       :command-value value
                       :call-stack (stack-peek call-stack)
                       :state (get state state-id))
        val-to-bind  (if-not (is-realized-val? val-to-bind)
                       val-to-bind
                       (case val-to-bind
                         nil ::pt/nil
                         false ::pt/false
                         val-to-bind))
        keyval-pairs (if impl?
                       [:impl-scope (update impl-scope bind-id #(conj % val-to-bind))]
                       [:source-scope (update source-scope bind-id #(conj % val-to-bind))])]
    (default-update ctx (if (not= :call-stack bind-from)
                          keyval-pairs
                          (conj keyval-pairs :call-stack (stack-frame-pop call-stack))))))

(defn process-unbind-name
  [{:keys [source-scope impl-scope]
    [{:keys [bind-id impl?]} & _] :commands
    :as ctx}]
  (let [keyvals (if impl?
                  [:impl-scope (update impl-scope bind-id pop)]
                  [:source-scope (update source-scope bind-id pop)])]
    (default-update ctx keyvals)))

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
    (default-update ctx false keyvals)))

(defn process-skip-when
  [{:keys [state]
    [cmd & _] :commands
    :as ctx}]
  (let [keyvals (if (get state (:state-id cmd))
                  (keyvals-for-skip ctx)
                  (keyvals-for-exec ctx))]
    (default-update ctx false keyvals)))

(defn process-replay-commands
  [{:keys [command-history]
    [{:keys [idx] :as cmd} & cmds] :commands
    :as ctx}]
  (let [cmds-to-replay (take-while (fn [old-cmd]
                                     (not (and (= :recur-target (:cmd old-cmd))
                                               (= idx (:idx old-cmd)))))
                                   command-history)]
    (default-update ctx false
                    [:commands (concat (reverse cmds-to-replay) [cmd] cmds)
                     :command-history (drop (count cmds-to-replay) command-history)])))

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
      next-command
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
        (default-update [:commands (concat catch-commands finally-commands cmds)
                         :state    (if-not ex-caught?
                                     state
                                     ;; note: wanted to wrap the ex in a val->unhandled
                                     (assoc state :caught-ex throwing-ex))]))))

(defn process-finally
  [{:keys [fn-idx commands] :as ctx}]
  (case (:cmd (first commands))
    ;; todo: the fn-stack is still leaking into the context, clean it up
    :begin-finally (update-in (next-command ctx) [:fn-stack fn-idx :finally-depth] inc)
    :end-finally   (update-in (next-command ctx) [:fn-stack fn-idx :finally-depth] dec)))

;; ------------------------------------------------------------------------------------------------
;; Processors: Middleware
;; ------------------------------------------------------------------------------------------------

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
  (fn [{:keys [is-throwing? is-finally?] [{:keys [cmd]}] :commands :as ctx}]
    (if (and is-throwing?
             (not is-finally?)
             (not (allowed-op-when-throwing? cmd)))
      (process-no-op ctx)
      (handler ctx))))

(defn wrap-call-stack
  [handler]
  (fn [{:keys [fn-idx call-stack-primary call-stack-finally finally-depth] :as ctx}]
    (let [[stack-name stack-to-use] (if (zero? finally-depth)
                                      [:call-stack-primary call-stack-primary]
                                      [:call-stack-finally call-stack-finally])
          ctx* (handler (-> ctx
                            (assoc :call-stack stack-to-use)
                            (assoc-in [:fn-stack fn-idx :call-stack] stack-to-use)))
          ;; todo: fix stack writes for better parity
          ;; modified-stack (:call-stack ctx*)
          ;; -------------------------
          ;; todo: it's possible, if the fn-stack was pushed/popped, that modified-stack is nil
          modified-stack (get-in ctx* [:fn-stack fn-idx :call-stack])
          ctx** (dissoc ctx* :call-stack)]
      (if-not modified-stack
        ctx**
        (assoc-in ctx** [:fn-stack fn-idx stack-name] modified-stack)))))

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
      (let [overlap (set/intersection (into #{} (keys ctx))
                                      (into #{} (keys fctx)))]
        (assert* (empty? overlap)
                 (str "There must be no key overlap between ctx and fctx but found "
                      (pr-str overlap))))
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
  ([_wrap-throwing? handler]
   (cond-> handler
     true wrap-throwing
     true wrap-call-stack
     true wrap-check-not-infinite
     true wrap-convenience-mappings
     true wrap-uncaught-ex)))

(defn inject-command-middleware
  [{:keys [wrap-throwing?] :as _opts} all-handlers]
  (reduce (fn [m k]
            (update m k (partial wrap-command-middleware
                                 wrap-throwing?)))
          all-handlers
          (keys all-handlers)))

;; ------------------------------------------------------------------------------------------------
;; Processors: Misc
;; ------------------------------------------------------------------------------------------------

(defn process-begin-form
  [{:keys [form-depth] :as ctx}]
  (default-update ctx [:form-depth (inc form-depth)]))

(defn process-end-form
  [{:keys [form-depth] :as ctx}]
  (default-update ctx [:form-depth (dec form-depth)]))

(defn process-assoc-state
  [{:keys [fn-idx] :as ctx}]
  (let [kvs (:kvs (first (:commands ctx)))]
    (-> ctx
        next-command
        (update-in [:fn-stack fn-idx :state]
                   ;; NOTE: not sure about merging state on top of the kvs
                   ;; seems counter-intuitive, but probably fine
                   (partial merge kvs)))))

(defn process-set-context
  [ctx]
  (let [props (:props (first (:commands ctx)))
        ctx*  (apply assoc ctx (apply concat (seq props)))]
    (next-command ctx*)))

(defn process-test-hook
  [ctx]
  (let [f (:hook-fn (first (:commands ctx)))]
    (f ctx)))

(comment
  "Example use case for test hook: "
  (let [f (fn [{:keys [fn-idx] :as ctx}]
            (-> ctx
                next-command
                (assoc-in [:fn-stack fn-idx :is-throwing?] true)))
        test-hook {:cmd :test-hook :hook-fn f}
        cmds (ptg/create-commands '(+ 1 1))
        cmds* (concat (butlast cmds)
                      [test-hook (last cmds)])]
    (execute cmds*)))

;; ------------------------------------------------------------------------------------------------
;; Processors: Handler Mappings
;; ------------------------------------------------------------------------------------------------

(defn create-command-handlers
  []
  (inject-command-middleware
   {}
   {:begin-form       process-begin-form
    :end-form         process-end-form
    :stack-push-frame process-stack-push-frame
    :stack-pop-frame  process-stack-pop-frame
    :create-fn        process-create-fn
    :invoke-fn        process-invoke-fn
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
    :intern-var       process-no-op ;; TODO: fix
    :recur-target     process-no-op ;; TODO: fix
    ;; --------------------------------------------------------------------------
    :assoc-state      process-assoc-state
    :set-context      process-set-context
    :test-hook        process-test-hook
    :not-implemented  process-scalar}))

;; ------------------------------------------------------------------------------------------------
;; Processors: Command Execution
;; ------------------------------------------------------------------------------------------------

;; also consider wrapping exceptions we know should be thrown inside
;; ex-info's with attached metadata, then wrap the entire interpreter
;; process with something like wrap-uncaught-ex but forward any wrapped
;; exception accordingly since they don't count as 'interpreter errors'
;; (maybe make a deftype for internal interpreter errors/exceptions)
(defn execute
  ([commands]
   (execute commands nil))
  ([commands stop-idx]
   (let [stop-early? (if (and (int? stop-idx)
                              (or (zero? stop-idx) (pos-int? stop-idx)))
                       (partial = stop-idx)
                       (constantly false))
         command-handlers (create-command-handlers)
         commands* commands
         ctx* (new-exec-ctx commands*)]
     (loop [{:keys [cmd-counter fn-idx] :as ctx} ctx*]
       (let [{:keys [commands] :as _fctx} (get-in ctx [:fn-stack fn-idx])
             ;; _ (println (pr-str (first command-history)))
             idx (inc @cmd-counter)
             h (get command-handlers (:cmd (first commands)))]
         (cond
           ;; todo: when you support drilling down into function calls, fix
           ;; this to pop fns off the fn-stack and only throw when none remain
           (stop-early? idx) ctx
           h                 (recur (h ctx))
           (> fn-idx 0)      (recur (pop-fn-stack ctx))
           :else             (pop-fn-stack ctx)))))))

#_(defn get-fctx
    [context]
    (get-in context [:fn-stack (:fn-idx context)]))

#_(defn execute-seq
    [handlers context]
    (cons context
          (lazy-seq
           (let [fctx (get-fctx context)
                 h (get handlers (:cmd (first (:commands fctx))))]
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
      (ctx-seq (new-exec-ctx input))
      (map? input)
      (cons input (lazy-seq
                   (let [fctx (get-in input [:fn-stack (:fn-idx input)])
                         h (get command-handlers
                                (:cmd (first (:commands fctx))))]
                     (when h
                       (ctx-seq (h input))))))
      :else
      (throw (IllegalArgumentException. "Input must be seq or map")))))
