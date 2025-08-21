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
            [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang IObj IPending LazySeq ArityException]))

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

;; ----------------------------------------------------------------------------------------

(def ^:dynamic *enable-try-catch-support* true)

(defn new-call-ctx
  ([]
   (new-call-ctx :fn-in-execute nil))
  ([exec-ctx]
   (new-call-ctx :fn-on-stack exec-ctx))
  ([call-type exec-ctx]
   {:call-type call-type
    :exec-ctx (when (= :fn-on-stack call-type)
                (assert (map? exec-ctx) "exec-ctx must be a map")
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

;; ----------------------------------------------------------------------------------------

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

(defn default-update
  ([ctx kvs]
   (default-update ctx true kvs))
  ([ctx consume-command? kvs]
   (cond-> ctx
     consume-command? next-command
     true (update-fn-ctx (apply with-keyvals kvs)))))

(def temp-ctx {::pt/current-ns *ns*})

;; ----------------------------------------------------------------------------------------

(defn container-type [this] (. this containerType))
(defn container-value [this] (. this containerValue))
(defn container-meta [this] (. this containerMeta))

(deftype Container [containerType containerValue containerMeta]
  IObj
  (meta [this]
    (container-meta this))
  (withMeta [this m]
    (new Container (container-type this) (container-value this) m)))

(defn new-container
  ([cv] (new-container nil cv))
  ([ct cv] (new Container ct cv nil)))

(defmethod print-method Container
  [this writer]
  (let [class-str (.getName (class (container-value this)))]
    (.write writer (case (container-type this)
                     :unrealized (str "#unrealized[" class-str "]")
                     :unhandled  (str "#unhandled[" class-str "]")
                     (str "#container[" (container-value this) "]")))))

(defn val->unrealized [v] (new-container :unrealized v))
(defn val->unhandled [v] (new-container :unhandled v))

(defn realized-val?
  [val*]
  (if (and (instance? Container val*)
           (= :unrealized (container-type val*)))
    false
    (if-not (instance? IPending val*)
      true
      (realized? val*))))

(defn stack-unwrap-val
  [val*]
  (if-not (instance? Container val*)
    val*
    (container-value val*)))

(defn stack-peek
  [call-stack]
  (stack-unwrap-val (peek call-stack)))

(defn stack-take
  [n call-stack]
  (map stack-unwrap-val (reverse (take n call-stack))))

(defn assoc-meta
  "Like 'assoc' but operates on obj's metadata."
  ([obj k v]
   (vary-meta obj assoc k v))
  ([obj k v k* v*]
   (vary-meta obj assoc k v k* v*))
  ([obj k v k* v* & kvs]
   (let [f #(apply assoc (concat [% k v k* v*] kvs))]
     (vary-meta obj f))))

(defn stack-eval-result
  [depth result]
  (let [total-meta (merge {::pt/is-evaled? true :form-depth depth} (meta result))]
    (with-meta
      (cond
        (not (realized-val? result)) (val->unrealized result)
        (instance? Exception result) (val->unhandled result)
        :else (new-container result))
      total-meta)))

(defn stack-filter-stale-args
  [call-stack depth]
  (apply list
         (first call-stack)
         (filter #(> depth (:form-depth (meta %)))
                 (rest call-stack))))

(defn stack-drop-args
  [call-stack arg-count]
  (apply list (drop arg-count call-stack)))

(defn stack-push-result
  [call-stack depth result]
  (conj call-stack (stack-eval-result depth result)))

(defn stack-resolve
  [call-stack depth arg-count result]
  (stack-push-result (stack-drop-args call-stack arg-count) depth result))

;; ----------------------------------------------------------------------------------------

(defn push-fn-stack
  [{:keys [fn-idx fn-stack] :as ctx} commands]
  (assoc ctx :fn-idx (inc fn-idx) :fn-stack (conj fn-stack (new-fn-ctx commands))))

(defn pop-fn-stack*
  [{:keys [fn-idx fn-stack] :as ctx}]
  (assoc ctx :fn-idx (dec fn-idx) :fn-stack (pop fn-stack)))

;; todo: create a proper stack abstraction so this code copied from the middleware is no longer
;; necessary; the issue here is once we pop-fn-stack, we're now on a fn-idx that hasn't been augmented
;; with convenience bindings + other middleware housekeeping, so we have to handle things the verbose way
(defn convey-result
  [{:keys [form-depth fn-idx] :as ctx} result]
  (let [{:keys [call-stack-primary call-stack-finally finally-depth]} (get-in ctx [:fn-stack fn-idx])
        [stack-name stack-to-use] (if (zero? finally-depth)
                                    [:call-stack-primary call-stack-primary]
                                    [:call-stack-finally call-stack-finally])
        new-stack (stack-push-result stack-to-use form-depth result)]
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
              (convey-result return)))))))

;; ----------------------------------------------------------------------------------------

(declare execute)

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

(defn process-no-op
  [ctx]
  (next-command ctx))

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
  [{:keys [form-depth call-stack source-scope impl-scope]
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
        (default-update
         [:call-stack (stack-push-result call-stack form-depth the-fn*)]))))

(defn invoke-opaque-fn
  [{:keys [form-depth throwing-ex is-throwing? is-finally? call-stack]
    [cmd & _] :commands
    :as ctx}
   fn-impl]
  (let [arg-count (:arg-count cmd)
        [ex? result] (try
                       ;; TODO: search local bindings for invokable fns
                       (let [return (apply fn-impl
                                           (map (map-impl-fn temp-ctx)
                                                (stack-take arg-count call-stack)))]
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
         [:call-stack (stack-resolve call-stack form-depth arg-count result)]))))

(defn invoke-interpreted-fn
  [{:keys [call-stack] [cmd & _] :commands :as ctx} fn-impl]
  (let [arg-count (:arg-count cmd)]
    (apply fn-impl
           (new-call-ctx
            (default-update ctx [:call-stack (stack-drop-args call-stack arg-count)]))
           (map (map-impl-fn temp-ctx) (stack-take arg-count call-stack)))))

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
      (default-update [:call-stack (pop call-stack)])))

(defn process-scalar
  [{:keys [form-depth call-stack source-scope impl-scope]
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
    (default-update ctx [:call-stack (stack-push-result call-stack form-depth scalar-value)])))

;; ----------------------------------------------------------------------------------------

(defn process-capture-state
  [{:keys [call-stack state]
    [cmd & _] :commands
    :as ctx}]
  ;; todo: should we pop the call-stack in this scenario?
  (default-update ctx [:state (assoc state (:state-id cmd) (stack-peek call-stack))]))

(defn process-bind-name
  [{:keys [state call-stack source-scope impl-scope]
    [{:keys [bind-id bind-from impl? value state-id]} & _] :commands
    :as ctx}]
  (let [val-to-bind  (case bind-from
                       :command-value value
                       :call-stack (stack-peek call-stack)
                       :state (get state state-id))
        val-to-bind  (if-not (realized-val? val-to-bind)
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
                          (conj keyval-pairs :call-stack (pop call-stack))))))

(defn process-unbind-name
  [{:keys [source-scope impl-scope]
    [{:keys [bind-id impl?]} & _] :commands
    :as ctx}]
  (let [keyvals (if impl?
                  [:impl-scope (update impl-scope bind-id pop)]
                  [:source-scope (update source-scope bind-id pop)])]
    (default-update ctx keyvals)))

;; ----------------------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------------------

(defn wrap-throwing
  [handler]
  (let [allowed-op? #{:unbind-name :cleanup-try :set-context :begin-finally :end-finally}]
    (fn [{:keys [is-throwing? is-finally?] [{:keys [cmd]}] :commands :as ctx}]
      (if (and is-throwing?
               (not is-finally?)
               (not (allowed-op? cmd)))
        (process-no-op ctx)
        (handler ctx)))))

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
  (fn [{:keys [fn-idx] :as ctx}]
    (let [ctx* (handler ctx)
          pre-handler-cmds (get-in ctx [:fn-stack fn-idx :commands])
          post-handler-cmds (get-in ctx* [:fn-stack fn-idx :commands])]
      (if-not (= pre-handler-cmds post-handler-cmds)
        ctx*
        (throw (new AssertionError
                    "Infinite processing error detected: handler did not update commands"))))))

(defn wrap-convenience-mappings
  [handler]
  (fn [{:keys [fn-idx] :as ctx}]
    (let [fctx (get-in ctx [:fn-stack fn-idx])]
      (assert (empty? (set/intersection (into #{} (keys ctx))
                                        (into #{} (keys fctx))))
              "There must be no key overlap between ctx and fctx")
      (apply dissoc
             (handler (merge ctx fctx))
             (keys fctx)))))

(defn wrap-uncaught-ex
  [handler]
  (fn [ctx]
    (try
      (handler ctx)
      (catch Exception e
        (throw (ex-info "Uncaught interpreter exception" {:ctx ctx} e))))))

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

;; ----------------------------------------------------------------------------------------

"TODO: When 'throwing?' -- we only care about:
 ** unbind-name
 ** cleanup-try
 
 Alternate between processing the two so that intermediate (finally)
 forms and catch/re-throws are scoped correctly as you work your way
 back up the fn call stack."

(defn process-begin-form
  [{:keys [form-depth] :as ctx}]
  (default-update ctx [:form-depth (inc form-depth)]))

(defn process-end-form
  [{:keys [form-depth is-throwing? call-stack] :as ctx}]
  (default-update ctx [:form-depth (dec form-depth)
                       :call-stack (if-not is-throwing?
                                     call-stack
                                     (stack-filter-stale-args call-stack (dec form-depth)))]))

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

(defn create-command-handlers
  []
  (inject-command-middleware
   {}
   {:begin-form      process-begin-form
    :end-form        process-end-form
    :create-fn       process-create-fn
    :invoke-fn       process-invoke-fn
    :invoke-throw    process-invoke-throw
    :scalar          process-scalar
    :capture-state   process-capture-state
    :bind-name       process-bind-name
    :unbind-name     process-unbind-name
    :exec-when       process-exec-when
    :skip-when       process-skip-when
    :replay-commands process-replay-commands
    :setup-try       process-setup-try
    :cleanup-try     process-cleanup-try
    :begin-finally   process-finally
    :end-finally     process-finally
    ;; --------------------------------------------------------------------------
    :intern-var      process-no-op ;; TODO: fix
    :recur-target    process-no-op ;; TODO: fix
    ;; --------------------------------------------------------------------------
    :assoc-state     process-assoc-state
    :set-context     process-set-context
    :test-hook       process-test-hook
    :not-implemented process-scalar}))

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

(defn get-fctx
  [context]
  (get-in context [:fn-stack (:fn-idx context)]))

(defn execute-seq
  [handlers context]
  (cons context
        (lazy-seq
         (let [fctx (get-fctx context)
               h (get handlers (:cmd (first (:commands fctx))))]
           (when h
             (execute-seq handlers (h context)))))))

(defn execute-lazy
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
