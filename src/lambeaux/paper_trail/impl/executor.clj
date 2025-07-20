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

;; (defrecord Unrealized [wrapped-value])

(deftype Unrealized [theWrappedVal theValsMetadata]
  ;; IPending (??)
  ;; (isRealized [this])
  IObj
  (meta [this] (. this theValsMetadata))
  (withMeta [this m] (new Unrealized (. this theWrappedVal) m)))

(defmethod print-method Unrealized
  [this writer]
  (let [class-str (.getName (class (. this theWrappedVal)))]
    (.write writer (str "#unrealized[" class-str "]"))))

(defn val->unrealized
  [v]
  (new Unrealized v nil))

(defn unrealized->val
  [u]
  (. u theWrappedVal))

(defn realized-val?
  [val*]
  (if (instance? Unrealized val*)
    false
    (if-not (instance? IPending val*)
      true
      (realized? val*))))

(defn stack-unwrap-val
  [val*]
  (if-not (instance? Unrealized val*)
    val*
    (unrealized->val val*)))

(defn stack-peek
  [call-stack]
  (stack-unwrap-val (peek call-stack)))

(defn stack-take
  [n call-stack]
  (map stack-unwrap-val (take n call-stack)))

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
  [result]
  (cond-> result
    (not (realized-val? result)) (val->unrealized)
    (instance? IObj result)      (assoc-meta ::pt/is-evaled? true)))

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
      :else arg)))

(defn process-no-op
  [ctx]
  (next-command ctx))

(defn process-create-fn
  [{:keys [call-stack]
    [{:keys [arities]} & _] :commands
    :as ctx}]
  (let [the-fn (fn [obj-name & args]
                 (let [arg-count (count args)
                       ks (map #(str "arg-" %) (range arg-count))
                       kvs (into {} (map vector ks args))]
                   (if-let [{:keys [commands] :as _metainf} (get arities arg-count)]
                     (execute (concat [{:cmd :assoc-state :kvs kvs}] commands))
                     (throw (ArityException. arg-count obj-name)))))]
    (-> ctx
        (default-update
         [:call-stack (conj call-stack (stack-eval-result (partial the-fn (str the-fn))))]))))

(defn process-invoke-fn
  [{:keys [enable-try-catch-support? throwing-ex is-throwing? is-finally? call-stack]
    [cmd & _] :commands
    :as ctx}]
  (let [arg-count (:arg-count cmd)
        [ex? result] (try
                       ;; TODO: search local bindings for invokable fns
                       (let [return (apply (ptu/->impl temp-ctx (:op cmd))
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
    (when (and ex? (not enable-try-catch-support?))
      (throw result))
    (-> ctx
        (assoc
         :is-throwing? (boolean (or is-throwing? ex?))
         :is-finally? (if ex? false is-finally?)
         :throwing-ex (if ex? result throwing-ex))
        (default-update
         [:call-stack (conj (apply list (drop arg-count call-stack))
                            (stack-eval-result result))]))))

(defn process-invoke-throw
  [{:keys [enable-try-catch-support? call-stack]
    :as ctx}]
  (when-not enable-try-catch-support?
    (throw (stack-peek call-stack)))
  (-> ctx
      (next-command)
      (assoc
       :is-throwing? true
       :is-finally? false
       :throwing-ex (stack-peek call-stack))))

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
    (default-update ctx [:call-stack (conj call-stack (stack-eval-result scalar-value))])))

;; ----------------------------------------------------------------------------------------

(defn process-capture-state
  [{:keys [call-stack state]
    [cmd & _] :commands
    :as ctx}]
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
    (default-update ctx keyval-pairs)))

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
  [{:keys [is-throwing? throwing-ex try-handlers]
    [_ & cmds] :commands
    :as ctx}]
  (let [{:keys [catches] finally-commands :finally} (peek try-handlers)
        catch-commands (find-catch throwing-ex catches)
        still-throwing? (boolean (when is-throwing? (not catch-commands)))]
    (-> ctx
        (assoc
         :is-throwing? still-throwing?
         :try-handlers (pop try-handlers)
         :throwing-ex (when still-throwing? throwing-ex))
        (default-update [:commands (concat catch-commands finally-commands cmds)]))))

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
          modified-stack (get-in ctx* [:fn-stack fn-idx :call-stack])]
      (-> ctx*
          (dissoc :call-stack)
          (assoc-in [:fn-stack fn-idx stack-name] modified-stack)))))

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
   (wrap-command-middleware false handler))
  ([wrap-throwing? handler]
   (cond-> handler
     wrap-throwing? wrap-throwing
     true           wrap-call-stack
     true           wrap-check-not-infinite
     true           wrap-convenience-mappings
     true           wrap-uncaught-ex)))

(defn inject-command-middleware
  [{:keys [wrap-throwing?] :as _opts} all-handlers]
  (reduce (fn [m k]
            (update m k (partial wrap-command-middleware
                                 wrap-throwing?)))
          all-handlers
          (keys all-handlers)))

;; ----------------------------------------------------------------------------------------

(def ^:dynamic *enable-try-catch-support* true)

"TODO: When 'throwing?' -- we only care about:
 ** unbind-name
 ** cleanup-try
 
 Alternate between processing the two so that intermediate (finally)
 forms and catch/re-throws are scoped correctly as you work your way
 back up the fn call stack."

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
   {:wrap-throwing? *enable-try-catch-support*}
   {:begin-form      process-no-op
    :end-form        process-no-op
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

(defn new-fn-ctx
  [commands]
  {:commands commands
   :command-history (list)
   ;; :call-stack (list) ;; (added with middleware)
   :call-stack-primary (list)
   :call-stack-finally (list)
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
     (loop [{:keys [cmd-counter throwing-ex is-throwing? fn-idx] :as ctx} ctx*]
       (let [{:keys [commands call-stack] :as _fctx} (get-in ctx [:fn-stack fn-idx])
             ;; _ (println (pr-str (first command-history)))
             idx (inc @cmd-counter)
             h (get command-handlers (:cmd (first commands)))]
         (if (stop-early? idx)
           ctx
           (if h
             (recur (h ctx))
             (if is-throwing?
               ;; todo: when you support drilling down into function calls, fix
               ;; this to pop fns off the fn-stack and only throw when none remain
               ;; ---
               ;; also consider wrapping exceptions we know should be thrown inside
               ;; ex-info's with attached metadata, then wrap the entire interpreter
               ;; process with something like wrap-uncaught-ex but forward any wrapped
               ;; exception accordingly since they don't count as 'interpreter errors'
               ;; (maybe make a deftype for internal interpreter errors/exceptions)
               (throw throwing-ex)
               (stack-peek call-stack)))))))))

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
